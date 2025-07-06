# binance.R
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(tidyr)


# Paramètres
SYMBOL <- "BTCUSDT"

fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  proxy_base <- "https://binance-proxy-fly-patient-frost-171.fly.dev"
  path <- "/api/v3/aggTrades"
  
  # Convertir en UTC et s'assurer que les dates sont correctes
  start_ms <- as.numeric(as.POSIXct(start_time, tz="UTC")) * 1000
  end_ms   <- as.numeric(as.POSIXct(end_time, tz="UTC")) * 1000
  
  # Debug: afficher les timestamps
  cat(sprintf("Période demandée: %s à %s\n", start_time, end_time))
  cat(sprintf("Timestamps: %.0f à %.0f\n", start_ms, end_ms))
  
  # Validation des timestamps
  if (start_ms >= end_ms) {
    stop("La date de début doit être antérieure à la date de fin")
  }
  
  if (end_ms > (as.numeric(Sys.time()) * 1000)) {
    cat("Attention: la date de fin est dans le futur\n")
  }
  
  all_trades <- list()
  current_start <- start_ms
  
  repeat {
    query <- list(
      symbol    = symbol,
      startTime = sprintf("%.0f", current_start),
      endTime   = sprintf("%.0f", end_ms),
      limit     = limit
    )
    
    url <- paste0(proxy_base, path, "?", 
                  paste(names(query), query, sep="=", collapse="&"))
    cat(sprintf("Requête: %s\n", url))
    
    res <- httr::GET(
      url = paste0(proxy_base, path),
      query = query,
      httr::timeout(15)
    )
    
    # Vérifier le statut avant d'extraire le contenu
    if (httr::status_code(res) != 200) {
      error_content <- httr::content(res, "text")
      cat(sprintf("Erreur HTTP %d: %s\n", 
                  httr::status_code(res), 
                  error_content))
      stop(sprintf("Erreur HTTP %d: %s", httr::status_code(res), error_content))
    }
    
    content_text <- httr::content(res, "text")
    trades <- jsonlite::fromJSON(content_text, simplifyDataFrame = TRUE)
    
    if (length(trades) == 0 || nrow(trades) == 0) {
      cat("Aucune donnée reçue, arrêt\n")
      break
    }
    
    all_trades[[length(all_trades) + 1]] <- trades
    cat(sprintf("Récupéré %d trades\n", nrow(trades)))
    
    last_time <- as.numeric(trades$T[nrow(trades)])
    if (last_time >= end_ms || nrow(trades) < limit) break
    
    # Ajouter 1 ms pour éviter doublons
    current_start <- last_time + 1
    Sys.sleep(0.2)  # Évite d'être trop agressif avec l'API
  }
  
  if (length(all_trades) == 0) {
    warning("Aucune donnée récupérée")
    return(data.frame())
  }
  
  return(dplyr::bind_rows(all_trades))
}

# Fonction pour obtenir des dates sûres
get_safe_date_range <- function(days_back = 1) {
  # Utiliser UTC pour éviter les problèmes de fuseau horaire
  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")
  
  # Toujours utiliser des dates complètes (pas aujourd'hui)
  end_date <- as.Date(now_utc) - 1  # Hier
  start_date <- end_date - days_back + 1
  
  # Convertir en timestamps de début et fin de journée
  start_time <- as.POSIXct(paste(start_date, "00:00:00"), tz = "UTC")
  end_time <- as.POSIXct(paste(end_date, "23:59:59"), tz = "UTC")
  
  return(list(start = start_time, end = end_time))
}

main <- function() {
  message("==== Début exécution ", Sys.time(), " ====")
  
  # Utiliser des dates sûres - CORRECTION ICI
  date_range <- get_safe_date_range(days_back = 1)
  cat(sprintf("Récupération des données du %s au %s\n", 
              date_range$start, date_range$end))
  
  # Récupération des données avec la nouvelle fonction
  trades <- fetch_trades(SYMBOL, date_range$start, date_range$end)
  
  if (nrow(trades) == 0) {
    message("Aucune donnée récupérée. Arrêt du script.")
    return()
  }
  
  # Transformation
  trades_clean <- trades %>%
    mutate(
      transaction_type = ifelse(m, "Vente", "Achat"),
      date = as.POSIXct(T/1000, origin = "1970-01-01", tz="UTC"),
      day  = as.Date(date),
      q    = as.numeric(q),
      p    = as.numeric(p),
      total = q * p
    ) %>%
    filter(!is.na(q) & !is.na(p))
  
  cat(sprintf("Données nettoyées: %d trades\n", nrow(trades_clean)))
  
  # df_normal
  df_normal <- trades_clean %>%
    group_by(day, transaction_type) %>%
    summarise(sum = sum(total), .groups = "drop") %>%
    pivot_wider(names_from = transaction_type, values_from = sum, values_fill = 0) %>%
    mutate(difference = Achat - Vente)
  
  # df_whale
  df_whale <- trades_clean %>%
    filter(q >= 10) %>%
    group_by(day, transaction_type) %>%
    summarise(sum = sum(total), .groups = "drop") %>%
    pivot_wider(names_from = transaction_type, values_from = sum, values_fill = 0) %>%
    mutate(
      Achat = ifelse(!"Achat" %in% colnames(.), 0, Achat),
      Vente = ifelse(!"Vente" %in% colnames(.), 0, Vente),
      difference_whales = Achat - Vente
    )
  
  # q_whale
  q_whale <- trades_clean %>%
    filter(q >= 10) %>%
    select(day, date, q, p, transaction_type)
  
  # Fonction de sauvegarde
  save_and_append_csv <- function(new_df, file_path, key_cols) {
    if (file.exists(file_path)) {
      old_df <- read_csv(file_path, show_col_types = FALSE)
      combined_df <- bind_rows(old_df, new_df) %>%
        distinct(across(all_of(key_cols)), .keep_all = TRUE)
    } else {
      combined_df <- new_df
    }
    write_csv(combined_df, file_path)
    cat(sprintf("Sauvegardé: %s (%d lignes)\n", file_path, nrow(combined_df)))
  }
  
  # Sauvegarde cumulée
  save_and_append_csv(df_normal, "df_normal.csv", c("day"))
  save_and_append_csv(df_whale,  "df_whale.csv",  c("day"))
  save_and_append_csv(q_whale,   "q_whale.csv",   c("day", "date", "q", "p"))
  
  # Git operations (optionnel)
  tryCatch({
    system("git config --global user.email 'your-email@example.com'")
    system("git config --global user.name 'Your Name'")
    system("git add *.csv")
    system("git commit -m 'Auto update CSVs'")
    system("git push origin main")
    cat("Git push réussi\n")
  }, error = function(e) {
    cat("Erreur Git:", e$message, "\n")
  })
  
  message("==== Fin exécution ", Sys.time(), " ====")
}

# Lancer - UTILISE MAINTENANT LA FONCTION CORRIGÉE
main()
