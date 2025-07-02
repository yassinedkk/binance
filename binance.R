# binance.R - Script complet avec gestion robuste des erreurs
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Configuration ----
SYMBOL <- "BTCUSDT"
MAX_RETRIES <- 3
PROXY_SERVICE <- "https://api.scrapingbee.com/v1/"
BINANCE_API <- "https://api.binance.com/api/v3/aggTrades"

# Fonction améliorée de récupération des trades ----
fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  api_key <- Sys.getenv("51JFEA1XP40U2ORZKLDPKRT41V19CVWOA7R5F6JOU25I84U538DXEUMLY91K21K2JVYU8FD36TJCYXKJ")
  if (is.null(api_key) stop("Clé API ScrapingBee manquante")
  
  for (attempt in 1:MAX_RETRIES) {
    tryCatch({
      # Construction de l'URL avec proxy
      url <- paste0(
        PROXY_SERVICE,
        "?url=", URLencode(BINANCE_API),
        "&api_key=", api_key,
        "&render_js=false"
      )
      
      # Paramètres de la requête
      params <- list(
        symbol = symbol,
        startTime = as.numeric(as.POSIXct(start_time, tz = "UTC")) * 1000,
        endTime = as.numeric(as.POSIXct(end_time, tz = "UTC")) * 1000,
        limit = limit
      )
      
      # Envoi de la requête
      response <- GET(url, query = params, timeout(10))
      
      # Vérification de la réponse
      if (status_code(response) == 200) {
        content <- content(response, "text", encoding = "UTF-8")
        data <- fromJSON(content)
        
        if (!is.null(data) && is.data.frame(data) && nrow(data) > 0) {
          return(data)
        }
      }
    }, error = function(e) {
      message("Tentative ", attempt, "/", MAX_RETRIES, " échouée: ", e$message)
      Sys.sleep(2)  # Attente avant nouvelle tentative
    })
  }
  
  message("Échec après ", MAX_RETRIES, " tentatives")
  return(data.frame())  # Retourne un dataframe vide
}

# Fonction de chargement des données ----
load_or_init <- function(file) {
  if (file.exists(file)) {
    tryCatch({
      df <- read_csv(file, show_col_types = FALSE)
      if (nrow(df) == 0) return(data.frame())
      return(df)
    }, error = function(e) {
      message("Erreur lecture ", file, ": ", e$message)
      return(data.frame())
    })
  } else {
    return(data.frame())
  }
}

# Fonction principale ----
main <- function() {
  message("\n==== Début exécution ", Sys.time(), " ====")
  
  # Initialisation des données
  df_normal <- load_or_init("df_normal.csv")
  df_whale <- load_or_init("df_whale.csv")
  q_whale <- load_or_init("q_whale.csv")
  
  # Récupération des nouvelles données
  new_trades <- fetch_trades(
    symbol = SYMBOL,
    start_time = Sys.Date() - 1,
    end_time = Sys.Date()
  )
  
  # Traitement des données
  if (nrow(new_trades) > 0) {
    message(nrow(new_trades), " nouvelles transactions trouvées")
    
    processed <- new_trades %>%
      mutate(
        transaction_type = ifelse(isTRUE(m), "Vente", "Achat"),
        date = as.POSIXct(T/1000, origin = "1970-01-01", tz = "UTC"),
        day = as.Date(date),
        q = as.numeric(q),
        p = as.numeric(p),
        total = q * p
      ) %>%
      filter(!is.na(q) & !is.na(p))
    
    # Mise à jour des données
    if (nrow(processed) > 0) {
      # df_normal
      daily_summary <- processed %>%
        group_by(day, transaction_type) %>%
        summarise(sum = sum(total, na.rm = TRUE), .groups = "drop")
      
      if (nrow(daily_summary) > 0) {
        df_normal <- daily_summary %>%
          pivot_wider(
            names_from = transaction_type,
            values_from = sum,
            values_fill = 0
          ) %>%
          mutate(difference = Achat - Vente) %>%
          bind_rows(df_normal) %>%
          distinct(day, .keep_all = TRUE)
      }
      
      # df_whale (transactions > 10 BTC)
      if (any(processed$q >= 10)) {
        df_whale <- processed %>%
          filter(q >= 10) %>%
          group_by(day, transaction_type) %>%
          summarise(sum = sum(total, na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(
            names_from = transaction_type,
            values_from = sum,
            values_fill = 0
          ) %>%
          mutate(difference_whales = Achat - Vente) %>%
          bind_rows(df_whale) %>%
          distinct(day, .keep_all = TRUE)
      }
      
      # q_whale (détail des grosses transactions)
      q_whale <- processed %>%
        filter(q >= 10) %>%
        select(day, date, q, p, transaction_type) %>%
        bind_rows(q_whale)
    }
  } else {
    message("Aucune nouvelle donnée disponible")
  }
  
  # Sauvegarde
  write_csv(df_normal, "df_normal.csv")
  write_csv(df_whale, "df_whale.csv")
  write_csv(q_whale, "q_whale.csv")
  message("Fichiers CSV sauvegardés")
  
  # Push vers GitHub (uniquement si changement)
  if (Sys.getenv("CI") == "true") {
    system("git config --global user.name 'GitHub Actions'")
    system("git config --global user.email 'actions@github.com'")
    
    # Vérification des changements
    system("git add *.csv")
    changes <- system("git diff --cached --name-only", intern = TRUE)
    
    if (length(changes) > 0) {
      system("git commit -m 'Auto-update data'")
      system("git push")
      message("Push vers GitHub réussi")
    } else {
      message("Aucun changement détecté")
    }
  }
  
  message("==== Fin exécution ", Sys.time(), " ====")
}

# Exécution ----
tryCatch({
  main()
}, error = function(e) {
  message("ERREUR: ", e$message)
  # Journalisation détaillée
  write_lines(
    paste(Sys.time(), "Erreur:", e$message, "\n", paste(capture.output(traceback()), collapse = "\n")),
    "error_log.txt"
  )
  quit(status = 1)
})
