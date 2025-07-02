library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Configuration
SYMBOL <- "BTCUSDT"
MAX_RETRIES <- 3
PROXY_URLS <- c(
  "https://corsproxy.io/?",
  "https://proxy.cors.sh/",
  "https://api.allorigins.win/raw?url="
)

# Fonction pour initialiser ou charger les données
load_or_init_data <- function(filename) {
  if (file.exists(filename)) {
    tryCatch({
      df <- read_csv(filename, show_col_types = FALSE)
      if (nrow(df) == 0) return(data.frame())
      return(df)
    }, error = function(e) {
      message("Erreur lecture ", filename, ": ", e$message)
      return(data.frame())
    })
  } else {
    message("Création du fichier ", filename)
    write_csv(data.frame(), filename)
    return(data.frame())
  }
}

# Fonction de récupération des données avec gestion robuste
fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  base_url <- "https://api.binance.com/api/v3/aggTrades"
  
  for (proxy in PROXY_URLS) {
    tryCatch({
      full_url <- paste0(proxy, URLencode(base_url))
      
      params <- list(
        symbol = symbol,
        startTime = as.numeric(as.POSIXct(start_time, tz = "UTC")) * 1000,
        endTime = as.numeric(as.POSIXct(end_time, tz = "UTC")) * 1000,
        limit = limit
      )
      
      response <- GET(full_url, query = params, timeout(10))
      
      if (status_code(response) == 200) {
        content <- content(response, "text", encoding = "UTF-8")
        
        # Gestion spécifique pour corsproxy.io
        if (grepl("corsproxy", proxy) && grepl("text/html", response$headers$`content-type`)) {
          stop("Proxy retourne une page HTML au lieu de JSON")
        }
        
        data <- fromJSON(content)
        
        # Vérification de la structure des données
        if (is.data.frame(data) && nrow(data) > 0) {
          return(data)
        }
      }
    }, error = function(e) {
      message("Essai proxy [", proxy, "] échoué: ", e$message)
    })
  }
  
  message("Tous les proxies ont échoué")
  return(data.frame())
}

# Fonction principale avec journalisation détaillée
main <- function() {
  # Journalisation
  message("==== Début exécution ", Sys.time(), " ====")
  
  # Charger les données existantes
  df_normal <- load_or_init_data("df_normal.csv")
  df_whale <- load_or_init_data("df_whale.csv")
  q_whale <- load_or_init_data("q_whale.csv")
  
  # Récupération des données
  new_trades <- tryCatch({
    fetch_trades(SYMBOL, Sys.Date() - 1, Sys.Date())
  }, error = function(e) {
    message("Échec récupération données: ", e$message)
    return(data.frame())
  })
  
  # Traitement des données si disponibles
  if (is.data.frame(new_trades) && nrow(new_trades) > 0) {
    message(nrow(new_trades), " nouvelles transactions trouvées")
    
    processed <- tryCatch({
      new_trades %>%
        mutate(
          transaction_type = ifelse(isTRUE(m), "Vente", "Achat"),
          date = as.POSIXct(T/1000, origin = "1970-01-01", tz = "UTC"),
          day = as.Date(date),
          q = as.numeric(q),
          p = as.numeric(p),
          total = q * p
        ) %>%
        filter(!is.na(q) & !is.na(p))
    }, error = function(e) {
      message("Erreur traitement: ", e$message)
      return(data.frame())
    })
    
    # Mise à jour si données valides
    if (nrow(processed) > 0) {
      # df_normal
      daily_summary <- tryCatch({
        processed %>%
          group_by(day, transaction_type) %>%
          summarise(sum = sum(total, na.rm = TRUE), .groups = "drop")
      }, error = function(e) {
        message("Erreur résumé: ", e$message)
        return(data.frame())
      })
      
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
      
      # df_whale
      if (any(processed$q >= 10, na.rm = TRUE)) {
        whale_summary <- tryCatch({
          processed %>%
            filter(q >= 10) %>%
            group_by(day, transaction_type) %>%
            summarise(sum = sum(total, na.rm = TRUE), .groups = "drop")
        }, error = function(e) {
          message("Erreur résumé whales: ", e$message)
          return(data.frame())
        })
        
        if (nrow(whale_summary) > 0) {
          df_whale <- whale_summary %>%
            pivot_wider(
              names_from = transaction_type,
              values_from = sum,
              values_fill = 0
            ) %>%
            mutate(difference_whales = Achat - Vente) %>%
            bind_rows(df_whale) %>%
            distinct(day, .keep_all = TRUE)
        }
      }
      
      # q_whale
      if (any(processed$q >= 10, na.rm = TRUE)) {
        q_whale <- processed %>%
          filter(q >= 10) %>%
          select(day, date, q, p, transaction_type) %>%
          bind_rows(q_whale)
      }
    }
  } else {
    message("Aucune nouvelle donnée disponible")
  }
  
  # Sauvegarde sécurisée
  tryCatch({
    write_csv(df_normal, "df_normal.csv")
    write_csv(df_whale, "df_whale.csv")
    write_csv(q_whale, "q_whale.csv")
    message("Fichiers CSV sauvegardés")
  }, error = function(e) {
    message("Erreur sauvegarde: ", e$message)
  })
  
  # Git push pour GitHub Actions
  if (Sys.getenv("CI") == "true") {
    tryCatch({
      system("git config --global user.name 'GitHub Actions'")
      system("git config --global user.email 'actions@github.com'")
      
      # Vérifier les changements avant commit
      system("git add *.csv")
      changes <- system("git status --porcelain", intern = TRUE)
      
      if (length(changes) > 0) {
        system("git commit -m 'Auto-update data'")
        system("git push")
        message("Push vers GitHub réussi")
      } else {
        message("Aucun changement détecté")
      }
    }, error = function(e) {
      message("Erreur Git: ", e$message)
    })
  }
  
  message("==== Fin exécution ", Sys.time(), " ====")
}

# Point d'entrée principal
tryCatch({
  main()
}, error = function(e) {
  message("ERREUR CRITIQUE: ", e$message)
  write_lines(paste(Sys.time(), e$message), "error_log.txt")
})
