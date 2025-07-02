# binance.R - Script complet avec gestion robuste des erreurs
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Fonction de récupération des trades avec gestion améliorée
fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  endpoints <- c(
    "https://api.binance.com",
    "https://api1.binance.com",
    "https://api2.binance.com",
    "https://api3.binance.com"
  )
  
  for(endpoint in endpoints) {
    tryCatch({
      url <- paste0(endpoint, "/api/v3/aggTrades")
      
      params <- list(
        symbol = symbol,
        startTime = as.numeric(as.POSIXct(start_time, tz = "UTC")) * 1000,
        endTime = as.numeric(as.POSIXct(end_time, tz = "UTC")) * 1000,
        limit = limit
      )
      
      response <- GET(url, query = params, timeout(10))
      
      if(status_code(response) == 200) {
        data <- fromJSON(content(response, "text"))
        
        # Vérification de la structure des données
        if(is.data.frame(data) && nrow(data) > 0) {
          return(data)
        }
      }
    }, error = function(e) {
      message("Essai avec ", endpoint, " échoué: ", e$message)
    })
  }
  
  message("Tous les endpoints ont échoué")
  return(data.frame())  # Retourne un dataframe vide au lieu de NULL
}

# Chargement des données existantes avec valeurs par défaut
load_or_init <- function(file) {
  if (file.exists(file)) {
    tryCatch({
      df <- read_csv(file, show_col_types = FALSE)
      if(nrow(df) == 0) return(data.frame())
      return(df)
    }, error = function(e) {
      message("Erreur lecture ", file, ": ", e$message)
      return(data.frame())
    })
  } else {
    return(data.frame())
  }
}

# Fonction principale avec gestion d'erreur améliorée
main <- function() {
  message("\nDébut de l'exécution à ", Sys.time())
  
  # Initialisation avec des dataframes vides
  df_normal <- data.frame()
  df_whale <- data.frame()
  q_whale <- data.frame()
  
  tryCatch({
    # Chargement des données existantes
    df_normal <- load_or_init("df_normal.csv")
    df_whale <- load_or_init("df_whale.csv")
    q_whale <- load_or_init("q_whale.csv")
    
    # Récupération des nouvelles données
    new_trades <- fetch_trades(
      symbol = "BTCUSDT",
      start_time = Sys.Date() - 1,
      end_time = Sys.Date()
    )
    
    # Vérification des données reçues
    if(!is.null(new_trades) && is.data.frame(new_trades) && nrow(new_trades) > 0) {
      message(nrow(new_trades), " nouvelles transactions récupérées")
      
      # Traitement des données
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
      
      # Mise à jour df_normal
      if(nrow(processed) > 0) {
        daily_summary <- processed %>%
          group_by(day, transaction_type) %>%
          summarise(sum = sum(total, na.rm = TRUE), .groups = "drop")
        
        if(nrow(daily_summary) > 0) {
          df_normal_update <- daily_summary %>%
            pivot_wider(names_from = transaction_type, 
                        values_from = sum, 
                        values_fill = 0) %>%
            mutate(difference = Achat - Vente)
          
          df_normal <- bind_rows(df_normal, df_normal_update) %>%
            distinct(day, .keep_all = TRUE)
        }
        
        # Mise à jour df_whale
        daily_summary_whales <- processed %>%
          filter(q >= 10) %>%
          group_by(day, transaction_type) %>%
          summarise(sum = sum(total, na.rm = TRUE), .groups = "drop")
        
        if(nrow(daily_summary_whales) > 0) {
          df_whale_update <- daily_summary_whales %>%
            pivot_wider(names_from = transaction_type, 
                        values_from = sum, 
                        values_fill = 0) %>%
            mutate(difference_whales = Achat - Vente)
          
          df_whale <- bind_rows(df_whale, df_whale_update) %>%
            distinct(day, .keep_all = TRUE)
        }
        
        # Mise à jour q_whale
        q_whale_update <- processed %>%
          filter(q >= 10) %>%
          select(-any_of(c("m", "f", "a", "F", "T")))
        
        q_whale <- bind_rows(q_whale, q_whale_update)
      }
    } else {
      message("Aucune nouvelle donnée aujourd'hui")
    }
    
    # Sauvegarde
    write_csv(df_normal, "df_normal.csv")
    write_csv(df_whale, "df_whale.csv")
    write_csv(q_whale, "q_whale.csv")
    message("Fichiers CSV mis à jour")
    
    # Configuration Git pour GitHub Actions
    if (Sys.getenv("CI") == "true") {
      message("Configuration Git pour GitHub Actions...")
      system("git config --global user.name 'GitHub Actions'")
      system("git config --global user.email 'actions@github.com'")
      system("git add *.csv")
      system("git commit -m 'Auto-update data' || echo 'Aucun changement'")
      system("git push")
      message("Push vers GitHub réussi!")
    }
    
  }, error = function(e) {
    message("ERREUR CRITIQUE: ", e$message)
    # Journalisation de l'erreur complète
    write_lines(paste(Sys.time(), e$message), "error_log.txt")
    write_lines(capture.output(traceback()), "error_log.txt", append = TRUE)
  })
  
  message("Fin de l'exécution à ", Sys.time())
}

# Exécution de la fonction principale
main()
