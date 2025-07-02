library(httr)
library(jsonlite)
library(dplyr)
library(readr)
SYMBOL <- "BTCUSDT"
fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  url <- "https://api.binance.com/api/v3/aggTrades"
  params <- list(
    symbol    = symbol,
    startTime = as.numeric(as.POSIXct(start_time, tz="UTC")) * 1000,
    endTime   = as.numeric(as.POSIXct(end_time,   tz="UTC")) * 1000,
    limit     = limit
  )
  res <- GET(url, query = params)
  if (status_code(res) != 200) stop("Erreur API Binance : ", status_code(res))
  data <- fromJSON(content(res, "text"), simplifyDataFrame = TRUE)
  if (!is.data.frame(data) || nrow(data) == 0) stop("Aucun trade récupéré.")
  return(data)
}

# Fonction principale
main <- function() {
  message("==== Début exécution ", Sys.time(), " ====")
  init_files()
  
  # Charger les données
  df_normal <- read_csv("df_normal.csv", show_col_types = FALSE)
  df_whale <- read_csv("df_whale.csv", show_col_types = FALSE)
  q_whale <- read_csv("q_whale.csv", show_col_types = FALSE)
  
  tryCatch({
    # Récupérer les données
    new_trades <- fetch_trades(SYMBOL, Sys.Date() - 1, Sys.Date())
    
    if (nrow(new_trades) > 0) {
      # Traitement des données
      processed <- new_trades %>%
        mutate(
          transaction_type = ifelse(m, "Vente", "Achat"),
          date = as.POSIXct(T/1000, origin = "1970-01-01"),
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
          summarise(sum = sum(total), .groups = "drop")
        
        df_normal <- daily_summary %>%
          pivot_wider(names_from = transaction_type, values_from = sum, values_fill = 0) %>%
          mutate(difference = Achat - Vente) %>%
          bind_rows(df_normal) %>%
          distinct(day, .keep_all = TRUE)
        
        # df_whale
        df_whale <- processed %>%
          filter(q >= 10) %>%
          group_by(day, transaction_type) %>%
          summarise(sum = sum(total), .groups = "drop") %>%
          pivot_wider(names_from = transaction_type, values_from = sum, values_fill = 0) %>%
          mutate(difference_whales = Achat - Vente) %>%
          bind_rows(df_whale) %>%
          distinct(day, .keep_all = TRUE)
        
        # q_whale
        q_whale <- processed %>%
          filter(q >= 10) %>%
          select(day, date, q, p, transaction_type) %>%
          bind_rows(q_whale)
      }
    }
  }, error = function(e) {
    message("Aucune nouvelle donnée: ", e$message)
  })
  
  # Sauvegarde
  write_csv(df_normal, "df_normal.csv")
  write_csv(df_whale, "df_whale.csv")
  write_csv(q_whale, "q_whale.csv")
  message("Fichiers CSV sauvegardés")
  
  # Git push
  if (Sys.getenv("CI") == "true") {
    system("git config --global user.name 'GitHub Actions'")
    system("git config --global user.email 'actions@github.com'")
    system("git add *.csv")
    
    # Vérifier les changements
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

# Exécution
tryCatch({
  main()
}, error = function(e) {
  message("ERREUR: ", e$message)
  writeLines(paste(Sys.time(), e$message), "error_log.txt")
})
