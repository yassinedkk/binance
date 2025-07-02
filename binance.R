library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Configuration
SYMBOL <- "BTCUSDT"
PROXY_API_KEY <- Sys.getenv("SCRAPINGBEE_API_KEY")
if (PROXY_API_KEY == "") stop(" Il manque la variable SCRAPINGBEE_API_KEY")

SCRAPINGBEE_URL <- "https://app.scrapingbee.com/api/v1/"

# Fonction pour charger/initialiser les fichiers
init_files <- function() {
  files <- c("df_normal.csv", "df_whale.csv", "q_whale.csv")
  for (file in files) {
    if (!file.exists(file)) {
      write_csv(data.frame(), file)
    }
  }
}

# Fonction de récupération fiable avec proxy professionnel
fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  base_url <- "https://api.binance.com/api/v3/aggTrades"
  
  params <- list(
    "api_key" = PROXY_API_KEY,
    "url" = base_url,
    "params" = URLencode(paste0(
      "symbol=", symbol,
      "&startTime=", as.numeric(as.POSIXct(start_time)) * 1000,
      "&endTime=", as.numeric(as.POSIXct(end_time)) * 1000,
      "&limit=", limit
    ))
  )
  
  response <- GET(SCRAPINGBEE_URL, query = params, timeout(10))
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    if (!is.null(data) && is.data.frame(data)) {
      return(data)
    }
  }
  stop("Échec de la récupération via ScrapingBee")
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
