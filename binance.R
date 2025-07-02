library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Configuration
SYMBOL <- "BTCUSDT"
MAX_RETRIES <- 3
DATA_SOURCES <- c(
  "https://api.binance.com",
  "https://api1.binance.com",
  "https://api2.binance.com",
  "https://api3.binance.com"
)

# Fonction pour charger les données
load_or_init <- function(filename) {
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

# Fonction pour récupérer les données via des IPs fiables
get_binance_data <- function(symbol, start_time, end_time, limit = 1000) {
  for (source in DATA_SOURCES) {
    tryCatch({
      url <- paste0(source, "/api/v3/aggTrades")
      
      params <- list(
        symbol = symbol,
        startTime = as.numeric(as.POSIXct(start_time)) * 1000,
        endTime = as.numeric(as.POSIXct(end_time)) * 1000,
        limit = limit
      )
      
      # Contournement DNS en utilisant l'IP directement
      ip_address <- switch(source,
        "https://api.binance.com" = "54.192.143.240",
        "https://api1.binance.com" = "54.192.143.241",
        "https://api2.binance.com" = "54.192.143.242",
        "https://api3.binance.com" = "54.192.143.243"
      )
      
      # Override du DNS
      modified_url <- sub("//[^/]+", paste0("//", ip_address), url)
      
      response <- GET(modified_url,
                     add_headers("Host" = "api.binance.com"),
                     query = params,
                     timeout(10))
      
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text"))
        if (is.data.frame(data) && nrow(data) > 0) return(data)
      }
    }, error = function(e) {
      message("Essai avec ", source, " échoué: ", e$message)
    })
  }
  message("Toutes les sources ont échoué")
  return(data.frame())
}

# Fonction principale
main <- function() {
  message("==== Début exécution ", Sys.time(), " ====")
  
  # Charger les données
  df_normal <- load_or_init("df_normal.csv")
  df_whale <- load_or_init("df_whale.csv")
  q_whale <- load_or_init("q_whale.csv")
  
  # Récupérer les données
  new_trades <- get_binance_data(SYMBOL, Sys.Date() - 1, Sys.Date())
  
  # Traitement des données
  if (nrow(new_trades) > 0) {
    message(nrow(new_trades), " nouvelles transactions trouvées")
    
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
  } else {
    message("Aucune nouvelle donnée disponible")
  }
  
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
    
    # Vérifier les changements avant commit
    changes <- system("git status --porcelain", intern = TRUE)
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

# Exécution sécurisée
tryCatch({
  main()
}, error = function(e) {
  message("ERREUR: ", e$message)
  writeLines(paste(Sys.time(), e$message), "error_log.txt")
})
