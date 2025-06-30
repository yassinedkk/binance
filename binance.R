# binance.R - Script complet pour Render.com + GitHub

# 1. Installation automatique des packages
required_packages <- c("httr", "jsonlite", "dplyr", "readr", "tidyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org")

# 2. Chargement des librairies
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(tidyr)

# 3. Fonction pour contourner les restrictions Binance (avec proxy)
fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  base_url <- "https://api.binance.com/api/v3/aggTrades"
  proxy_url <- "https://cors-anywhere.herokuapp.com/"  # Proxy gratuit
  url <- paste0(proxy_url, base_url)
  
  trades <- list()
  
  repeat {
    params <- list(
      symbol = symbol,
      startTime = as.numeric(as.POSIXct(start_time, tz = "UTC")) * 1000,
      endTime = as.numeric(as.POSIXct(end_time, tz = "UTC")) * 1000,
      limit = limit
    )
    
    response <- tryCatch(
      GET(url, query = params, add_headers("X-Requested-With" = "XMLHttpRequest")),
      error = function(e) stop("Erreur de connexion : ", e$message)
    )
    
    if (status_code(response) != 200) {
      stop("Erreur API : ", content(response, "text"))
    }
    
    data <- fromJSON(content(response, "text"))
    if (is.null(data) || nrow(data) == 0) break
    
    trades <- append(trades, list(data))
    start_time <- as.POSIXct(max(data$T)/1000, origin = "1970-01-01", tz = "UTC") + 1
    if (nrow(data) < limit) break
  }
  
  if (length(trades) > 0) bind_rows(trades) else data.frame()
}

# 4. Chargement des données existantes
load_or_init <- function(file) {
  if (file.exists(file)) {
    tryCatch(
      read_csv(file),
      error = function(e) data.frame()
    )
  } else {
    data.frame()
  }
}

# 5. Initialisation des données
df_normal <- load_or_init("df_normal.csv")
df_whale <- load_or_init("df_whale.csv")
q_whale <- load_or_init("q_whale.csv")

# 6. Récupération des nouvelles données
tryCatch({
  message("\nDébut de l'exécution à ", Sys.time())
  
  new_trades <- fetch_trades(
    symbol = "BTCUSDT",
    start_time = Sys.Date() - 1,
    end_time = Sys.Date()
  )
  
  if (nrow(new_trades) > 0) {
    message(nrow(new_trades), " nouvelles transactions récupérées")
    
    # Traitement des données
    processed <- new_trades %>%
      mutate(
        transaction_type = ifelse(m, "Vente", "Achat"),
        date = as.POSIXct(T/1000, origin = "1970-01-01", tz = "UTC"),
        day = as.Date(date),
        q = as.numeric(q),
        p = as.numeric(p),
        total = q * p
      ) %>%
      filter(!is.na(q) & !is.na(p))
    
    # Mise à jour des fichiers
    df_normal <- processed %>%
      group_by(day, transaction_type) %>%
      summarise(sum = sum(total), .groups = "drop") %>%
      pivot_wider(names_from = transaction_type, values_from = sum, values_fill = 0) %>%
      mutate(difference = Achat - Vente) %>%
      bind_rows(df_normal) %>%
      distinct(day, .keep_all = TRUE)
    
    df_whale <- processed %>%
      filter(q >= 10) %>%
      group_by(day, transaction_type) %>%
      summarise(sum = sum(total), .groups = "drop") %>%
      pivot_wider(names_from = transaction_type, values_from = sum, values_fill = 0) %>%
      mutate(difference_whales = Achat - Vente) %>%
      bind_rows(df_whale) %>%
      distinct(day, .keep_all = TRUE)
    
    q_whale <- processed %>%
      filter(q >= 10) %>%
      select(-m, -f, -a, -F, -T) %>%
      bind_rows(q_whale)
    
    # Sauvegarde
    write_csv(df_normal, "df_normal.csv")
    write_csv(df_whale, "df_whale.csv")
    write_csv(q_whale, "q_whale.csv")
    message("Fichiers CSV mis à jour")
    
    # Push vers GitHub (uniquement sur Render)
    if (Sys.getenv("RENDER") == "true") {
      message("Configuration Git...")
      system("git config --global user.name 'Render Bot'")
      system("git config --global user.email 'render@bot.com'")
      system(paste0("git remote set-url origin https://", Sys.getenv("GITHUB_PAT"), "@github.com/yassinedkk/binance.git"))
      system("git add *.csv")
      system("git commit -m '[Render] Auto-update' || echo 'Aucun changement'")
      system("git push origin main")
      message("Push vers GitHub réussi!")
    }
  } else {
    message("Aucune nouvelle donnée aujourd'hui")
  }
}, error = function(e) {
  message("ERREUR : ", e$message)
  write_lines(paste(Sys.time(), e$message), "error_log.txt")
})

message("Fin de l'exécution à ", Sys.time())
