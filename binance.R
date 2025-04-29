# Charger les bibliothèques nécessaires
library(httr)
library(jsonlite)
library(dplyr)

# Fonction de récupération des trades Binance
fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  url <- "https://api.binance.com/api/v3/aggTrades"
  trades <- list()

  repeat {
    params <- list(
      symbol = symbol,
      startTime = as.numeric(as.POSIXct(start_time, tz = "UTC")) * 1000,
      endTime = as.numeric(as.POSIXct(end_time, tz = "UTC")) * 1000,
      limit = limit
    )

    response <- GET(url, query = params)

    if (http_status(response)$category != "Success") {
      message("Erreur API : ", content(response, as = "text", encoding = "UTF-8"))
      break
    }

    data <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)

    if (is.null(data) || nrow(data) == 0) {
      message("Aucun trade récupéré.")
      break
    }

    trades <- append(trades, list(data))
    start_time <- as.POSIXct(max(data$T) / 1000, origin = "1970-01-01", tz = "UTC") + 1

    if (nrow(data) < limit) break
  }

  if (length(trades) > 0) {
    return(bind_rows(trades))
  } else {
    return(data.frame())
  }
}

# Initialiser les dataframes à vide
df_normal <- data.frame()
df_whale <- data.frame()
q_whale <- data.frame()

# Charger les anciens fichiers s’ils existent
if (file.exists("df_normal.csv")) {
  df_normal <- read.csv("df_normal.csv")
}
if (file.exists("df_whale.csv")) {
  df_whale <- read.csv("df_whale.csv")
}
if (file.exists("q_whale.csv")) {
  q_whale <- read.csv("q_whale.csv")
}

# Définir la paire et la période
symbol <- "BTCUSDT"
start_time <- as.POSIXct(Sys.Date() - 1, tz = "UTC")
end_time <- as.POSIXct(Sys.Date(), tz = "UTC")

# Récupérer les nouveaux trades
trades <- fetch_trades(symbol, start_time, end_time)

if (nrow(trades) > 0) {
  trades <- trades %>%
    mutate(
      transaction_type = ifelse(m == TRUE, "Vente", "Achat"),
      date = as.POSIXct(T / 1000, origin = "1970-01-01", tz = "UTC"),
      day = as.Date(date),
      q = as.numeric(as.character(q)),
      p = as.numeric(as.character(p))
    ) %>%
    filter(!is.na(q) & !is.na(p))

  # Résumé global
  daily_summary <- trades %>%
    mutate(total = q * p) %>%
    group_by(day, transaction_type) %>%
    summarise(sum = sum(total, na.rm = TRUE), .groups = "drop") %>%
    group_by(day) %>%
    summarise(
      Achat = sum(sum * (transaction_type == "Achat"), na.rm = TRUE),
      Vente = sum(sum * (transaction_type == "Vente"), na.rm = TRUE),
      difference = Achat - Vente
    )

  # Résumé whale
  daily_summary_whales <- trades %>%
    filter(q >= 10) %>%
    mutate(total = q * p) %>%
    group_by(day, transaction_type) %>%
    summarise(sum = sum(total, na.rm = TRUE), .groups = "drop") %>%
    group_by(day) %>%
    summarise(
      Achat_whales = sum(sum * (transaction_type == "Achat"), na.rm = TRUE),
      Vente_whales = sum(sum * (transaction_type == "Vente"), na.rm = TRUE),
      difference_whales = Achat_whales - Vente_whales
    )

  whales <- trades %>%
    filter(q >= 10) %>%
    select(-m, -f, -a, -F, -T)

  # Ajouter aux anciens fichiers
  df_normal <- bind_rows(df_normal, daily_summary)
  df_whale <- bind_rows(df_whale, daily_summary_whales)
  q_whale <- bind_rows(q_whale, whales)

  # Sauvegarder
  write.csv(df_normal, "df_normal.csv", row.names = FALSE)
  write.csv(df_whale, "df_whale.csv", row.names = FALSE)
  write.csv(q_whale, "q_whale.csv", row.names = FALSE)

  message("Mise à jour terminée : fichiers CSV sauvegardés.")
} else {
  message("Aucune nouvelle donnée à ajouter.")
}


