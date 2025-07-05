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
  query <- list(
    symbol    = symbol,
    startTime = as.numeric(as.POSIXct(start_time, tz="UTC")) * 1000,
    endTime   = as.numeric(as.POSIXct(end_time,   tz="UTC")) * 1000,
    limit     = limit
  )

  res <- httr::GET(
    url = paste0(proxy_base, path),
    query = query,
    httr::timeout(15)
  )
  httr::stop_for_status(res)
  df <- jsonlite::fromJSON(httr::content(res, "text"), simplifyDataFrame = TRUE)
  return(df)
}


main <- function() {
  message("==== Début exécution ", Sys.time(), " ====")
  
  # Récupération des données
  trades <- fetch_trades(SYMBOL, Sys.Date() - 1, Sys.Date())
  
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
    mutate(difference_whales = Achat - Vente)
  
  # q_whale
  q_whale <- trades_clean %>%
    filter(q >= 10) %>%
    select(day, date, q, p, transaction_type)
  
  # Sauvegarder les CSV
  write_csv(df_normal, "df_normal.csv")
  write_csv(df_whale,  "df_whale.csv")
  write_csv(q_whale,   "q_whale.csv")
  
  message("==== Fin exécution ", Sys.time(), " ====")
}

# Lancer
main()
