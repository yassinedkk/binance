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
  
  start_ms <- as.numeric(as.POSIXct(start_time, tz="UTC")) * 1000
  end_ms   <- as.numeric(as.POSIXct(end_time,   tz="UTC")) * 1000
  
  all_trades <- list()
  current_start <- start_ms
  
  repeat {
    query <- list(
      symbol    = symbol,
      startTime = current_start,
      endTime   = end_ms,
      limit     = limit
    )
    
    res <- httr::GET(
      url = paste0(proxy_base, path),
      query = query,
      httr::timeout(15)
    )
    
    httr::stop_for_status(res)
    
    trades <- jsonlite::fromJSON(httr::content(res, "text"), simplifyDataFrame = TRUE)
    
    if (length(trades) == 0) break
    
    all_trades[[length(all_trades) + 1]] <- trades
    
    last_time <- trades$T[nrow(trades)]
    if (last_time >= end_ms || nrow(trades) < limit) break
    
    # Ajouter 1 ms pour éviter doublons
    current_start <- last_time + 1
    Sys.sleep(0.2)  # Évite d'être trop agressif avec l'API
  }
  
  return(dplyr::bind_rows(all_trades))
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
  mutate(
    Achat = ifelse(!"Achat" %in% colnames(.), 0, Achat),
    Vente = ifelse(!"Vente" %in% colnames(.), 0, Vente),
    difference_whales = Achat - Vente
  )

  
  # q_whale
  q_whale <- trades_clean %>%
    filter(q >= 10) %>%
    select(day, date, q, p, transaction_type)
  
  # Sauvegarder les CSV
  write_csv(df_normal, "df_normal.csv")
  write_csv(df_whale,  "df_whale.csv")
  write_csv(q_whale,   "q_whale.csv")

system("git config --global user.email 'your-email@example.com'")
system("git config --global user.name 'Your Name'")
system("git add *.csv")
system("git commit -m 'Auto update CSVs'")
system("git push origin main")
  
  message("==== Fin exécution ", Sys.time(), " ====")
}

# Lancer
main()
