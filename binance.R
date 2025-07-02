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
  "https://api.allorigins.win/get?url="
)

# Initialisation des fichiers
init_files <- function() {
  files <- c("df_normal.csv", "df_whale.csv", "q_whale.csv")
  for (file in files) {
    if (!file.exists(file)) {
      write_csv(data.frame(), file)
    }
  }
}

# Fonction de récupération avec proxy
fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  base_url <- "https://api.binance.com/api/v3/aggTrades"
  
  for (proxy in PROXY_URLS) {
    tryCatch({
      full_url <- if (grepl("allorigins", proxy)) {
        paste0(proxy, URLencode(base_url))
      } else {
        paste0(proxy, base_url)
      }
      
      params <- list(
        symbol = symbol,
        startTime = as.numeric(as.POSIXct(start_time)) * 1000,
        endTime = as.numeric(as.POSIXct(end_time)) * 1000,
        limit = limit
      )
      
      response <- GET(full_url, query = params, timeout(10))
      
      if (status_code(response) == 200) {
        content <- if (grepl("allorigins", proxy)) {
          fromJSON(content(response, "text"))$contents
        } else {
          content(response, "text")
        }
        return(fromJSON(content))
      }
    }, error = function(e) {
      message("Proxy ", proxy, " échoué: ", e$message)
    })
  }
  stop("Tous les proxies ont échoué")
}

# Fonction principale
main <- function() {
  message("Début de l'exécution: ", Sys.time())
  init_files()
  
  # Chargement des données
  df_normal <- read_csv("df_normal.csv", show_col_types = FALSE)
  df_whale <- read_csv("df_whale.csv", show_col_types = FALSE) 
  q_whale <- read_csv("q_whale.csv", show_col_types = FALSE)
  
  # Récupération des données
  new_trades <- tryCatch({
    fetch_trades(SYMBOL, Sys.Date() - 1, Sys.Date())
  }, error = function(e) {
    message("Échec: ", e$message)
    return(data.frame())
  })
  
  # Traitement des données
  if (nrow(new_trades) > 0) {
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
      df_normal <- processed %>%
        group_by(day, transaction_type) %>%
        summarise(sum = sum(total), .groups = "drop") %>%
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
  
  # Sauvegarde
  write_csv(df_normal, "df_normal.csv")
  write_csv(df_whale, "df_whale.csv") 
  write_csv(q_whale, "q_whale.csv")
  
  # Git push
  if (Sys.getenv("CI") == "true") {
    system("git config --global user.name 'GitHub Actions'")
    system("git config --global user.email 'actions@github.com'")
    system("git add *.csv")
    system("git commit -m 'Auto-update data' || echo 'No changes'")
    system("git push")
  }
  
  message("Fin de l'exécution: ", Sys.time())
}

# Exécution
tryCatch({
  main()
}, error = function(e) {
  message("ERREUR: ", e$message)
  writeLines(paste(Sys.time(), e$message), "error_log.txt")
})
