library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Configuration
SYMBOL <- "BTCUSDT"
MAX_RETRIES <- 3
PROXY_URLS <- c(
  "https://cors-anywhere.herokuapp.com/",
  "https://proxy.cors.sh/",
  "https://api.allorigins.win/get?url="
)

# Initialisation des fichiers CSV
init_files <- function() {
  files <- c("df_normal.csv", "df_whale.csv", "q_whale.csv")
  for (file in files) {
    if (!file.exists(file)) {
      write_csv(data.frame(), file)
    }
  }
}

# Fonction de récupération avec proxy
fetch_with_proxy <- function(url, params) {
  for (proxy in PROXY_URLS) {
    tryCatch({
      full_url <- if (grepl("allorigins", proxy)) {
        paste0(proxy, URLencode(url))
      } else {
        paste0(proxy, url)
      }
      
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
  init_files()
  
  # Charger les données existantes
  df_normal <- read_csv("df_normal.csv", show_col_types = FALSE)
  df_whale <- read_csv("df_whale.csv", show_col_types = FALSE)
  q_whale <- read_csv("q_whale.csv", show_col_types = FALSE)
  
  # Récupérer les nouvelles données
  params <- list(
    symbol = SYMBOL,
    startTime = as.numeric(as.POSIXct(Sys.Date() - 1)) * 1000,
    endTime = as.numeric(as.POSIXct(Sys.Date())) * 1000,
    limit = 1000
  )
  
  new_data <- tryCatch({
    fetch_with_proxy("https://api.binance.com/api/v3/aggTrades", params)
  }, error = function(e) {
    message("Échec de récupération: ", e$message)
    return(data.frame())
  })
  
  # Traitement des données
  if (nrow(new_data) > 0) {
    processed <- new_data %>%
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
  
  # Sauvegarde
  write_csv(df_normal, "df_normal.csv")
  write_csv(df_whale, "df_whale.csv")
  write_csv(q_whale, "q_whale.csv")
  
  # Push vers GitHub
  if (Sys.getenv("CI") == "true") {
    system("git config --global user.name 'GitHub Actions'")
    system("git config --global user.email 'actions@github.com'")
    system("git add *.csv")
    system("git commit -m 'Auto-update data' || echo 'No changes'")
    system("git push")
  }
}

# Exécution
tryCatch({
  main()
}, error = function(e) {
  message("ERREUR: ", e$message)
  write_lines(paste(Sys.time(), e$message), "error_log.txt")
})
