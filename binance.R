library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(tidyr)
# Nouvelle fonction avec rotation de proxies
fetch_trades <- function(symbol, start_time, end_time, limit = 1000) {
  proxies <- c(
    "https://cors-anywhere.herokuapp.com/",
    "https://api.allorigins.win/get?url=",
    "https://thingproxy.freeboard.io/fetch/"
  )
  
  base_url <- "https://api.binance.com/api/v3/aggTrades"
  
  for(i in 1:length(proxies)) {
    tryCatch({
      url <- ifelse(
        grepl("allorigins", proxies[i]),
        paste0(proxies[i], URLencode(base_url)),
        paste0(proxies[i], base_url)
      )
      
      response <- GET(url, 
                    query = list(
                      symbol = symbol,
                      startTime = as.numeric(as.POSIXct(start_time, tz = "UTC")) * 1000,
                      endTime = as.numeric(as.POSIXct(end_time, tz = "UTC")) * 1000,
                      limit = limit
                    ),
                    timeout(10))
      
      if(status_code(response) == 200) {
        content <- if(grepl("allorigins", proxies[i])) {
          fromJSON(rawToChar(response$content))$contents
        } else {
          content(response, "text")
        }
        return(fromJSON(content))
      }
    }, error = function(e) NULL)
  }
  stop("Tous les proxies ont échoué")
}


# Chargement des données existantes
load_or_init <- function(file) {
  if (file.exists(file)) {
    tryCatch(
      read_csv(file, show_col_types = FALSE),
      error = function(e) {
        message("Erreur lecture ", file, ": ", e$message)
        data.frame()
      }
    )
  } else {
    data.frame()
  }
}

# Initialisation des données
df_normal <- load_or_init("df_normal.csv")
df_whale <- load_or_init("df_whale.csv")
q_whale <- load_or_init("q_whale.csv")

# Fonction principale
tryCatch({
  message("\nDébut de l'exécution à ", Sys.time())
  
  # Récupération des données
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
    
    # Mise à jour df_normal
    df_normal_update <- processed %>%
      group_by(day, transaction_type) %>%
      summarise(sum = sum(total), .groups = "drop") %>%
      pivot_wider(names_from = transaction_type, 
                  values_from = sum, 
                  values_fill = 0) %>%
      mutate(difference = Achat - Vente)
    
    df_normal <- bind_rows(df_normal, df_normal_update) %>%
      distinct(day, .keep_all = TRUE)
    
    # Mise à jour df_whale
    df_whale_update <- processed %>%
      filter(q >= 10) %>%
      group_by(day, transaction_type) %>%
      summarise(sum = sum(total), .groups = "drop") %>%
      pivot_wider(names_from = transaction_type, 
                  values_from = sum, 
                  values_fill = 0) %>%
      mutate(difference_whales = Achat - Vente)
    
    df_whale <- bind_rows(df_whale, df_whale_update) %>%
      distinct(day, .keep_all = TRUE)
    
    # Mise à jour q_whale
    q_whale_update <- processed %>%
      filter(q >= 10) %>%
      select(-m, -f, -a, -F, -T)
    
    q_whale <- bind_rows(q_whale, q_whale_update)
    
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
  } else {
    message("Aucune nouvelle donnée aujourd'hui")
  }
}, error = function(e) {
  message("ERREUR CRITIQUE: ", e$message)
  write_lines(paste(Sys.time(), e$message), "error_log.txt")
})

message("Fin de l'exécution à ", Sys.time())
