library(httr)
library(jsonlite)
library(dplyr)

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
    
    # Imprimer les paramètres pour vérifier les timestamps
    print(paste("startTime (ms):", params$startTime))
    print(paste("endTime (ms):", params$endTime))
    
    response <- GET(url, query = params)
    
    # Vérifier le statut de la réponse
    if (http_status(response)$category != "Success") {
      print("Erreur dans la requête API.")
      print(content(response, as = "text", encoding = "UTF-8"))
      break
    }
    
    data <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
    
    # Vérification si la réponse est correcte et contient des données
    if (is.null(data) || length(data) == 0 || is.null(nrow(data))) {
      print("Aucun trade récupéré ou données invalides.")
      break  # Arrêter si aucun trade n'est retourné ou si les données sont invalides
    }
    
    # Ajouter les données au tableau de résultats
    trades <- append(trades, list(data))
    
    # Mettre à jour le start_time pour commencer à partir du dernier trade
    start_time <- as.POSIXct(max(data$T) / 1000, origin = "1970-01-01", tz = "UTC") + 1
    
    if (nrow(data) < limit) break  # Arrêter si le nombre de résultats est inférieur à la limite
  }
  
  # Transformer la liste de trades en data frame si elle n'est pas vide
  if (length(trades) > 0) {
    trades_df <- bind_rows(trades)  # Combine toutes les données en un seul data frame
    return(trades_df)
  } else {
    return(data.frame())  # Retourner un tableau vide si aucune donnée n'a été récupérée
  }
}
if (interactive()) {
  symbol <- "BTCUSDT"
  
  # Lire les dates de début et de fin en entrée utilisateur
  start_time_str <- readline(prompt = "Entrez la date de début (YYYY-MM-DD): ")
  end_time_str <- readline(prompt = "Entrez la date de fin (YYYY-MM-DD): ")
  
  start_time <- as.POSIXct(start_time_str, format = "%Y-%m-%d", tz = "UTC")
  end_time <- as.POSIXct(end_time_str, format = "%Y-%m-%d", tz = "UTC")
  
  # Récupérer les trades
  trades <- fetch_trades(symbol, start_time, end_time)
  
  # Si des trades sont récupérés, les afficher dans un tableau (data frame)
  if (nrow(trades) > 0) {
    print("Transactions récupérées :")
    print(head(trades))  # Afficher les premières lignes du tableau
    
    # Afficher dans une vue sous forme de tableau si vous êtes dans RStudio
    View(trades)
  } else {
    print("Aucune transaction récupérée pour la période spécifiée.")
  }
}

trades<-cbind(trades,transaction_type=ifelse(trades$m==TRUE,"Vente","Achat"))
trades$date <- as.POSIXct(trades$T / 1000, origin = "1970-01-01", tz = "UTC")  # Convertir le timestamp en date
trades$day <- as.Date(trades$date)  # Extraire la date (sans l'heure)

# Conversion et nettoyage des données (comme précédemment)
trades_clean <- trades %>%
  mutate(
    q = as.numeric(as.character(q)),
    p = as.numeric(as.character(p))
  ) %>%
  filter(!is.na(q) & !is.na(p))

# Calcul pour toutes les transactions
daily_summary <- trades_clean %>%
  mutate(total = q * p) %>%
  group_by(day, transaction_type) %>%
  summarise(sum = sum(total, na.rm = TRUE), .groups = "drop") %>%
  group_by(day) %>%
  summarise(
    Achat = sum(sum * (transaction_type == "Achat"), na.rm = TRUE),
    Vente = sum(sum * (transaction_type == "Vente"), na.rm = TRUE),
    difference = Achat - Vente
  )

# Calcul pour les transactions de "whales" (plus de 10 BTC)
daily_summary_whales <- trades_clean %>%
  filter(q >= 10) %>%  # Filtre pour les transactions de 10 BTC ou plus
  mutate(total = q * p) %>%
  group_by(day, transaction_type) %>%
  summarise(sum = sum(total, na.rm = TRUE), .groups = "drop") %>%
  group_by(day) %>%
  summarise(
    Achat_whales = sum(sum * (transaction_type == "Achat"), na.rm = TRUE),
    Vente_whales = sum(sum * (transaction_type == "Vente"), na.rm = TRUE),
    difference_whales = Achat_whales - Vente_whales
  )
t_wales=trades_clean[,-4:-8]
whales= subset(t_wales,trades_clean$q >= 10)
df_normal=daily_summary
df_whale=daily_summary_whales
q_whale=whales

write.csv(df_normal, "df_normal.csv", row.names = FALSE)
write.csv(df_whale, "df_whale.csv", row.names = FALSE)
write.csv(q_whale, "q_whale.csv", row.names = FALSE)

system("git add df_normal.csv")  
system("git commit -m 'Mise à jour des données récupérées'")  
system("git push origin main")


