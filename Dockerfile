FROM rocker/r-ver:4.3.0

# Installer les dépendances système
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git

# Copier les fichiers nécessaires
COPY binance.R /app/
COPY *.csv /app/  # Vos fichiers CSV initiaux

# Installer les packages R
RUN R -e "install.packages(c('httr', 'jsonlite', 'dplyr', 'readr'), repos='https://cloud.r-project.org')"

# Dossier de travail
WORKDIR /app

# Commande d'exécution
CMD ["Rscript", "binance.R"]
