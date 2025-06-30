FROM rocker/r-ver:4.3.0

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

WORKDIR /app
COPY . .

RUN R -e "install.packages(c('httr', 'jsonlite', 'dplyr', 'readr'))"

CMD ["Rscript", "binance.R"]
