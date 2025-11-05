# 1. Partiamo da Ubuntu 20.04
FROM ubuntu:20.04

# Evito che le installazioni chiedano l'input dell'utente
ENV DEBIAN_FRONTEND=noninteractive

#2. Aggiorniamo e installiamo R e le dipendenze di sistema
RUN apt-get update && apt-get install -y --no-install-recommends \
    r-base \
    r-base-dev \
    # Compilatori di base (spesso già inclusi in r-base-dev, ma per sicurezza)
    g++ \
    make \
    # Dipendenze per pacchetti comuni (curl, ssl, xml) 
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    # Dipendenze per RMarkdown/Knitr
    pandoc \
    pandoc-citeproc \
    # Dipendenze per Grafica (ggplot2, cairo, ecc.)
    libcairo2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    libxt-dev \
    # Dipendenze comuni per pacchetti GIS/Mappe (spesso richiesti)
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    # Utilità
    wget \
    ca-certificates \
    && apt-get clean && rm -rf /var/lib/apt/lists/* # Pulizia
# 3. Installiamo i pacchetti R che ci servono
RUN R -e "install.packages(c('data.table', 'dplyr', 'ggplot2' 'rmarkdown', 'knitr'), repos='https://cran.rstudio.com/', dependencies=TRUE)"

# 4. Creiamo una cartella di lavoro
WORKDIR /work

# 5. Impostiamo il comando di default per avviare una shell Bash
CMD ["/bin/bash"]