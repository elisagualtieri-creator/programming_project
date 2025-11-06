# 1. Immagine di base 
FROM ubuntu:latest

# 2. Impostiamo l'ambiente per un'installazione non interattiva
ENV DEBIAN_FRONTEND=noninteractive

# 3. CONFIGURIAMO IL REPOSITORY R CRAN 
# Installiamo gli strumenti per aggiungere repository
RUN apt-get update && \
    apt-get install -y --no-install-recommends dirmngr wget software-properties-common && \                  #gestiamo le chaivi di sicurezza
# Importiamo la chiave GPG di R-CRAN 
    wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | \                           #chiave pubblica
    gpg --dearmor | tee /usr/share/keyrings/cran.gpg > /dev/null && \                                        #convertiamo la chiave in formato leggibile da atp e la salviamo in un file
# Aggiunge il repository CRAN per 'noble' (ubuntu:latest) 
    echo "deb [signed-by=/usr/share/keyrings/cran.gpg] https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/" | \         #indirizzo repository di noble
    tee /etc/apt/sources.list.d/cran.list && \
# Puliamo la cache
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# 4. INSTALLIAMO R E LE DIPENDENZE DI SISTEMA
# Aggiorniamo dopo aver aggiunto il repo CRAN
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        # Pacchetti R
        r-base \
        r-base-dev \
        # Compilatori e utilità per rmarkdown
        build-essential \
        curl \
        pandoc \
        # Dipendenze di libreria per i pacchetti R 
        libcurl4-gnutls-dev \
        libssl-dev \
        libxml2-dev \
        libpq-dev \
        libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# 5. INSTALLIAMO I PACCHETTI R RICHIESTI
#avviamo R e installiamo i pacchetti
RUN R -e "install.packages(c('data.table', 'dplyr', 'tidyverse', 'knitr', 'rmarkdown', 'sqldf', 'tidyr', 'ggplot2', 'scales'), repos = 'https://cloud.r-project.org/')"

# 6. Comando di avvio

CMD ["/bin/bash"]
