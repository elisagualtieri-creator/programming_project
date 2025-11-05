#EX1: Filter, summarize, and group bulk counts. 

# Carico i pacchetti che useremo in questo esercizio
# data.table per il metodo veloce, dplyr per il metodo alternativo.
library(data.table)
library(dplyr)
library(ggplot2)

#creo gli oggetti data.table usnado la funzione "fast read"
counts_dt <- fread("data/bulk_counts_long.csv")
metadata_dt <- fread("data/sample_metadata.csv")
# creo anche le variabili per usare data.frame
counts_df <- as.data.frame(counts_dt)
metadata_df <- as.data.frame(metadata_dt)
# guardo le prime righe per essere sicura che sia tutt a posto
print("Dati dei conteggi:")
head(counts_df)

print("Dati dei metadati:")
head(metadata_df)

#Approccio con data.frame
time_df_ex1 <- system.time({
#uniamo le due tabelle usando come punto di incontro le righe con lo stesso id per unire le condizioni (nel file metadata) e il file con i risultati degli esperimenti
  merged_df <- merge(counts_df, metadata_df, by = "sample_id")
  #filtriamo per tenere solo le cose richieste
  filtered_df <- subset(merged_df, condition == "treated" & grepl("^GENE_00", gene))
  #raggrupiamo e calcoliamo media e mediana
  summary_df_ex1 <- aggregate(count ~ gene, data = filtered_df, FUN = function(x) { c(mean_count = mean(x), median_count = median(x))
})
})
print("--- Risultato con data.frame (Esercizio 1) ---")
head(summary_df_ex1)
print(time_df_ex1)

#Approccio con data.table
#impostiamo una chiave che ordina fisicamente la tabella in base alla colonna specifica e crea un indice
time_dt_ex1 <- system.time({
  setkey(counts_dt, sample_id)
  setkey(metadata_dt, sample_id)
#uniamo le tabelle
  merged_dt <- metadata_dt[counts_dt]
#filtriamo e calcoliamo le statistiche in un solo comando
  summary_dt_ex1 <- merged_dt[condition == "treated" & grepl("^GENE_00", gene), 
                            .(mean_count = mean(count), median_count = median(count)), 
                            by = gene] 
})

print("--- Risultato con data.table (Esercizio 1) ---")
head(summary_dt_ex1)
 print(time_dt_ex1)

#Approccio consigliato: dplyr 
time_dplyr_ex1 <- system.time({ 
 summary_dplyr_ex1 <- counts_df %>%
   # 1. Uniamo le tabelle
   left_join(metadata_df, by = "sample_id") %>%
   # 2. Filtriamo le righe 
   filter(condition == "treated", grepl("^GENE_00", gene)) %>%
   # 3. Raggruppiamo i dati per gene in modo tale che l'operazione sia fatta per ogni gene
   group_by(gene) %>%
   # 4. Calcoliamo media e mediana con summarise()
   summarise(
     mean_count = mean(count),
     median_count = median(count)
   )
 
 })
print("--- Risultato con dplyr (Esercizio 1) ---")
head(summary_dplyr_ex1)
print(time_dplyr_ex1)

#EX2: Add QC-style derived columns without copying.
#approccio con data.table
counts_dt_ex2 <- copy(counts_dt)
time_dt_ex2 <- system.time({
  # Aggiungiamo le due colonne in un unico comando, usando l'operatore `:=`.
  counts_dt_ex2[, ':=' (log2_count = log2(count + 1),
                        high = count > 100)]
  #sovrascriviamo la colonna "high"
  counts_dt_ex2[, high := count > median(count), by = gene]
  
})

print("--- Risultato con data.table (Esercizio 2) ---")
head(counts_dt_ex2)
print(time_dt_ex2)

#Approccio con data.frame (in questo caso ogni volta che aggiungaimo una colonna si crea automaticamene una copia dell'intera tabella)

counts_df_ex2 <- counts_df
time_df_ex2 <- system.time({
  
  # Aggiungiamo una colonna log2_count. Usiamo count + 1 per evitare log(0) che non è definito.
  counts_df_ex2$log2_count <- log2(counts_df_ex2$count + 1)
  # Aggiungiamo la colonna 'high' con la soglia fissa > 100
  counts_df_ex2$high <- counts_df_ex2$count > 100
  
  # Calcoliamo la mediana dei conteggi per ogni gene
  median_by_gene_df <- aggregate(count ~ gene, data = counts_df_ex2, FUN = median)
  colnames(median_by_gene_df) <- c("gene", "median_count")
  # Vediamo com'è fatta questa tabella riassuntiva
  print("Tabella delle mediane per gene:")
  head(median_by_gene_df)
  dim(median_by_gene_df)
  # Uniamo la tabella originale con la nostra tabella delle mediane
  counts_with_median_df <- merge(counts_df_ex2, median_by_gene_df, by = "gene")
  
  # Diamo una sbirciatina per vedere la nuova colonna "median_count"
  print("Tabella originale con la mediana di gruppo aggiunta:")
  head(counts_with_median_df)
  dim(counts_with_median_df)
  # Sovrascriviamo la colonna 'high' facendo un semplice confronto riga per riga
  counts_with_median_df$log2_count <- log2(counts_with_median_df$count + 1)
  counts_with_median_df$high <- counts_with_median_df$count > counts_with_median_df$median_count
  print("--- Risultato finale con aggregate + merge (Esercizio 2) ---")
  head(counts_with_median_df)
})

# Stampiamo le prime righe del risultato e il tempo impiegato
print("--- Risultato con data.frame (Esercizio 2) ---")
head(counts_df_ex2)
print(time_df_ex2)

#Approccio con dplyr
time_dplyr_ex2 <- system.time({
  #aggiungiamo le due nuove colonne
  counts_dplyr_ex2 <- counts_df %>%
  mutate(
    log2_count = log2(count + 1),
    high = count > 100
  ) %>%
  #raggruppiamo per gene
  group_by(gene) %>%
  #sovrascriviamo la colonna high, essendo i dati raggruppati per gene la meida vienen calcolata per ogni gruppo di geni
  mutate(high = count > median(count))
})
print("--- Risultato con dplyr (Esercizio 2) ---")
head(counts_dplyr_ex2)
print(time_dplyr_ex2)

#Ex3. Speed up joins/lookups. 
#approccio con data.table
counts_dt <- fread("data/bulk_counts_long.csv")
metadata_dt <- fread("data/sample_metadata.csv")
#3.1 Join
time_dt_ex3a <- system.time({
  # 1. Impostiamo le chiavi (ordiniamo le tabelle)
  setkey(counts_dt, sample_id)
  setkey(metadata_dt, sample_id)
  #eseguiamo il join
  merged_dt_ex3 <- metadata_dt[counts_dt]
})

print("--- Risultato con data.table (Task 3.1: Join) ---")
head(merged_dt_ex3)
print(time_dt_ex3a)

#3.2 Benchmark
#Prima di impostare l'indice
time_dt_ex3b_before <- system.time({
  #chiediamo di cercare un particolare gene per capire quanto ci impiega senza indice
  subset_dt_before <- counts_dt[gene == "GENE_0091" & sample_id == "S24"]
})
print("--- Risultato con data.table (Task 3.2: Ricerca PRIMA dell'indice) ---")
print(subset_dt_before)
print(time_dt_ex3b_before)
#dopo aver creato l'indice 
setindex(counts_dt, gene, sample_id)
time_dt_ex3b_after <- system.time({
  subset_dt_after <- counts_dt[gene == "GENE_0091" & sample_id == "S24"]
  #ora data.table usa l'indice per trovare le righe istantaneamente
})
print("--- Risultato con data.table (Task 3.2: Ricerca DOPO) ---")
print(subset_dt_after)
print(time_dt_ex3b_after)

#approccio con data.frame
#3.1 Join
time_df_ex3a <- system.time({
  # merge() deve cercare ogni sample_id di counts_df dentro metadata_df
  merged_df_ex3 <- merge(counts_df, metadata_df, by = "sample_id")
})

print("--- Risultato con data.frame (Task 3.1: Join) ---")
head(merged_df_ex3)
print(time_df_ex3a)

#3.2 Benchmark (con data.frame in realtà non puoi mettere l'indice)
time_df_ex3b <- system.time({
  # R base deve scansionare tutte le 12.000 righe per trovare quelle che ci servono
  subset_df_ex3 <- subset(counts_df, gene == "GENE_0091" & sample_id == "S24")
})

print("--- Risultato con data.frame (Task 3.2: Ricerca) ---")
print(subset_df_ex3)
print(time_df_ex3b)

#approccio con dplyr
#3.1 Join
time_dplyr_ex3a <- system.time({
  merged_dplyr_ex3 <- left_join(counts_df, metadata_df, by = "sample_id")
})

print("--- Risultato con dplyr (Task 3.1: Join) ---")
head(merged_dplyr_ex3)
print(time_dplyr_ex3a)
#3.2 Benchmark: uso filter ma non metto un vero e proprio indice 
time_dplyr_ex3b <- system.time({
  subset_dplyr_ex3 <- filter(counts_df, gene == "GENE_0091", sample_id == "S24")
})

print("--- Risultato con dplyr (Task 3.2: Ricerca) ---")
print(subset_dplyr_ex3)
print(time_dplyr_ex3b)

#Ex4. Annotate counts with sample and patient info. 

#approccio con data.table
#4.1 
counts_dt_ex4 <- copy(counts_dt)
metadata_dt_ex4 <- copy(metadata_dt)
#impostiamo la chiave sulla copia che ho fatto e uniamo i due file
time_dt_ex4a <- system.time({
  setkey(counts_dt_ex4, sample_id)
  setkey(metadata_dt_ex4, sample_id)
  merged_dt_ex4 <- metadata_dt_ex4[counts_dt_ex4]
  # Raggruppiamo per 'patient_id' e calcoliamo la somma
  summary_dt_ex4 <- merged_dt_ex4[, .(total_count = sum(count)), by = patient_id]

})

print("--- Risultato con data.table (Task 4.1: Totali per Paziente) ---")
head(summary_dt_ex4)
print(time_dt_ex4a)

#4.2  
time_dt_ex4b <- system.time({
  #calcoliamo la media per gene e condizione
  avg_dt_ex4b <- merged_dt_ex4[, .(avg_count = mean(count)), by = .(condition, gene)]
  #oridianmo le righe dal più grande al più piccolo e per ogni gruppo prendo le prime 10 righe
  summary_dt_ex4b <- avg_dt_ex4b[order(-avg_count), head(.SD, 10), by = condition]
  
})

print("--- Risultato con data.table (Task 4.2) ---")
print(summary_dt_ex4b)
print(time_dt_ex4b)

#approccio con data.frame
#4.1
counts_df_ex4 <- counts_df
metadata_df_ex4 <- metadata_df
time_df_ex4a <- system.time({
  #unimao aggreghiamo per paziente e calcoliamo la somma
  merged_df_ex4 <- merge(counts_df_ex4, metadata_df_ex4, by = "sample_id")
  summary_df_ex4a <- aggregate(count ~ patient_id, data = merged_df_ex4, FUN = sum)
  #rinominiamo la colonna per chiarezza
  colnames(summary_df_ex4a) <- c("patient_id", "total_count")
})

print("--- Risultato con data.frame (Task 4.1: Totali per Paziente) ---")
head(summary_df_ex4a)
print(time_df_ex4a)
#4.2
time_df_ex4b <- system.time({
  #calcoliamo la media per gene e per condizione
  avg_df_ex4b <- aggregate(count ~ gene + condition, data = merged_df_ex4, FUN = mean)
  #dividiamo la tabella in una lista con trattati e controlli  
  split_df_ex4b <- split(avg_df_ex4b, avg_df_ex4b$condition)
  #per ogni tabella nella lista ordinamo e prendiamo le prime 10
  top10_list_ex4b <- lapply(split_df_ex4b, function(x) {
    x_ordered <- x[order(x$count, decreasing = TRUE), ]
    head(x_ordered, 10)
  })
  # Rimettiamo insieme la lista
  summary_df_ex4b <- do.call(rbind, top10_list_ex4b)
  
})

print("--- Risultato con data.frame (Task 4.2: Top 10 per Condizione) ---")
print(head(summary_df_ex4b, 5))
print(tail(summary_df_ex4b, 5))
print(time_df_ex4b)

#approccio con dplyr
#4.1
counts_df_ex4a <- counts_df
metadata_df_ex4a <- metadata_df
time_dplyr_ex4a <- system.time({
  summary_dplyr_ex4a <- counts_df_ex4a %>%
    left_join(metadata_df_ex4a, by = "sample_id") %>%
    group_by(patient_id) %>%
    summarise(total_count = sum(count))
  
})
print("--- Risultato con dplyr (Task 4.1: Totali per Paziente) ---")
head(summary_dplyr_ex4a)
print(time_dplyr_ex4a)

#4.2
counts_df_ex4b <- counts_df
metadata_df_ex4b <- metadata_df
time_dplyr_ex4b <- system.time({
  summary_dplyr_ex4b <- counts_df_ex4b %>% 
    left_join(metadata_df_ex4b, by = "sample_id") %>%
    group_by(condition, gene) %>%
    #riduciamo i dati ad una singola riga con summarise, creiamo una nuova colonna con la media (viene chiesto di trovare la media per ogni singolo gene, per questo raggruppiamo per gene e per condizione)
    summarise(avg_count = mean(count), .groups = 'drop') %>%
    #con drop togliamo il vecchio raggruppamento perchè ora devo trovare i 10 geni più alti per ogni condizione)
    group_by(condition) %>%
    #prendiamo la parte con i valori più alti
    slice_max(order_by = avg_count, n = 10) %>%
    #per le due condizioni ordiniamo i 10 geni in modo decrescente
    arrange(condition, desc(avg_count))
})

print("--- Risultato con dplyr (Task 4.2: Top 10 per Condizione) ---")
print(summary_dplyr_ex4b)
print(time_dplyr_ex4b)

#EX.5:Classify values against reference intervals. 
#carichiamo i file che ci servono 

master_labs_dt <- fread("data/clinical_labs.csv")
master_ranges_dt <- fread("data/lab_reference_ranges.csv")

# Creiamo le versioni in data.frame
master_labs_df <- as.data.frame(master_labs_dt)
master_ranges_df <- as.data.frame(master_ranges_dt)

#approccio con data.frame 
labs_df_ex5 <- master_labs_df
ranges_df_ex5 <- master_ranges_df

time_df_ex5 <- system.time({
  #---Task 1---
  #data.frame non ha un non-equi-join diretto quindi procediamo con un classico merge usando la colonna lab_id
  merged_df_ex5 <- merge(labs_df_ex5, ranges_df_ex5, by = "lab")
  #creiamo una colonna chiamata status e assegnamo ad ongi singola riga il valore "out of range"
  merged_df_ex5$status <- "out_of_range"
  #creiamo un indice logico che contenga le colonne che hanno il valore sia maggiore/uguale al lower_bound che minore uguale all'upper_buond, il risultato è un vettore con valore T/F se è nomrale o fuori range
  is_normal <- merged_df_ex5$value >= merged_df_ex5$lower_bound & 
    merged_df_ex5$value <= merged_df_ex5$upper_bound
  #ora sostituiamo il valore out of range con normal solo nelle rige TRUE
  merged_df_ex5$status[is_normal] <- "normal"
  #---Task 2---
  #creiamo una tabella che riporta i risultati "out of range"
  abnormal_df_ex5 <- subset(merged_df_ex5, status == "out_of_range")
  #raggruppiamo i dati per patient_id usando la tabella con solo i risultati anormali
  summary_patient_df_ex5 <- aggregate(value ~ patient_id, data = abnormal_df_ex5, FUN = length)
  #rinominimao le colonne per chiarezza
  colnames(summary_patient_df_ex5) <- c("patient_id", "abnormal_count")
  #raggruppaimo e calcoliamo gli anormali per lab_id invece che per patient _id
  summary_lab_df_ex5 <- aggregate(value ~ lab, data = abnormal_df_ex5, FUN = length)
  colnames(summary_lab_df_ex5) <- c("lab", "abnormal_count")
})

print("--- Risultato con data.frame (Esercizio 5) ---")
print("Classificazione (prime 5 righe):")
head(merged_df_ex5, 5)
print("Conteggio per paziente (prime 5 righe):")
head(summary_patient_df_ex5, 5)
print("Conteggio per esame (prime 5 righe):")
head(summary_lab_df_ex5, 5)
print(time_df_ex5)

#Approccio con data.table: devo creare una tabella semplificata che tolga il sesso del pazinete dai ranges (data.table se no si ferma con un errore "Join results in 640 rows." dato dal fatto che ci sono due intervalli M e F per lo stesso lab)
#Creiamo una nuova tabella di riferimento "semplice"
ranges_simple_dt <- master_ranges_dt[, .(
  lower = mean(lower),
  upper = mean(upper )
), by = lab]

print("--- Tabella di riferimento semplificata creata ---")
head(ranges_simple_dt)

time_dt_ex5 <- system.time({
  labs_dt_ex5 <- copy(master_labs_dt)
  ranges_dt_ex5 <- copy(ranges_simple_dt)
  setkey(labs_dt_ex5, lab)
  setkey(ranges_dt_ex5, lab)
  #eseguiamo il join
  labeled_dt_ex5 <- ranges_dt_ex5[labs_dt_ex5]
  #creiamo la colonna status che ha due opzioni
  labeled_dt_ex5[, status := ifelse(value >= lower & value <= upper, 
                                    "normal", 
                                    "out_of_range")]

  #creiamo ora una tabella con gli anormali
  abnormal_dt_ex5 <- labeled_dt_ex5[status == "out_of_range"]
  #creiamo la tabella sommaria e calcoliamo il numero di righe
  summary_patient_dt_ex5 <- abnormal_dt_ex5[, .(abnormal_count = .N), by = patient_id]
  summary_lab_dt_ex5 <- abnormal_dt_ex5[, .(abnormal_count = .N), by = lab]
})

print("--- Risultato con data.table (Esercizio 5) ---")
print("Classificazione (prime 5 righe):")
head(labeled_dt_ex5, 5)
print("Conteggio per paziente (prime 5 righe):")
head(summary_patient_dt_ex5, 5)
print("Conteggio per esame (prime 5 righe):")
head(summary_lab_dt_ex5, 5)
print(time_dt_ex5)

#EX 6: Nearest-time matching of vitals to lab draws. 
#carichiamo i file che ci servono
master_vitals_dt <- fread("data/vitals_time_series.csv")
#convertiamo le colonne da testo in data/ora
master_labs_dt[, time_iso := as.POSIXct(time_iso)]
master_vitals_dt[, time_iso := as.POSIXct(time_iso)]

#Approccio con data.table (in data.frame non esiste ls funzione rolling_join, ho provato a farlo ma non riesco perchè il codice è troppo complesso)
time_dt_ex6 <- system.time({
  labs_dt_ex6 <- copy(master_labs_dt)
  vitals_dt_ex6 <- copy(master_vitals_dt)
  #rimodelliamo la colonna value 
  vitals_wide_dt_ex6 <- dcast(vitals_dt_ex6, 
                              patient_id + time_iso ~ vital, 
                              value.var = "value")
  labs_dt_ex6[, lab_time := time_iso]
 
   # --- Task 1: Rolling Join ---
  
  # 1. Impostiamo le chiavi su ENTRAMBE le colonne (paziente e tempo)
  setkey(vitals_dt_ex6, patient_id, time_iso)
  setkey(labs_dt_ex6, patient_id, time_iso)
  #2. Eseguiamo il join: Per ogni riga in 'labs_dt_ex6' (i), trova la riga più vicina
  # in 'vitals_dt_ex6' (x) che abbia lo stesso 'patient_id'.
  joined_dt_ex6 <- vitals_wide_dt_ex6[labs_dt_ex6, roll = "nearest"]
  
  # --- Task 2: Calcolo Time Lag ---
  
  # Creiamo la colonna del time lag per riferimento
  joined_dt_ex6[, time_lag_minutes := as.numeric(difftime(lab_time, time_iso, units = "mins"))]
  
  # --- Task 3: Correlazione ---
  
  # Filtriamo per 'CRP' e calcoliamo la correlazione per paziente
  summary_dt_ex6 <- joined_dt_ex6[lab == "CRP",    #prendiamo solo le righe dove lab= CPR
                                  .(
                                    cor_HR = cor(HR, value, use = "pairwise.complete.obs"), #nuova colonna in cui applichaimo la funzione cor per calcolare la correlazione
                                    cor_SBP = cor(SBP, value, use = "pairwise.complete.obs"),
                                    n_obs = .N # Contiamo anche quante osservazioni, ci dice su quanti punti si basa la nostra correlazione.
                                  ), 
                                  by = patient_id] #dividiamo tutto per paziente
  
})
print("--- Risultato con data.table (Esercizio 6) ---")
print("Join al tempo più vicino (prime 5 righe):")
# Stampiamo le colonne chiave per vedere il join
print(joined_dt_ex6[1:5, .(patient_id, lab, time_iso, time_lag_minutes, HR, SBP, value)])
print("Correlazione CRP (prime 5 righe):")
head(summary_dt_ex6, 5)
print(time_dt_ex6)

#EX 7: Slice genomics windows efficiently. 
master_peaks_dt <- fread("data/atac_peaks.bed.csv")
master_peaks_df <- as.data.frame(master_peaks_dt)

#Approccio con data.frame
time_df_ex7 <- system.time({
  peaks_df_ex7 <- master_peaks_df
  # --- Task 1: Filtrare i picchi ---
  # Usiamo subset() per filtrare le righe in base a 3 condizioni
  filtered_df_ex7 <- subset(peaks_df_ex7, 
                            chr == "chr2" & 
                              start >= 2000000 & 
                              start <= 4000000)
  # --- Task 2: Ordinare e prendere i Top 50 ---
  #ordinaimo in ordine decrescente sulla colonna score
  ordered_df_ex7 <- filtered_df_ex7[order(filtered_df_ex7$score, decreasing = TRUE), ]
  #Prendiamo le prime 50 righe
  summary_df_ex7 <- head(ordered_df_ex7, 50)
  
})
print("--- Risultato con data.frame (Esercizio 7) ---")
print("Top 50 picchi su chr2 (prime 5 righe):")
head(summary_df_ex7, 5)
print(time_df_ex7)

#Approccio con data.table
time_dt_ex7 <- system.time({
  peaks_dt_ex7 <- copy(master_peaks_dt)
  # --- Task 1: Filtrare i picchi ---
  filtered_dt_ex7 <- peaks_dt_ex7[chr == "chr2" & 
                                    start >= 2000000 & 
                                    start <= 4000000]
  # --- Task 2: Ordinare e prendere i Top 50 ---
  setorder(filtered_dt_ex7, -score)
  summary_dt_ex7 <- head(filtered_dt_ex7, 50)
  
})

print("--- Risultato con data.table (Esercizio 7) ---")
print("Top 50 picchi su chr2 (prime 5 righe):")
head(summary_dt_ex7, 5)
print(time_dt_ex7)

#EX 8: Multi-column operations per group
#Approccio con data.frame
time_df_ex8 <- system.time({
  counts_df_ex8 <- counts_df
  metadata_df_ex8 <- metadata_df
  merged_df_ex8 <- merge(counts_df_ex8, metadata_df_ex8, by = "sample_id")
  #--- Task 1: Calcoliamo le statistiche ---
  stats_df_ex8 <- aggregate(count ~ gene + condition, data = merged_df_ex8, 
                            FUN = function(x) {
                              c(
                                mean = mean(x),
                                median = median(x),
                                Q1 = quantile(x, 0.25),
                                Q3 = quantile(x, 0.75)
                              )
                            })
  #ora abbiamo ottenuto una colonna "count" che è una matrice, dobbiamo quindi spacchettarla
  stats_unpacked_df <- do.call(data.frame, stats_df_ex8)
  #rimodelliamo da lungo a largo per avere le due medie sulla stessa riga 
  # Vogliamo 'gene' come riga e 'condition' come colonna
  wide_df_ex8 <- reshape(stats_unpacked_df, 
                         idvar = "gene",                # Le righe sono i geni
                         timevar = "condition",         # Le colonne derivano da 'condition'
                         direction = "wide")            # Vogliamo un formato largo
  # --- Task 2: Filtriamo i geni ---
  # Teniamo solo le righe dove la media 'treated' > media 'control'
  summary_df_ex8 <- subset(wide_df_ex8, 
                           count.mean.treated > count.mean.control & 
                             !is.na(count.mean.treated) & 
                             !is.na(count.mean.control))
  
})
print("--- Risultato con data.frame (Esercizio 8) ---")
print("Geni dove treated > control (prime 5 righe, solo colonne media):")
head(summary_df_ex8[, c("gene", "count.mean.treated", "count.mean.control")], 5)
print(time_df_ex8)

#Approccio con data.table
time_dt_ex8 <- system.time({
  counts_dt_ex8 <- copy(counts_dt)
  metadata_dt_ex8 <- copy(metadata_dt)
  #uniamo i dati
  setkey(counts_dt_ex8, sample_id)
  setkey(metadata_dt_ex8, sample_id)
  merged_dt_ex8 <- metadata_dt_ex8[counts_dt_ex8]
  # --- Task 1: calcoliamo le statistiche ---
  
  stats_dt_ex8 <- merged_dt_ex8[, .(
    mean_count = mean(count),
    median_count = median(count),
    q1_count = quantile(count, 0.25),
    q3_count = quantile(count, 0.75)
  ), 
  by = .(gene, condition)] #raggruppando per gene e condizione
  #Rimodelliamo (Pivot) da Lungo a Largo
  wide_dt_ex8 <- dcast(stats_dt_ex8, 
                       gene ~ condition, 
                       value.var = c("mean_count", "median_count", "q1_count", "q3_count")) #quali colonne allargare
  # --- Task 2: Filtriamo i geni ---
  summary_dt_ex8 <- wide_dt_ex8[mean_count_treated > mean_count_control]
  
})

print("--- Risultato con data.table (Esercizio 8) ---")
print("Geni dove treated > control (prime 5 righe, solo colonne media):")
head(summary_dt_ex8[, .(gene, mean_count_treated, mean_count_control)], 5)
print(time_dt_ex8)

#EX 9: Go Wide, to Long, to Wide
master_wide_dt <- fread("data/bulk_counts_wide.csv")
master_wide_df <- as.data.frame(master_wide_dt)

#Approccio con data.frame
time_df_ex9 <- system.time({
  
  wide_df_ex9 <- master_wide_df
  metadata_df_ex9 <- metadata_df
  
  # --- Task 1: Da Largo a Lungo ---
  # Otteniamo i nomi delle colonne che devono essere "fuse" (tutte tranne 'gene')
  sample_columns <- colnames(wide_df_ex9)[-1]
  long_df_ex9 <- reshape(wide_df_ex9,
                         direction = "long",             # Vogliamo un formato lungo
                         varying = sample_columns,       # Le colonne da fondere
                         v.names = "count",              # Nome della nuova colonna per i valori
                         idvar = "gene",                 # Colonna(e) da mantenere fisse
                         timevar = "sample_id",          # Nome della nuova colonna per i nomi
                         times = sample_columns)         # I valori da mettere in 'sample_id'
  # --- Task 2: Aggiungere Totali (e unire i metadati) ---
  # Aggiungiamo i totali per campione
  long_df_ex9$sample_total <- ave(long_df_ex9$count, 
                                  long_df_ex9$sample_id, 
                                  FUN = sum)
  # Uniamo i metadati
  merged_df_ex9 <- merge(long_df_ex9, metadata_df_ex9, by = "sample_id")
  
  # --- Task 3: Da Lungo a Largo (di nuovo) ---
  #Calcoliamo la media per gene E per condizione
  mean_df_ex9 <- aggregate(count ~ gene + condition, 
                           data = merged_df_ex9, 
                           FUN = mean)
  #usiamo di nuovo il reshape
  summary_df_ex9 <- reshape(mean_df_ex9,
                            direction = "wide",
                            idvar = "gene",
                            timevar = "condition",
                            v.names = "count")
  # Puliamo i nomi delle colonne (es. da "count.treated" a "treated")
  colnames(summary_df_ex9) <- gsub("count\\.", "", colnames(summary_df_ex9))
  
})

print("--- Risultato con data.frame (Esercizio 9) ---")
print("Tabella finale Gene ~ Condition (prime 5 righe):")
head(summary_df_ex9, 5)
print(time_df_ex9)

#Approccio con data.table
time_dt_ex9 <- system.time({
  wide_dt_ex9 <- copy(master_wide_dt)
  metadata_dt_ex9 <- copy(metadata_dt)
  
  # --- Task 1: Da Largo a Lungo ---
  long_dt_ex9 <- melt(wide_dt_ex9,
                      id.vars = "gene",               # Colonna da mantenere fissa
                      variable.name = "sample_id",    # Nome della nuova colonna per i nomi
                      value.name = "count")           # Nome della nuova colonna per i valori
  # --- Task 2: Aggiungere Totali (e unire i metadati) ---
  
  # Aggiungiamo i totali, raggruppando per sample_id
  long_dt_ex9[, sample_total := sum(count), by = sample_id]
  # Uniamo i metadati per ottenere la 'condition'
  setkey(long_dt_ex9, sample_id)
  setkey(metadata_dt_ex9, sample_id)
  merged_dt_ex9 <- metadata_dt_ex9[long_dt_ex9]
  # --- Task 3: Da Lungo a Largo (di nuovo) ---
  #usiamo dcast che può calcolare la media E rimodellare in un unico comando
  summary_dt_ex9 <- dcast(merged_dt_ex9,
                          gene ~ condition,         # Formula: righe ~ colonne
                          value.var = "count",      # Colonna da usare per i valori
                          fun.aggregate = mean)     # Funzione da applicare (calcola la media)
  
})

print("--- Risultato con data.table (Esercizio 9) ---")
print("Tabella finale Gene ~ Condition (prime 5 righe):")
head(summary_dt_ex9, 5)
print(time_dt_ex9)

#EX 10: ATAC-to-gene mapping
#Approccio con data.frame
master_genes_dt <- fread("data/gene_annotation.bed.csv")
master_genes_df <- as.data.frame(master_genes_dt)

time_df_ex10 <- system.time({
  
  peaks_df_ex10 <- master_peaks_df
  genes_df_ex10 <- master_genes_df
  # --- Task 1: Trovare le sovrapposizioni ---
  # facciamo un join cartesiano per cromosoma
  merged_df_ex10 <- merge(peaks_df_ex10, genes_df_ex10, by = "chr")
  #Filtriamo la tabella enorme per trovare le vere sovrapposizioni
  #Usiamo i suffissi .x (per i picchi) e .y (per i geni) che merge() crea
  overlaps_df_ex10 <- subset(merged_df_ex10, 
                             start.x < end.y & end.x > start.y)
 
  # --- Task 2: Contare i picchi per gene ---
  # Usiamo 'aggregate' con 'length' per contare
  count_df_ex10 <- aggregate(peak_id ~ gene,      #peak_id.x' (una colonna qualsiasi dei picchi) per contare 
                             data = overlaps_df_ex10, 
                             FUN = length)
  colnames(count_df_ex10) <- c("gene_id", "peak_count")
  
  # --- Task 3 & 4: Calcolare overlap e trovare i Top 20 ---
  #Calcoliamo la lunghezza della sovrapposizione per ogni riga
  overlaps_df_ex10$overlap_len <- pmin(overlaps_df_ex10$end.x, overlaps_df_ex10$end.y) - 
    pmax(overlaps_df_ex10$start.x, overlaps_df_ex10$start.y)
  
  #Sommiamo la lunghezza totale per gene
  overlap_sum_df_ex10 <- aggregate(overlap_len ~ gene, 
                                   data = overlaps_df_ex10, 
                                   FUN = sum)
  colnames(overlap_sum_df_ex10) <- c("gene_id", "total_overlap_bp")
  
  #Ordiniamo per trovare i top 20
  ordered_df_ex10 <- overlap_sum_df_ex10[order(overlap_sum_df_ex10$total_overlap_bp, 
                                               decreasing = TRUE), ]
  summary_df_ex10 <- head(ordered_df_ex10, 20)
  
})

print("--- Risultato con data.frame (Esercizio 10) ---")
print("Conteggio picchi per gene (prime 5 righe):")
head(count_df_ex10, 5)
print("Top 20 geni per sovrapposizione (prime 5 righe):")
head(summary_df_ex10, 5)
print(time_df_ex10)

#approccio con data.table
time_dt_ex10 <- system.time({
  peaks_dt_ex10 <- copy(master_peaks_dt)
  genes_dt_ex10 <- copy(master_genes_dt)
  
  # --- Task 1: Trovare le sovrapposizioni ---
  
  #Impostiamo le chiavi per il join
  setkey(peaks_dt_ex10, chr, start, end)
  setkey(genes_dt_ex10, chr, start, end)
  
  #Usiamo foverlaps()
  overlaps_dt_ex10 <- foverlaps(peaks_dt_ex10,
                                genes_dt_ex10,
                                type = "any",
                                which = FALSE)
  setnames(overlaps_dt_ex10, 
           c("start", "end", "i.start", "i.end"), 
           c("peak_start", "peak_end", "gene_start", "gene_end"))
  
  # --- Task 2: Contare i picchi per gene ---ù
  count_dt_ex10 <- overlaps_dt_ex10[, .N, by = gene]
  setnames(count_dt_ex10, "N", "peak_count")
  
  # --- Task 3 & 4: Calcolare overlap e trovare i Top 20 ---
  
  #Calcoliamo la lunghezza della sovrapposizione
  overlaps_dt_ex10[, overlap_len := pmin(peak_end, gene_end) - 
                     pmax(peak_start, gene_start)]
  #Sommiamo la lunghezza totale per gene
  overlap_sum_dt_ex10 <- overlaps_dt_ex10[, .(total_overlap_bp = sum(overlap_len)), 
                                          by = gene]
  
  #Ordiniamo per trovare i top 20
  setorder(overlap_sum_dt_ex10, -total_overlap_bp)
  summary_dt_ex10 <- head(overlap_sum_dt_ex10, 20)
  
})

print("--- Risultato con data.table (Esercizio 10) ---")
print("Conteggio picchi per gene (prime 5 righe):")
head(count_dt_ex10, 5)
print("Top 20 geni per sovrapposizione (prime 5 righe):")
head(summary_dt_ex10, 5)
print(time_dt_ex10)

#EX 11: Map SNPs to genes

master_variants_dt <- fread("data/variants.csv")
master_variants_df <- as.data.frame(master_variants_dt)

#Approccio con data.frame
#I nostri geni sono intervalli (start, end), ma le varianti sono punti singoli (pos)
#Dobbiamo "convertire" i punti delle varianti in intervalli di 1 coppia di basi (1bp)
time_df_ex11 <- system.time({
  variants_df_ex11 <- master_variants_df
  genes_df_ex11 <- master_genes_df
  
  # --- Task 1: Creare intervalli e trovare sovrapposizioni ---
  
  #Convertiamo le posizioni (pos) in intervalli (start/end) 
  variants_df_ex11$start <- variants_df_ex11$pos
  variants_df_ex11$end <- variants_df_ex11$pos
  # Rinominiamo le colonne dei geni per evitare conflitti e facciamo il join cartesiano
  colnames(genes_df_ex11) <- c("chr", "gene_start", "gene_end", "gene")
  merged_df_ex11 <- merge(variants_df_ex11, genes_df_ex11, by = "chr")
  
  # 3. Filtriamo per trovare le sovrapposizioni (variante DENTRO il gene)
  overlaps_df_ex11 <- subset(merged_df_ex11, 
                             start >= gene_start & end <= gene_end)
  # --- Task 2: Sintesi delle varianti HIGH ---
  
  #Filtriamo solo per impatto "HIGH"
  high_impact_df_ex11 <- subset(overlaps_df_ex11, impact == "HIGH")
  
  #contiamo quante varianti HIGH ha ogni gene
  summary_gene_df_ex11 <- aggregate(impact ~ gene, 
                                    data = high_impact_df_ex11, 
                                    FUN = length)
  colnames(summary_gene_df_ex11) <- c("gene", "high_impact_count")
  #contiamo quante varianti HIGH ha ogni campione
  summary_sample_df_ex11 <- aggregate(impact ~ sample_id, 
                                      data = high_impact_df_ex11, 
                                      FUN = length)
  colnames(summary_sample_df_ex11) <- c("sample_id", "high_impact_count")
  
  # --- Task 3: Geni con varianti HIGH in TUTTI i campioni ---
  
  #Troviamo il numero totale di campioni presenti nel file
  num_all_samples <- length(unique(variants_df_ex11$sample_id))
  
  #Contiamo per ogni gene, in quanti campioni unici appare
  gene_sample_count_df <- aggregate(sample_id ~ gene, 
                                    data = high_impact_df_ex11, 
                                    FUN = function(x) length(unique(x)))
  
  #Filtriamo per i geni dove il conteggio è uguale al totale
  summary_all_samples_df <- subset(gene_sample_count_df, sample_id == num_all_samples)
  colnames(summary_all_samples_df)[2] <- "unique_sample_count"
  
})

print("--- Risultato con data.frame (Esercizio 11) ---")
print("Sintesi per gene (prime 5 righe):")
head(summary_gene_df_ex11, 5)
print("Sintesi per campione (prime 5 righe):")
head(summary_sample_df_ex11, 5)
print("Geni con varianti HIGH in tutti i campioni (prime 5 righe):")
head(summary_all_samples_df, 5)
print(time_df_ex11)

#Approccio con data.table
time_dt_ex11 <- system.time({
  variants_dt_ex11 <- copy(master_variants_dt)
  genes_dt_ex11 <- copy(master_genes_dt)
  
  # --- Task 1: Creare intervalli e trovare sovrapposizioni ---
  
  # 1. Convertiamo 'pos' in intervalli start/end 
  variants_dt_ex11[, ':=' (start = pos, end = pos)]
  
  # 2. Impostiamo le chiavi per l'overlap join
  setkey(variants_dt_ex11, chr, start, end)
  setkey(genes_dt_ex11, chr, start, end)
  overlaps_dt_ex11 <- foverlaps(variants_dt_ex11, 
                                genes_dt_ex11, 
                                type = "within",  # trova varianti che sono DENTRO i geni
                                which = FALSE)    # Restituisci le colonne
  # Rinominiamo le colonne dei geni per chiarezza
  setnames(overlaps_dt_ex11, c("start", "end", "i.start", "i.end"), 
           c("variant_start", "variant_end", "gene_start", "gene_end"))
 
  # --- Task 2: Sintesi delle varianti HIGH ---
  
  #Filtriamo per impatto "HIGH"
  high_impact_dt_ex11 <- overlaps_dt_ex11[impact == "HIGH"]
  
  #contiamo le righe (.N) raggruppando per 'gene'
  summary_gene_dt_ex11 <- high_impact_dt_ex11[, .(high_impact_count = .N), by = gene]
  
  #contiamo le righe (.N) raggruppando per 'sample_id'
  summary_sample_dt_ex11 <- high_impact_dt_ex11[, .(high_impact_count = .N), by = sample_id]
  
  # --- Task 3: Geni con varianti HIGH in TUTTI i campioni ---
  
  #Troviamo il numero totale di campioni
  num_all_samples <- uniqueN(variants_dt_ex11$sample_id)
  
  #Contiamo i campioni UNICI (uniqueN) per ogni gene
  gene_sample_count_dt <- high_impact_dt_ex11[, .(unique_sample_count = uniqueN(sample_id)), 
                                              by = gene]
  
  #Filtriamo per i geni dove il conteggio è uguale al totale
  summary_all_samples_dt <- gene_sample_count_dt[unique_sample_count == num_all_samples]
  
})

print("--- Risultato con data.table (Esercizio 11) ---")
print("Sintesi per gene (prime 5 righe):")
head(summary_gene_dt_ex11, 5)
print("Sintesi per campione (prime 5 righe):")
head(summary_sample_dt_ex11, 5)
print("Geni con varianti HIGH in tutti i campioni (prime 5 righe):")
head(summary_all_samples_dt, 5)
print(time_dt_ex11)

#EX 12: Combine cohorts safely
master_cohortA_dt <- fread("data/cohortA_samples.csv")
master_cohortB_dt <- fread("data/cohortB_samples.csv")
master_cohortA_df <- as.data.frame(master_cohortA_dt)
master_cohortB_df <- as.data.frame(master_cohortB_dt)

#Approccio con data.frame

time_df_ex12 <- system.time({
  cohortA_df_ex12 <- master_cohortA_df
  cohortB_df_ex12 <- master_cohortB_df
  counts_df_ex12 <- counts_df
  
  # --- Task 1: Combinare le Coorti ---
  
  #Troviamo tutti i nomi di colonna unici da entrambe le tabelle
  all_cols <- union(names(cohortA_df_ex12), names(cohortB_df_ex12))
  
  #Aggiungiamo le colonne mancanti a Coorte A (riempite con NA)
  missing_cols_A <- setdiff(all_cols, names(cohortA_df_ex12))
  if (length(missing_cols_A) > 0) {
    cohortA_df_ex12[missing_cols_A] <- NA
  }
  
  #Aggiungiamo le colonne mancanti a Coorte B (riempite con NA)
  missing_cols_B <- setdiff(all_cols, names(cohortB_df_ex12))
  if (length(missing_cols_B) > 0) {
    cohortB_df_ex12[missing_cols_B] <- NA
  }
  combined_df_ex12 <- rbind(cohortA_df_ex12[, all_cols], cohortB_df_ex12[, all_cols])
  
  # --- Task 2: Ordinare ---
  combined_df_ex12 <- combined_df_ex12[order(combined_df_ex12$cohort, 
                                             combined_df_ex12$condition, 
                                             combined_df_ex12$sample_id), ]
 
  # --- Task 3: Analisi dei Top 100 Geni ---
  
  #Troviamo i 100 geni più variabili (usando la varianza 'var')
  gene_vars_df <- aggregate(count ~ gene, data = counts_df_ex12, FUN = var)
  gene_vars_df <- gene_vars_df[order(gene_vars_df$count, decreasing = TRUE), ]
  top_100_genes <- head(gene_vars_df$gene, 100)
  
  #Filtriamo i conteggi per tenere solo quei geni
  filtered_counts_df <- subset(counts_df_ex12, gene %in% top_100_genes)
  
  #Uniamo i conteggi filtrati con le coorti
  merged_df_ex12 <- merge(filtered_counts_df, combined_df_ex12, by = "sample_id")
  
  #Calcoliamo la media per coorte, condizione e gene
  summary_df_ex12 <- aggregate(count ~ cohort + condition + gene, 
                               data = merged_df_ex12, 
                               FUN = mean)
  
})

print("--- Risultato con data.frame (Esercizio 12) ---")
print("Media conteggi Top 100 Geni (prime 5 righe):")
head(summary_df_ex12, 5)
print(time_df_ex12)

#Approccio con data.table
time_dt_ex12 <- system.time({
  cohortA_dt_ex12 <- copy(master_cohortA_dt)
  cohortB_dt_ex12 <- copy(master_cohortB_dt)
  counts_dt_ex12 <- copy(counts_dt)
  
  # --- Task 1: Combinare le Coorti ---
  cohort_list <- list(cohortA_dt_ex12, cohortB_dt_ex12)
  combined_dt_ex12 <- rbindlist(cohort_list, use.names = TRUE, fill = TRUE)
  
  # --- Task 2: Ordinare ---
  setorder(combined_dt_ex12, cohort, condition, sample_id)
  
  # --- Task 3: Analisi dei Top 100 Geni ---
  
  #Calcoliamo la varianza
  gene_vars_dt <- counts_dt_ex12[, .(gene_var = var(count)), by = gene]
  setorder(gene_vars_dt, -gene_var, na.last = TRUE) # Gestiamo gli NA del 'var' mettendoli al fondo della lista
  top_100_genes <- head(gene_vars_dt$gene, 100)
  
  #Filtriamo i conteggi
  filtered_counts_dt <- counts_dt_ex12[gene %in% top_100_genes]
  
  # Rimuoviamo le righe dove sample_id è NA PRIMA di setkey
  combined_dt_ex12 <- combined_dt_ex12[!is.na(sample_id)]
  # Uniamo i conteggi filtrati con le coorti
  setkey(filtered_counts_dt, sample_id)
  setkey(combined_dt_ex12, sample_id)
  
  # Usiamo nomatch=0L per essere sicuri, rimuove anche da filtered_counts_dt
  # eventuali sample_id che non sono in combined_dt_ex12
  merged_dt_ex12 <- filtered_counts_dt[combined_dt_ex12, nomatch = 0L] 
  
  #Calcoliamo la media per coorte, condizione e gene
  summary_dt_ex12 <- merged_dt_ex12[, .(mean_count = mean(count)), 
                                    by = .(cohort, condition, gene)]
  
})

print("--- Risultato con data.table (Esercizio 12) ---")
head(summary_dt_ex12, 5)
print(time_dt_ex12)

# --- FINAL REVISION ---
#Approccio con data.frame
master_clusters_dt <- fread("data/annotated_GSM3516673_normal_annotated_GSM3516672_tumor_SeuratIntegration.csv")
master_cell_types_dt <- fread("data/nt_combined_clustering.output.csv")

master_clusters_df <- as.data.frame(master_clusters_dt)
master_cell_types_df <- as.data.frame(master_cell_types_dt)

# 1. Soluzione con data.frame
time_df_final <- system.time({
  
  clusters_df <- master_clusters_df
  cell_types_df <- master_cell_types_df
  
  #Passaggio di pulixia dei due file per fare sì che le cellule siano annotate allo stesso modo
  # Sostituiamo "_X_." (con il punto) con un singolo "."
  clusters_df$cell <- gsub(pattern = "_X_\\.", replacement = ".", x = clusters_df$cell)
  
  # --- Task 1: Creare e salvare il file combinato ---
  merged_df_final <- merge(clusters_df, cell_types_df, by = "cell")
  
  write.csv(merged_df_final, "final_task1_combined_df.csv", row.names = FALSE)
  
  # --- Task 2: Creare e salvare il conteggio per cluster ---
  counts_df_final <- aggregate(cell ~ integration_cluster + cell_type, 
                               data = merged_df_final, 
                               FUN = length)
  colnames(counts_df_final)[3] <- "cell_count"
  
  write.csv(counts_df_final, "final_task2_counts_per_cluster_df.csv", row.names = FALSE)
  
  # --- Task 3: Creare e salvare la tabella riassuntiva N vs T ---
  summary_long_df <- aggregate(cell ~ integration_cluster + cell_type + sample_type, 
                               data = merged_df_final, 
                               FUN = length)
  colnames(summary_long_df)[4] <- "cell_count"
  
  summary_wide_df_final <- reshape(summary_long_df,
                                   idvar = c("integration_cluster", "cell_type"),
                                   timevar = "sample_type",
                                   direction = "wide")
  
  colnames(summary_wide_df_final) <- gsub("cell_count\\.", "", colnames(summary_wide_df_final))
  summary_wide_df_final[is.na(summary_wide_df_final)] <- 0
  
  write.csv(summary_wide_df_final, "final_task3_summary_N_vs_T_df.csv", row.names = FALSE)
  
})

print("--- Risultato con data.frame (Final Revision) ---")
head(merged_df_final, 5)
print(time_df_final)

#Approccio con data.table
time_dt_final <- system.time({
  
  clusters_dt <- copy(master_clusters_dt)
  cell_types_dt <- copy(master_cell_types_dt)
  
  #Pulizia
  # Sostituiamo "_X_." con "."
  clusters_dt[, cell := gsub(pattern = "_X_\\.", replacement = ".", x = cell)]
  
  # --- Task 1: Creare e salvare il file combinato ---
  setkey(clusters_dt, cell)
  setkey(cell_types_dt, cell)
  merged_dt_final <- clusters_dt[cell_types_dt, nomatch = 0L]  # nomatch=0L tiene solo le cellule in comune
  
  fwrite(merged_dt_final, "final_task1_combined_dt.csv")
  
  # --- Task 2: Creare e salvare il conteggio per cluster ---
  counts_dt_final <- merged_dt_final[, .(cell_count = .N), 
                                     by = .(integration_cluster, cell_type)]
  
  fwrite(counts_dt_final, "final_task2_counts_per_cluster_dt.csv")
  
  # --- Task 3: Creare e salvare la tabella riassuntiva N vs T ---
  summary_wide_dt_final <- dcast(merged_dt_final,                           # Usiamo dcast() per contare e rimodellare in un unico comando
                                 integration_cluster + cell_type ~ sample_type,           # righe ~ colonne
                                 fun.aggregate = length,       #calcola il conteggio
                                 value.var = "cell",
                                 fill = 0)     #riempie gli NA con 0
  
  fwrite(summary_wide_dt_final, "final_task3_summary_N_vs_T_dt.csv")
  
})

print("--- Risultato con data.table (Final Revision) ---")
head(merged_dt_final, 5)
print(time_dt_final)
# --- Task 4 & 5  ---
# 1. Creiamo la tabella "lunga" di conteggi (simile al Task 3 di data.frame)
summary_long_dt <- merged_dt_final[, .(cell_count = .N),
                                   by = .(integration_cluster, cell_type, sample_type)]

# 2. Calcoliamo la percentuale "per riferimento" (:=)
#    La sintassi 'by=.(...)' calcola sum(cell_count) all'interno di ogni
#    gruppo (cluster, sample_type) e usa quel totale per calcolare la %
#    Questa tabella ci serve come punto di partenza per il plot e le percentuali
summary_long_dt[, percentage := (cell_count / sum(cell_count)) * 100, 
                by = .(integration_cluster, sample_type)]

print("Tabella con % Normalizzata (data.table):")
head(summary_long_dt, 5)
final_plot_dt <- ggplot(summary_long_dt, 
                        aes(x = integration_cluster, y = percentage, fill = cell_type)) +    #definisce gli assi
  geom_bar(stat = "identity", position = "stack") +                    ## "stat='identity'" usa il valore 'y' (percentage) come altezza, "position='stack'" impila le barre (i tipi di cellule)
  facet_wrap(~ sample_type) +
  labs(title = "Distribuzione Tipi di Cellule nei Cluster (Normal vs Tumor)",
       x = "Cluster di Integrazione",
       y = "Percentuale del Cluster (%)",
       fill = "Tipo di Cellula") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Stampiamo il grafico
print(final_plot_dt)

# Salviamo il grafico
ggsave("final_plot_dt.png", final_plot_dt, width = 10, height = 6)
