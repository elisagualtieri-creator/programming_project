# -------------------------------------------------------------------
# TEST SCRIPT FOR RUNNING SINGLE FUNCTIONS ("The Laboratory")
# -------------------------------------------------------------------
# This script loads everything necessary (libraries, data, functions)
# and then runs ONLY the exercise you choose in the test area.
#
# INSTRUCTIONS:
# 1. Scroll to the bottom, to the "TEST AREA".
# 2. Find the block for the exercise you want to test (e.g., "Exercise 5 (data.table)").
# 3. Select the lines in that block.
# 4. Press Ctrl+Shift+C (or Cmd+Shift+C on Mac) to uncomment them.
# 5. Run the entire script (e.g., with the "Source" button in RStudio).
# -------------------------------------------------------------------
# --- 1. LOAD LIBRARIES ---
library(data.table)
library(ggplot2)

# --- 2. UPLOAD FUNCTIONS ---
print("Caricamento file delle funzioni...")
#The file must be in the same folder
source("analysis_functions.R")

# --- 3. UPLOADING DATA ---
counts_dt <- fread("data/bulk_counts_long.csv")
metadata_dt <- fread("data/sample_metadata.csv")
master_labs_dt <- fread("data/clinical_labs.csv")
master_ranges_dt <- fread("data/lab_reference_ranges.csv")
master_vitals_dt <- fread("data/vitals_time_series.csv")
master_peaks_dt <- fread("data/atac_peaks.bed.csv")
master_wide_dt <- fread("data/bulk_counts_wide.csv")
master_genes_dt <- fread("data/gene_annotation.bed.csv")
master_variants_dt <- fread("data/variants.csv")
master_cohortA_dt <- fread("data/cohortA_samples.csv")
master_cohortB_dt <- fread("data/cohortB_samples.csv")
master_clusters_dt <- fread("data/annotated_GSM3516673_normal_annotated_GSM3516672_tumor_SeuratIntegration.csv")
master_cell_types_dt <- fread("data/nt_combined_clustering.output.csv")
# --- SPECIAL CORRECTIONS ON DATA ---
master_labs_dt[, time_iso := as.POSIXct(time_iso)]
master_vitals_dt[, time_iso := as.POSIXct(time_iso)]
master_clusters_dt[, cell := gsub(pattern = "_X_\\.", replacement = ".", x = cell)]
try(setnames(master_ranges_dt, c("lab_id", "lower_bound", "upper_bound"), c("lab", "lower", "upper"), skip_absent = TRUE), silent = TRUE)
master_ranges_simple_dt <- master_ranges_dt[, .(lower = mean(lower), upper = mean(upper)), by = lab]
# --- DATA.FRAME COPIES ---
counts_df <- as.data.frame(counts_dt)
metadata_df <- as.data.frame(metadata_dt)
master_labs_df <- as.data.frame(master_labs_dt)
master_ranges_df <- as.data.frame(master_ranges_dt)
master_vitals_df <- as.data.frame(master_vitals_dt)
master_peaks_df <- as.data.frame(master_peaks_dt)
master_wide_df <- as.data.frame(master_wide_dt)
master_genes_df <- as.data.frame(master_genes_dt)
master_variants_df <- as.data.frame(master_variants_dt)
master_cohortA_df <- as.data.frame(master_cohortA_dt)
master_cohortB_df <- as.data.frame(master_cohortB_dt)
master_clusters_df <- as.data.frame(master_clusters_dt)
master_cell_types_df <- as.data.frame(master_cell_types_dt)
master_ranges_simple_df <- as.data.frame(master_ranges_simple_dt)

# -------------------------------------------------------------------
# --- 4. AREA DI TEST: ESEGUI UN ESERCIZIO ---
# -------------------------------------------------------------------
# De-commenta (Ctrl+Shift+C) il blocco che vuoi eseguire.
# Assicurati che solo UN blocco sia attivo alla volta.
# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 1 (data.frame)")
# ris <- esegui_ex1_df(counts_df, metadata_df)
# print(head(ris$summary, 3))
# print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 1 (data.table)")
# ris <- esegui_ex1_dt(counts_dt, metadata_dt)
# print(head(ris$summary, 3))
# print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 2 (data.frame)")
# ris <- esegui_ex2_df(as.data.frame(counts_dt)
# print(head(ris$summary, 3))
# print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 2 (data.table)")
# ris <- esegui_ex2_dt(copy(counts_dt))
# print(head(ris$summary, 3))
# print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 3 (data.frame)")
# ris <- esegui_ex3_df(counts_df, metadata_df)
# print("Tempo Join:"); print(ris$time_join)
# print("Tempo Search:"); print(ris$time_search)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 3 (data.table)")
# ris <- esegui_ex3_dt(counts_dt, metadata_dt)
# print("Tempo Join:"); print(ris$time_join)
# print("Tempo Search (Prima):"); print(ris$time_search_before)
# print("Tempo Search (Dopo):"); print(ris$time_search_after)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 4 (data.frame)")
# ris <- esegui_ex4_df(counts_df, metadata_df)
# print("Task 1 (Somma Paziente):"); print(head(ris$summary_a, 3))
# print("Task 2 (Top 10):"); print(head(ris$summary_b, 3))
# print("Tempo Task 1:"); print(ris$time_a)
# print("Tempo Task 2:"); print(ris$time_b)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 4 (data.table)")
# ris <- esegui_ex4_dt(counts_dt, metadata_dt)
# print("Task 1 (Somma Paziente):"); print(head(ris$summary_a, 3))
# print("Task 2 (Top 10):"); print(head(ris$summary_b, 3))
# print("Tempo Task 1:"); print(ris$time_a)
# print("Tempo Task 2:"); print(ris$time_b)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 5 (data.frame)")
# ris <- esegui_ex5_df(master_labs_df, master_ranges_simple_df)
# print("Riepilogo Paziente:"); print(head(ris$summary_patient, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 5 (data.table)")
# ris <- esegui_ex5_dt(master_labs_dt, master_ranges_simple_dt)
# print("Riepilogo Paziente:"); print(head(ris$summary_patient, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 6 (data.frame)")
# ris <- esegui_ex6_df(master_labs_df, master_vitals_df)
# print("Riepilogo Correlazione:"); print(head(ris$summary, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 6 (data.table)")
# ris <- esegui_ex6_dt(master_labs_dt, master_vitals_dt)
# print("Riepilogo Correlazione:"); print(head(ris$summary, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 7 (data.frame)")
# ris <- esegui_ex7_df(master_peaks_df)
# print("Riepilogo Top 50:"); print(head(ris$summary, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 8 (data.frame)")
# ris <- esegui_ex8_df(counts_df, metadata_df)
# print("Riepilogo Geni Filtrati:"); print(head(ris$summary, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 8 (data.table)")
# ris <- esegui_ex8_dt(master_counts_dt, metadata_dt)
# print("Riepilogo Geni Filtrati (dove treated > control):")
# print(head(ris$summary, 3))
# print("Tempo:")
# print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 9 (data.table)")
# ris <- esegui_ex9_dt(master_wide_dt, metadata_dt)
# print("Riepilogo Gene vs Condizione:"); print(head(ris$summary, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 10 (data.frame)")
# ris <- esegui_ex10_df(master_peaks_df, master_genes_df)
# print("Riepilogo Top 20 Geni:"); print(head(ris$summary_top20, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 10 (data.table)")
# ris <- esegui_ex10_dt(master_peaks_dt, master_genes_dt)
# print("Riepilogo Top 20 Geni:"); print(head(ris$summary_top20, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 11 (data.frame)")
# ris <- esegui_ex11_df(master_variants_df, master_genes_df)
# print("Riepilogo Geni HIGH:"); print(head(ris$summary_gene, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 11 (data.table)")
# ris <- esegui_ex11_dt(master_variants_dt, master_genes_dt)
# print("Riepilogo Geni HIGH:"); print(head(ris$summary_gene, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 12 (data.frame)")
# ris <- esegui_ex12_df(master_cohortA_df, master_cohortB_df, counts_df)
# print("Riepilogo Medie Top 100:"); print(head(ris$summary, 3))
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Esercizio 12 (data.table)")
# ris <- esegui_ex12_dt(master_cohortA_dt, master_cohortB_dt, counts_dt)
# print("Riepilogo Medie Top 100:"); print(head(ris$summary, 3))
# # print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Final Revision (data.frame)")
# ris <- esegui_final_df(master_clusters_df, master_cell_types_df)
# print("Tabella N vs T:"); print(head(ris$summary_wide, 3))
# print("Tabella Normalizzata:"); print(head(ris$summary_norm, 3))
# print("--- Grafico Verrà Generato nel Pannello Plots ---")
# print(ris$plot)
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------

# print(">>> TEST: Final Revision (data.table)")
# ris <- esegui_final_dt(master_clusters_dt, master_cell_types_dt)
# print("Tabella N vs T:"); print(head(ris$summary_wide, 3))
# print("Tabella Normalizzata:"); print(head(ris$summary_norm, 3))
# print("--- Grafico Verrà Generato nel Pannello Plots ---")
# print(ris$plot)
# print("Tempo:"); print(ris$time)

# -------------------------------------------------------------------
# --- END TEST AREA ---
# -------------------------------------------------------------------