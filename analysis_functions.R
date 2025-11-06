
# --- Esercizio 1: Filtrare, Riassumere e Raggruppare ---

esegui_ex1_df <- function(counts_df, metadata_df) {
  
  time_taken <- system.time({
    merged_df <- merge(counts_df, metadata_df, by = "sample_id")
    filtered_df <- subset(merged_df, condition == "treated" & grepl("^GENE_00", gene))
    
    summary_df_ex1 <- aggregate(count ~ gene, data = filtered_df, FUN = function(x) {
      c(mean_count = mean(x), median_count = median(x))
    })
  })
  
  return(list(summary = summary_df_ex1, time = time_taken))
}

esegui_ex1_dt <- function(counts_dt, metadata_dt) {
  
  time_taken <- system.time({
    counts_copy <- copy(counts_dt)
    metadata_copy <- copy(metadata_dt)
    
    setkey(metadata_copy, sample_id)
    setkey(counts_copy, sample_id)
    merged_dt <- metadata_copy[counts_copy]
    
    summary_dt_ex1 <- merged_dt[condition == "treated" & grepl("^GENE_00", gene),
                                .(mean_count = mean(count), median_count = median(count)),
                                by = gene]
  })
  
  return(list(summary = summary_dt_ex1, time = time_taken))
}
results_ex1_df <- esegui_ex1_df(counts_df,metadata_df)
# --- Esercizio 2: Aggiungere Colonne QC Derivate ---

esegui_ex2_df <- function(counts_df) {
  
  time_taken <- system.time({
    counts_df$log2_count <- log2(counts_df$count + 1)
    counts_df$high <- counts_df$count > 100
    median_by_gene_df <- aggregate(count ~ gene, data = counts_df, FUN = median)
    colnames(median_by_gene_df) <- c("gene", "median_count")
    counts_with_median_df <- merge(counts_df, median_by_gene_df, by = "gene")
    counts_with_median_df$high <- counts_with_median_df$count > counts_with_median_df$median_count
    })
    
    # Restituiamo la tabella finale e il tempo
    return(list(summary = counts_with_median_df, time = time_taken))
}

esegui_ex2_dt <- function(counts_dt) {
  
  time_taken <- system.time({
    counts_dt[, ':=' (log2_count = log2(count + 1),
                      high = count > 100)]
    counts_dt[, high := count > median(count), by = gene]
  })
  
  return(list(summary = counts_dt, time = time_taken))
}

# --- Esercizio 3: Velocizzare Join e Ricerche ---

esegui_ex3_df <- function(counts_df, metadata_df) {
  # Task 3.1: Join
  time_join <- system.time({
    merged_df_ex3 <- merge(counts_df, metadata_df, by = "sample_id")
  })
  
  # Task 3.2: Ricerca
  un_gene_reale <- "GENE_0356" # Valore di test
  un_campione_reale <- "S01"  # Valore di test
  time_search <- system.time({
    subset_df_ex3 <- subset(counts_df, gene == un_gene_reale & sample_id == un_campione_reale)
  })
  
  return(list(time_join = time_join, time_search = time_search))
}
# --- Esercizio 3: Velocizzare Join e Ricerche ---

esegui_ex3_df <- function(counts_df, metadata_df) {
  # Task 3.1: Join
  time_join <- system.time({
    merged_df_ex3 <- merge(counts_df, metadata_df, by = "sample_id")
  })
  
  # Task 3.2: Ricerca
  un_gene_reale <- "GENE_0356" # Valore di test
  un_campione_reale <- "S01"  # Valore di test
  time_search <- system.time({
    subset_df_ex3 <- subset(counts_df, gene == un_gene_reale & sample_id == un_campione_reale)
  })
  
  return(list(time_join = time_join, time_search = time_search))
}

esegui_ex3_dt <- function(counts_dt, metadata_dt) {
  # Task 3.1: Join
  time_join <- system.time({
    counts_copy <- copy(counts_dt)
    metadata_copy <- copy(metadata_dt)
    setkey(counts_copy, sample_id)
    setkey(metadata_copy, sample_id)
    merged_dt_ex3 <- metadata_copy[counts_copy]
  })
  
  # Task 3.2: Ricerca
  un_gene_reale <- "GENE_0356"
  un_campione_reale <- "S01"
  counts_copy <- copy(counts_dt) # Nuova copia per il test
  
  time_search_before <- system.time({
    subset_dt_before <- counts_copy[gene == un_gene_reale & sample_id == un_campione_reale]
  })
  
  setindex(counts_copy, gene, sample_id) # Aggiungiamo l'indice
  
  time_search_after <- system.time({
    subset_dt_after <- counts_copy[gene == un_gene_reale & sample_id == un_campione_reale]
  })
  
  return(list(time_join = time_join, 
              time_search_before = time_search_before,
              time_search_after = time_search_after))
}

# --- Esercizio 4: Annotare Conteggi ---

esegui_ex4_df <- function(counts_df, metadata_df) {
  # Task 4.1 e 4.2
  time_taken_a <- system.time({
    merged_df_ex4 <- merge(counts_df, metadata_df, by = "sample_id")
    summary_df_ex4a <- aggregate(count ~ patient_id, data = merged_df_ex4, FUN = sum)
  })
  
  time_taken_b <- system.time({
    merged_df_ex4 <- merge(counts_df, metadata_df, by = "sample_id") # Rieseguiamo il merge per un test equo
    avg_df <- aggregate(count ~ gene + condition, data = merged_df_ex4, FUN = mean)
    split_df <- split(avg_df, avg_df$condition)
    top10_list <- lapply(split_df, function(x) {
      x_ordered <- x[order(x$count, decreasing = TRUE), ]
      head(x_ordered, 10)
    })
    summary_df_ex4b <- do.call(rbind, top10_list)
  })
  
  return(list(summary_a = summary_df_ex4a, summary_b = summary_df_ex4b, 
              time_a = time_taken_a, time_b = time_taken_b))
}

esegui_ex4_dt <- function(counts_dt, metadata_dt) {
  # Task 4.1
  time_taken_a <- system.time({
    counts_copy <- copy(counts_dt)
    metadata_copy <- copy(metadata_dt)
    setkey(counts_copy, sample_id)
    setkey(metadata_copy, sample_id)
    merged_dt_ex4 <- metadata_copy[counts_copy]
    summary_dt_ex4a <- merged_dt_ex4[, .(total_count = sum(count)), by = patient_id]
  })
  
  # Task 4.2
  time_taken_b <- system.time({
    counts_copy <- copy(counts_dt)
    metadata_copy <- copy(metadata_dt)
    setkey(counts_copy, sample_id)
    setkey(metadata_copy, sample_id)
    merged_dt_ex4 <- metadata_copy[counts_copy]
    avg_dt <- merged_dt_ex4[, .(avg_count = mean(count)), by = .(condition, gene)]
    summary_dt_ex4b <- avg_dt[order(-avg_count), head(.SD, 10), by = condition]
  })
  
  return(list(summary_a = summary_dt_ex4a, summary_b = summary_dt_ex4b, 
              time_a = time_taken_a, time_b = time_taken_b))
}

# --- Esercizio 5: Classificare Valori in Intervalli ---

esegui_ex5_df <- function(labs_df, ranges_simple_df) {
  time_taken <- system.time({
    merged_df_ex5 <- merge(labs_df, ranges_simple_df, by = "lab")
    merged_df_ex5$status <- "out_of_range"
    is_normal <- merged_df_ex5$value >= merged_df_ex5$lower & 
      merged_df_ex5$value <= merged_df_ex5$upper
    merged_df_ex5$status[is_normal] <- "normal"
    
    abnormal_df_ex5 <- subset(merged_df_ex5, status == "out_of_range")
    summary_patient_df_ex5 <- aggregate(value ~ patient_id, data = abnormal_df_ex5, FUN = length)
    summary_lab_df_ex5 <- aggregate(value ~ lab, data = abnormal_df_ex5, FUN = length)
  })
  
  return(list(summary_patient = summary_patient_df_ex5, summary_lab = summary_lab_df_ex5, time = time_taken))
}

esegui_ex5_dt <- function(labs_dt, ranges_simple_dt) {
  time_taken <- system.time({
    labs_copy <- copy(labs_dt)
    ranges_copy <- copy(ranges_simple_dt)
    
    setkey(labs_copy, lab)
    setkey(ranges_copy, lab)
    labeled_dt_ex5 <- ranges_copy[labs_copy]
    
    labeled_dt_ex5[, status := ifelse(value >= lower & value <= upper, "normal", "out_of_range")]
    
    abnormal_dt_ex5 <- labeled_dt_ex5[status == "out_of_range"]
    summary_patient_dt_ex5 <- abnormal_dt_ex5[, .(abnormal_count = .N), by = patient_id]
    summary_lab_dt_ex5 <- abnormal_dt_ex5[, .(abnormal_count = .N), by = lab]
  })
  
  return(list(summary_patient = summary_patient_dt_ex5, summary_lab = summary_lab_dt_ex5, time = time_taken))
}

# --- Esercizio 6: Abbinamento al Tempo più Vicino ---

esegui_ex6_dt <- function(labs_dt, vitals_dt) {
  time_taken <- system.time({
    labs_copy <- copy(labs_dt)
    vitals_copy <- copy(vitals_dt)
    
    vitals_wide_dt_ex6 <- dcast(vitals_copy, 
                                patient_id + time_iso ~ vital, 
                                value.var = "value")
    
    labs_copy[, lab_time := time_iso]
    
    setkey(vitals_wide_dt_ex6, patient_id, time_iso)
    setkey(labs_copy, patient_id, time_iso)
    
    joined_dt_ex6 <- vitals_wide_dt_ex6[labs_copy, roll = "nearest"]
    
    summary_dt_ex6 <- joined_dt_ex6[lab == "CRP", 
                                    .(
                                      cor_HR = cor(HR, value, use = "pairwise.complete.obs"),
                                      cor_SBP = cor(SBP, value, use = "pairwise.complete.obs"),
                                      n_obs = .N 
                                    ), 
                                    by = patient_id]
  })
  
  return(list(summary = summary_dt_ex6, time = time_taken))
}

# --- Esercizio 7: Filtrare Finestre Genomiche ---

esegui_ex7_df <- function(peaks_df) {
  time_taken <- system.time({
    filtered_df_ex7 <- subset(peaks_df, 
                              chr == "chr2" & 
                                start >= 2000000 & 
                                start <= 4000000)
    ordered_df_ex7 <- filtered_df_ex7[order(filtered_df_ex7$score, decreasing = TRUE), ]
    summary_df_ex7 <- head(ordered_df_ex7, 50)
  })
  
  return(list(summary = summary_df_ex7, time = time_taken))
}

esegui_ex7_dt <- function(peaks_dt) {
  time_taken <- system.time({
    peaks_copy <- copy(peaks_dt)
    filtered_dt_ex7 <- peaks_copy[chr == "chr2" & 
                                    start >= 2000000 & 
                                    start <= 4000000]
    setorder(filtered_dt_ex7, -score)
    summary_dt_ex7 <- head(filtered_dt_ex7, 50)
  })
  
  return(list(summary = summary_dt_ex7, time = time_taken))
}

# --- Esercizio 8: Operazioni Multi-Colonna per Gruppo ---

esegui_ex8_df <- function(counts_df, metadata_df) {
  time_taken <- system.time({
    merged_df_ex8 <- merge(counts_df, metadata_df, by = "sample_id")
    
    stats_df_ex8 <- aggregate(count ~ gene + condition, data = merged_df_ex8, 
                              FUN = function(x) {
                                c(mean = mean(x), median = median(x),
                                  Q1 = quantile(x, 0.25), Q3 = quantile(x, 0.75))
                              })
    stats_unpacked_df <- do.call(data.frame, stats_df_ex8)
    
    wide_df_ex8 <- reshape(stats_unpacked_df, 
                           idvar = "gene", timevar = "condition", direction = "wide")
    
    summary_df_ex8 <- subset(wide_df_ex8, 
                             count.mean.treated > count.mean.control & 
                               !is.na(count.mean.treated) & 
                               !is.na(count.mean.control))
  })
  
  return(list(summary = summary_df_ex8, time = time_taken))
}

esegui_ex8_dt <- function(counts_dt, metadata_dt) {
  time_taken <- system.time({
    counts_copy <- copy(counts_dt)
    metadata_copy <- copy(metadata_dt)
    
    setkey(counts_copy, sample_id)
    setkey(metadata_copy, sample_id)
    merged_dt_ex8 <- metadata_copy[counts_copy]
    
    stats_dt_ex8 <- merged_dt_ex8[, .(
      mean_count = mean(count),
      median_count = median(count),
      q1_count = quantile(count, 0.25),
      q3_count = quantile(count, 0.75)
    ), 
    by = .(gene, condition)]
    
    wide_dt_ex8 <- dcast(stats_dt_ex8, 
                         gene ~ condition, 
                         value.var = c("mean_count", "median_count", "q1_count", "q3_count"))
    
    summary_dt_ex8 <- wide_dt_ex8[mean_count_treated > mean_count_control]
  })
  
  return(list(summary = summary_dt_ex8, time = time_taken))
}

# --- Esercizio 9: Da Largo a Lungo e Ritorno ---

esegui_ex9_df <- function(wide_df, metadata_df) {
  time_taken <- system.time({
    sample_columns <- colnames(wide_df)[-1] 
    long_df_ex9 <- reshape(wide_df,
                           direction = "long", varying = sample_columns, v.names = "count",
                           idvar = "gene", timevar = "sample_id", times = sample_columns)
    
    long_df_ex9$sample_total <- ave(long_df_ex9$count, 
                                    long_df_ex9$sample_id, 
                                    FUN = sum)
    merged_df_ex9 <- merge(long_df_ex9, metadata_df, by = "sample_id")
    
    mean_df_ex9 <- aggregate(count ~ gene + condition, data = merged_df_ex9, FUN = mean)
    summary_df_ex9 <- reshape(mean_df_ex9,
                              direction = "wide", idvar = "gene",
                              timevar = "condition", v.names = "count")
    colnames(summary_df_ex9) <- gsub("count\\.", "", colnames(summary_df_ex9))
  })
  
  return(list(summary = summary_df_ex9, time = time_taken))
}

esegui_ex9_dt <- function(wide_dt, metadata_dt) {
  time_taken <- system.time({
    wide_copy <- copy(wide_dt)
    metadata_copy <- copy(metadata_dt)
    
    long_dt_ex9 <- melt(wide_copy,
                        id.vars = "gene", variable.name = "sample_id", value.name = "count")
    
    setkey(long_dt_ex9, sample_id)
    setkey(metadata_copy, sample_id)
    merged_dt_ex9 <- metadata_copy[long_dt_ex9]
    
    summary_dt_ex9 <- dcast(merged_dt_ex9,
                            gene ~ condition,
                            value.var = "count",
                            fun.aggregate = mean)
  })
  
  return(list(summary = summary_dt_ex9, time = time_taken))
}

# --- Esercizio 10: Mappatura ATAC-Gene ---

esegui_ex10_df <- function(peaks_df, genes_df) {
  time_taken <- system.time({
    merged_df_ex10 <- merge(peaks_df, genes_df, by = "chr")
    overlaps_df_ex10 <- subset(merged_df_ex10, start.x < end.y & end.x > start.y)
    
    count_df_ex10 <- aggregate(peak_id ~ gene, data = overlaps_df_ex10, FUN = length)
    
    overlaps_df_ex10$overlap_len <- pmin(overlaps_df_ex10$end.x, overlaps_df_ex10$end.y) - 
      pmax(overlaps_df_ex10$start.x, overlaps_df_ex10$start.y)
    overlap_sum_df_ex10 <- aggregate(overlap_len ~ gene, data = overlaps_df_ex10, FUN = sum)
    
    ordered_df_ex10 <- overlap_sum_df_ex10[order(overlap_sum_df_ex10$total_overlap_bp, decreasing = TRUE), ]
    summary_df_ex10 <- head(ordered_df_ex10, 20)
  })
  
  return(list(summary_top20 = summary_df_ex10, summary_counts = count_df_ex10, time = time_taken))
}

esegui_ex10_dt <- function(peaks_dt, genes_dt) {
  time_taken <- system.time({
    peaks_copy <- copy(peaks_dt)
    genes_copy <- copy(genes_dt)
    
    setkey(peaks_copy, chr, start, end)
    setkey(genes_copy, chr, start, end)
    overlaps_dt_ex10 <- foverlaps(peaks_copy, genes_copy, type = "any", which = FALSE)
    
    setnames(overlaps_dt_ex10, c("start", "end", "i.start", "i.end"), 
             c("peak_start", "peak_end", "gene_start", "gene_end"))
    
    count_dt_ex10 <- overlaps_dt_ex10[, .N, by = gene]
    setnames(count_dt_ex10, "N", "peak_count")
    
    overlaps_dt_ex10[, overlap_len := pmin(peak_end, gene_end) - pmax(peak_start, gene_start)]
    overlap_sum_dt_ex10 <- overlaps_dt_ex10[, .(total_overlap_bp = sum(overlap_len)), by = gene]
    
    setorder(overlap_sum_dt_ex10, -total_overlap_bp)
    summary_dt_ex10 <- head(overlap_sum_dt_ex10, 20)
  })
  
  return(list(summary_top20 = summary_dt_ex10, summary_counts = count_dt_ex10, time = time_taken))
}

# --- Esercizio 11: Mappatura SNP ai Geni ---

esegui_ex11_df <- function(variants_df, genes_df) {
  time_taken <- system.time({
    variants_df$start <- variants_df$pos
    variants_df$end <- variants_df$pos
    
    genes_df_renamed <- genes_df
    colnames(genes_df_renamed) <- c("chr", "gene_start", "gene_end", "gene")
    
    merged_df_ex11 <- merge(variants_df, genes_df_renamed, by = "chr")
    overlaps_df_ex11 <- subset(merged_df_ex11, start >= gene_start & end <= gene_end)
    
    high_impact_df_ex11 <- subset(overlaps_df_ex11, impact == "HIGH")
    summary_gene_df_ex11 <- aggregate(impact ~ gene, data = high_impact_df_ex11, FUN = length)
    summary_sample_df_ex11 <- aggregate(impact ~ sample_id, data = high_impact_df_ex11, FUN = length)
    
    num_all_samples <- length(unique(variants_df$sample_id))
    gene_sample_count_df <- aggregate(sample_id ~ gene, 
                                      data = high_impact_df_ex11, 
                                      FUN = function(x) length(unique(x)))
    summary_all_samples_df <- subset(gene_sample_count_df, sample_id == num_all_samples)
  })
  
  return(list(summary_gene = summary_gene_df_ex11, 
              summary_sample = summary_sample_df_ex11, 
              summary_all = summary_all_samples_df, 
              time = time_taken))
}

esegui_ex11_dt <- function(variants_dt, genes_dt) {
  time_taken <- system.time({
    variants_copy <- copy(variants_dt)
    genes_copy <- copy(genes_dt)
    
    variants_copy[, ':=' (start = pos, end = pos)]
    setkey(variants_copy, chr, start, end)
    setkey(genes_copy, chr, start, end)
    overlaps_dt_ex11 <- foverlaps(variants_copy, genes_copy, type = "within", which = FALSE)
    
    high_impact_dt_ex11 <- overlaps_dt_ex11[impact == "HIGH"]
    summary_gene_dt_ex11 <- high_impact_dt_ex11[, .(high_impact_count = .N), by = gene]
    summary_sample_dt_ex11 <- high_impact_dt_ex11[, .(high_impact_count = .N), by = sample_id]
    
    num_all_samples <- uniqueN(variants_copy$sample_id)
    gene_sample_count_dt <- high_impact_dt_ex11[, .(unique_sample_count = uniqueN(sample_id)), by = gene]
    summary_all_samples_dt <- gene_sample_count_dt[unique_sample_count == num_all_samples]
  })
  
  return(list(summary_gene = summary_gene_dt_ex11, 
              summary_sample = summary_sample_dt_ex11, 
              summary_all = summary_all_samples_dt, 
              time = time_taken))
}

# --- Esercizio 12: Combinare Coorti ---

esegui_ex12_df <- function(cohortA_df, cohortB_df, counts_df) {
  time_taken <- system.time({
    all_cols <- union(names(cohortA_df), names(cohortB_df))
    missing_cols_A <- setdiff(all_cols, names(cohortA_df)); if (length(missing_cols_A) > 0) cohortA_df[missing_cols_A] <- NA
    missing_cols_B <- setdiff(all_cols, names(cohortB_df)); if (length(missing_cols_B) > 0) cohortB_df[missing_cols_B] <- NA
    combined_df_ex12 <- rbind(cohortA_df[, all_cols], cohortB_df[, all_cols])
    combined_df_ex12 <- combined_df_ex12[order(combined_df_ex12$cohort, 
                                               combined_df_ex12$condition, 
                                               combined_df_ex12$sample_id), ]
    
    gene_vars_df <- aggregate(count ~ gene, data = counts_df, FUN = var)
    gene_vars_df <- gene_vars_df[order(gene_vars_df$count, decreasing = TRUE, na.last = TRUE), ]
    top_100_genes <- head(gene_vars_df$gene, 100)
    filtered_counts_df <- subset(counts_df, gene %in% top_100_genes)
    merged_df_ex12 <- merge(filtered_counts_df, combined_df_ex12, by = "sample_id")
    summary_df_ex12 <- aggregate(count ~ cohort + condition + gene, 
                                 data = merged_df_ex12, FUN = mean)
  })
  
  return(list(summary = summary_df_ex12, time = time_taken))
}

esegui_ex12_dt <- function(cohortA_dt, cohortB_dt, counts_dt) {
  time_taken <- system.time({
    cohortA_copy <- copy(cohortA_dt)
    cohortB_copy <- copy(cohortB_dt)
    counts_copy <- copy(counts_dt)
    
    cohort_list <- list(cohortA_copy, cohortB_copy)
    combined_dt_ex12 <- rbindlist(cohort_list, use.names = TRUE, fill = TRUE)
    
    setorder(combined_dt_ex12, cohort, condition, sample_id)
    
    gene_vars_dt <- counts_copy[, .(gene_var = var(count)), by = gene]
    setorder(gene_vars_dt, -gene_var, na.last = TRUE)
    top_100_genes <- head(gene_vars_dt$gene, 100)
    filtered_counts_dt <- counts_copy[gene %in% top_100_genes]
    
    combined_dt_ex12 <- combined_dt_ex12[!is.na(sample_id)]
    setkey(filtered_counts_dt, sample_id)
    setkey(combined_dt_ex12, sample_id)
    merged_dt_ex12 <- filtered_counts_dt[combined_dt_ex12, nomatch = 0L]
    summary_dt_ex12 <- merged_dt_ex12[, .(mean_count = mean(count)), 
                                      by = .(cohort, condition, gene)]
  })
  
  return(list(summary = summary_dt_ex12, time = time_taken))
}
# --- Final Revision: Associazione Tipi di Cellule ---

esegui_final_df <- function(clusters_df, cell_types_df) {
  time_taken <- system.time({
    clusters_df$cell <- gsub(pattern = "_X_\\.", replacement = ".", x = clusters_df$cell)
    
    merged_df_final <- merge(clusters_df, cell_types_df, by = "cell")
    
    counts_df_final <- aggregate(cell ~ integration_cluster + cell_type, 
                                 data = merged_df_final, FUN = length)
    
    summary_long_df <- aggregate(cell ~ integration_cluster + cell_type + sample_type, 
                                 data = merged_df_final, FUN = length)
    colnames(summary_long_df)[4] <- "cell_count"
    
    summary_wide_df_final <- reshape(summary_long_df,
                                     idvar = c("integration_cluster", "cell_type"),
                                     timevar = "sample_type",
                                     direction = "wide")
    colnames(summary_wide_df_final) <- gsub("cell_count\\.", "", colnames(summary_wide_df_final))
    summary_wide_df_final[is.na(summary_wide_df_final)] <- 0
    
    # Task 4 & 5 (Plot e Percentuali)
    summary_long_df$group_total <- ave(summary_long_df$cell_count, 
                                       summary_long_df$integration_cluster, 
                                       summary_long_df$sample_type, 
                                       FUN = sum)
    summary_long_df$percentage <- (summary_long_df$cell_count / summary_long_df$group_total) * 100
    
    final_plot_df <- ggplot(summary_long_df, 
                            aes(x = integration_cluster, y = percentage, fill = cell_type)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ sample_type) +
      labs(title = "Distribuzione Tipi di Cellule (Normal vs Tumor)",
           x = "Cluster", y = "Percentuale del Cluster (%)", fill = "Tipo di Cellula") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  return(list(summary_wide = summary_wide_df_final, 
              summary_norm = summary_long_df, 
              plot = final_plot_df, 
              time = time_taken))
}

esegui_final_dt <- function(clusters_dt, cell_types_dt) {
  time_taken <- system.time({
    clusters_copy <- copy(clusters_dt)
    cell_types_copy <- copy(cell_types_dt)
    
    clusters_copy[, cell := gsub(pattern = "_X_\\.", replacement = ".", x = cell)]
    
    setkey(clusters_copy, cell)
    setkey(cell_types_copy, cell)
    merged_dt_final <- clusters_copy[cell_types_copy, nomatch = 0L]
    
    counts_dt_final <- merged_dt_final[, .(cell_count = .N), 
                                       by = .(integration_cluster, cell_type)]
    
    summary_wide_dt_final <- dcast(merged_dt_final, 
                                   integration_cluster + cell_type ~ sample_type, 
                                   fun.aggregate = length, 
                                   value.var = "cell",
                                   fill = 0)
    
    # Task 4 & 5 (Plot e Percentuali)
    summary_long_dt <- merged_dt_final[, .(cell_count = .N), 
                                       by = .(integration_cluster, cell_type, sample_type)]
    summary_long_dt[, percentage := (cell_count / sum(cell_count)) * 100, 
                    by = .(integration_cluster, sample_type)]
    
    final_plot_dt <- ggplot(summary_long_dt, 
                            aes(x = integration_cluster, y = percentage, fill = cell_type)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ sample_type) +
      labs(title = "Distribuzione Tipi di Cellule (Normal vs Tumor)",
           x = "Cluster", y = "Percentuale del Cluster (%)", fill = "Tipo di Cellula") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  return(list(summary_wide = summary_wide_dt_final, 
              summary_norm = summary_long_dt, 
              plot = final_plot_dt, 
              time = time_taken))
}