

source("006_globals.R")
source("007_utils.R")

source("010_subset_text_files.R")
source("014_corpus.R")
source("020_pred_ngram_bare_dtf.R")
source("022_pred_ngram_reduced_dtf.R")



qc <- NULL # quanteda corpus


pipeline <- function(force_calc) {
  
  ok = T
  
  # ================ clean ======================
  # done in java once and for all
  
  # ================  subset ====================
  ok && subsetTextFilesByLines(data_dir_corpus_in() 
    ,data_dir_corpus_subset ,1,1000 , force_calc)
  
  # ===== FREQUENCY RAW DATA TABLES ============
  # --- use qanteda for tokenization and freq calc
  # --- load into quanteda corpus
  corp_dir <- data_dir_corpus_in()
  rie(qc,force_calc, NULL , readQCorp ,corp_dir)
  # print(qc)
  # --- calculate types frequencies
  dtfs_gram_sep <- produce_ngram_bare_dtf(qc_full, force_calc)
  # print(dtfs_gram_sep[[4]])
  
  # ======= REDUCE FREQUENCY DATA TABLES ========
  reduced_dtfs <- reduce_dtfs(dtfs_gram_sep)
  print(reduced_dtfs[[4]])
  
}


pipeline(force_calc = F)

