
source("006_globals.R")
source("007_utils.R")

source("010_subset_text_files.R")
source("014_corpus.R")
source("020_pred_ngram_bare_dtf.R")
source("022_pred_ngram_reduced_dtf.R")


if (!exists("qc"))                qc            <- NULL # quanteda corpus
if (!exists("dtfs_gram_sep"))     dtfs_gram_sep <- NULL # quanteda corpus
if (!exists("reduced_dtfs"))      reduced_dtfs  <- NULL # quanteda corpus


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
  rie(dtfs_gram_sep, force_calc,NULL,produce_ngram_bare_dtf,qc, force_calc)
  dtfs_gram_sep <- produce_ngram_bare_dtf(qc, force_calc)
  # print(dtfs_gram_sep[[4]])
  
  # ======= REDUCE FREQUENCY DATA TABLES ========
  reduce_matrix <- rbind(c(20,20), c(20, 2000), c(20, 3000))
  reduced_dtfs <- reduce_dtfs(dtfs_gram_sep, reduce_matrix)

  if (!reduced_dtfs[[1]]) {
    stop("pipeline failed")
  }
  
  for (i in seq_along(reduced_dtfs[2:4])) {
    saveRDS(reduced_dtfs[[i+1]], file = PRED_NGRAM_FNAMES[i])
    prt("generated ngram prediction file: ", PRED_NGRAM_FNAMES[i])
  }
    
  TRUE
}

pipeline(F)