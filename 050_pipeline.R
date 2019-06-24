


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
  sound_nr <- 1
  
  # ================ clean ======================
  # done in java once and for all
  
  # ================  subset ====================
  ok && subsetTextFilesByLines(data_dir_corpus_full 
    ,data_dir_corpus_subset ,25,100 , force_calc)
  beep(sound = sound_nr, expr = NULL)
  
  
  # ===== FREQUENCY RAW DATA TABLES ============
  # --- use qanteda for tokenization and freq calc
  # --- load into quanteda corpus
  prt("Build Q Corpus - Start")
  corp_dir <- data_dir_corpus_in()
  rie(qc,force_calc, NULL , readQCorp ,corp_dir)
  prt("Build Q Corpus - End")
  # --- calculate types frequencies
  prt("Bare DTF - Start")
  rie(dtfs_gram_sep, force_calc,NULL,produce_ngram_bare_dtf,qc, force_calc)
  prt("Bare DTF - End")
  beep(sound = sound_nr, expr = NULL)

  # ======= REDUCE FREQUENCY DATA TABLES ========
  mille  <- 0 # 1000
  nr_succ <- 0 # 6
  reduce_matrix <- rbind(c(20,20), c(nr_succ, 1000*mille), c(nr_succ, 400*mille))
  prt("Reduce DTF - Start")
  reduced_dtfs <- reduce_dtfs(dtfs_gram_sep, reduce_matrix)
  prt("Reduce DTF - End")
  beep(sound = sound_nr, expr = NULL)
  
  prt("Writing models - Start")
  for (i in seq_along(reduced_dtfs[2:4])) {
    fname <- file.path(SHINY_LOCAL_DATA_DIR,PRED_NGRAM_FNAMES[i])
    saveRDS(reduced_dtfs[[i+1]], file = fname)
    prt("generated ngram prediction file: ", fname)
  }
  prt("Writing models - End")
  
  beep(sound = sound_nr, expr = NULL)
  
  TRUE
}

pipeline(F)