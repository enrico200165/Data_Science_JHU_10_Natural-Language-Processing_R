

source("007_utils.R")
source("020_pred_ngram_bare_dtf.R")
source("014_corpus.r")

clean_rds("ngram[1-3][^_]")


# --------------------------------------------------------------------
test_ngram_bare_dtf <- function(force_calc = F) 
  # --------------------------------------------------------------------
{
  silent <<- F
  keypressWait <<- F
  fulldata <<- F
  
  use_full_corpus(F,ngram_bare_re_init)
  
  rie(qc_full, force_calc, , readQCorp, data_dir_corpus_in())
  
  
  dtfs_gram_sep <- produce_ngram_bare_dtf(qc_full, force_calc)
  
  ret = dtfs_gram_sep[1]
  
  dtf_1gram_sep <- dtfs_gram_sep[[2]]
  dtf_2gram_sep <- dtfs_gram_sep[[3]]
  dtf_3gram_sep <- dtfs_gram_sep[[4]]
  
  
  dtf_info(dtfs_gram_sep[[2]]) 
  dtf_info(dtfs_gram_sep[[3]]) 
  dtf_info(dtfs_gram_sep[[4]]) 
  
}
pred_ngrams_re_init()
#
test_ngram_bare_dtf(F)