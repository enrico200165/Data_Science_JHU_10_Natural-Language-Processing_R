

source("02_pred_ngram_bare_dtf.R")

# --------------------------------------------------------------------
test_ngram_bare_dtf <- function() 
# --------------------------------------------------------------------
{
  silent <<- F
  keypressWait <<- T 
  fulldata <<- F
  
  use_full_corpus(F,ngram_bare_re_init)
  
  

  dtfs_gram_Sep <- produce_ngram_bare_dtf()

  ret = dtfs_gram_Sep[1]
  dtf_info(dtfs_gram_Sep[2]) 
  dtf_info(dtfs_gram_Sep[3]) 
  dtf_info(dtfs_gram_Sep[4]) 
  
  dtf_info(dtf_1gram_sep)
  dtf_info(dtf_2gram_sep)
  dtf_info(dtf_3gram_sep)
}

# 
test_ngram_bare_dtf()