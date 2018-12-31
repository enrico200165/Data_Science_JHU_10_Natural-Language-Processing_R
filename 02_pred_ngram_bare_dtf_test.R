

source("02_pred_ngram_bare_dtf.R")



nsilent <- F
keypressWait <- T 

use_full_corpus(F,ngram_bare_re_init)


ret <- produce_ngram_bare_dtf()

dtf_info(dtf_1gram_sep)
dtf_info(dtf_2gram_sep)
dtf_info(dtf_3gram_sep)
