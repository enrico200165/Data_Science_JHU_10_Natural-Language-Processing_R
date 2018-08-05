
# rm(list=ls(all=TRUE))

source("02_pred_ngram_bare_dtf.R")



fulldata <- T; memory.limit(size = 24000)
silent <- F
keypressWait <- T 

use_full_corpus(T,ngram_bare_re_init)


ret <- produce_ngram_bare_dtf()
print(str(ret))


dtf_info(dtf_1gram_sep)
dtf_info(dtf_2gram_sep)
dtf_info(dtf_3gram_sep)
