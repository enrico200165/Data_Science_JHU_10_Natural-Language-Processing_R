
rm(list=ls(all=TRUE))

source("02_pred_ngram_bare_dtf.R")



fulldata <- T; memory.limit(size = 24000)
silent <- F

ret <- produce_ngram_bare_dtf()
print(str(ret))
