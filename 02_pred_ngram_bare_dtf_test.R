
rm(list=ls(all=TRUE))

source("02_pred_ngram_bare_dtf.R")



fulldata <- T
silent <- F

ret <- produce_ngram_bare_dtf()
print(str(ret))
