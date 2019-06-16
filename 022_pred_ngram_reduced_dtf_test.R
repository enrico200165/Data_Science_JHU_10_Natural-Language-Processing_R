
source("022_pred_ngram_reduced_dtf.R")

force_calc <- F


rie(qc, force_calc, NULL, readQCorp, data_dir_corpus_in())
dtf_ngram_sep_list <- produce_ngram_bare_dtf(qc, force_calc)
reduce_matrix <- rbind(c(20,20), c(2000,20), c(3000,20))

reduced <- reduce_dtfs(dtf_ngram_sep_list,reduce_matrix)

n3 <- reduced[[4]]
print(head(n3[ .("at","the")], 20))

n2 <- reduced[[3]]
print(head(n2[ .("to")], 20))
      