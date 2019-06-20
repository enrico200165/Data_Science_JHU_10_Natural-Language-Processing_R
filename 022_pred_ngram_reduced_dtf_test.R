
source("022_pred_ngram_reduced_dtf.R")


force_calc <- F
qc<- NULL

rie(qc, force_calc, NULL, readQCorp, data_dir_corpus_in())
dtf_ngram_sep_list <- produce_ngram_bare_dtf(qc, force_calc)
reduce_matrix <- rbind(c(20,20), c(20, 2000), c(20, 3000))

reduced <- reduce_dtfs(dtf_ngram_sep_list,reduce_matrix)



print(" 1 grams")
n1 <- reduced[[2]]
dtf_info(n1, T)

coverageGraphs(n1, 1,qtiles_vec)

print(" 2 grams")
n2 <- reduced[[3]]
dtf_info(n2, T)

print(" 3 grams")
n3 <- reduced[[4]]
dtf_info(n3, T)

print(head(n3[ .("at","the")], 20))
print(head(n3[.("zack","ryder")], 20))
