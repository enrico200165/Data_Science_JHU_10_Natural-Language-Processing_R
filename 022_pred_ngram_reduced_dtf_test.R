
source("022_pred_ngram_reduced_dtf.R")



rie(qc_full, force_calc, , readQCorp, data_dir_corpus_in())
dtfs_ngrams_bare_sep <- produce_ngram_bare_dtf(qc_full, force_calc)
reduce_dtf(dtfs_ngrams_bare_sep)

force_calc <- F
rie(qc_full, force_calc, , readQCorp, data_dir_corpus_in())
# 
dtf_ngram_sep_list <- produce_ngram_bare_dtf(qc_full, force_calc)
reduced <- reduce_dtfs(dtf_ngram_sep_list,5 , 5 , 5)

n3 <- reduced[[4]]
print(head(n3[ .("at","the")], 20))

n2 <- reduced[[3]]
print(head(n2[ .("to")], 20)
      