
source("022_pred_ngram_reduced_dtf.R")



rie(qc_full, force_calc, , readQCorp, data_dir_corpus_in())
dtfs_ngrams_bare_sep <- produce_ngram_bare_dtf(qc_full, force_calc)
reduce_dtf(dtfs_ngrams_bare_sep)