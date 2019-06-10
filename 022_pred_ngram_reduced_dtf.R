# ####################################################################
#                       MODULE MISSION
# reduce the dtf (data table frequency) with all the ngrams
# leaving only the N more frequent predecessors and
# for each predecessor
#    - at most M entries (the prediction items)
# ####################################################################



source("020_pred_globals.R")
source("020_pred_ngram_bare_dtf.R")



reduce_dtfs <- function(dtf_ngram_sep_list, n1 = 1000, n2 = 5000, n3 = 10000)
{
  n <- 3
  ngram <- dtf_ngram_sep_list[[n+1]]
  dtf_3gram_reduced <- reduce_dtf(ngram, n3)
  
  n <- 2
  ngram <- dtf_ngram_sep_list[[n+1]]
  dtf_2gram_reduced <- reduce_dtf(ngram, n2)
  
  dtf_1gram_reduced <- NULL
  
  ret <- T
  return(list(dtf_1gram_reduced, dtf_1gram_reduced, dtf_2gram_reduced, dtf_3gram_reduced))
}


reduce_dtf <- function(dtf_ngram_sep_list, pdcess_cut)
  #' @param dtf_ngram_sep_list 
  #' list(dtf_1gram_sep, dtf_2gram_sep, dtf_3gram_sep)
  #'
  #' @return
{

  preds_cols <- TYPES_COLNAMES[1:n-1]

  cols_to_keep <- c(TYPES_COLNAMES[1:n], PREDECESSOR_FREQUENCY, FREQUENCY_COL)
  cols_to_remove <- setdiff(names(ngram), cols_to_keep)
  
  # remove unnecessary columns
  if (length(cols_to_remove) > 0)
      ngram[  ,(cols_to_remove) := NULL]
  
  # get predecessor's frequency
  # () needed to get variable content
  ngram[ , (PREDECESSOR_FREQUENCY) := sum(frequency) , by = preds_cols]
  # get index of last predecessor
  # ngram must be ordered by predecessor frequency, descending
  extract_cols <- c(preds_cols, PREDECESSOR_FREQUENCY)
  unique_predec <- unique(ngram[ , ..extract_cols])
  setkeyv(unique_predec, cols = c(PREDECESSOR_FREQUENCY, TYPES_COLNAMES[1:n-1]))
  key(unique_predec)
  # get predecess l freq  n3_pdcess_cut (ascending order)
  pred_frequency_to_include <- unique_predec[(.N-pdcess_cut)][[PREDECESSOR_FREQUENCY]]
  
  # exclude
  ngram <- ngram[ ngram[[PREDECESSOR_FREQUENCY]] >= pred_frequency_to_include ] 
  setkeyv(ngram, cols = c(TYPES_COLNAMES[1:n-1], PREDECESSOR_FREQUENCY))
  key(ngram)
  
  ngram[.("eee","sss")][ , c(TYPES_COLNAMES, FREQUENCY_COL), with = F][1:10]
}

###########################################################
#                 TEST TEMP
###########################################################

# 
force_calc <- F
rie(qc_full, force_calc, , readQCorp, data_dir_corpus_in())
# 
dtf_ngram_sep_list <- produce_ngram_bare_dtf(qc_full, force_calc)
reduced <- reduce_dtfs(dtf_ngram_sep_list)

head(ngram, 10)


# --- check that grouping works as expected --- 
# sum single frequencies
sum_single <- sum(ngram$frequency)
# sum frequencies of predecessors, for check
extract_cols <- c(preds_cols, PREDECESSOR_FREQUENCY)
unique_predec <- unique(ngram[ , ..extract_cols])
sum_occurencies_predecess <- sum(unique_predec$pred_freq)
identical(sum_single, sum_occurencies_predecess)

