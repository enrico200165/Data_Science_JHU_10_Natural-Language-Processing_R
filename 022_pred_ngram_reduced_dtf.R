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
  dtf_3gram_reduced <- reduce_dtf(ngram, n, n3)
  
  n <- 2
  ngram <- dtf_ngram_sep_list[[n+1]]
  dtf_2gram_reduced <- reduce_dtf(ngram, n, n2)
  
  dtf_1gram_reduced <- NULL
  
  ret <- T
  return(list(dtf_1gram_reduced, dtf_1gram_reduced, dtf_2gram_reduced, dtf_3gram_reduced))
}


reduce_dtf <- function(ngram, n, pdcess_cut)
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
  # get predecess freq  pdcess_cut (ascending order)
  pred_frequency_to_include <- unique_predec[(.N-pdcess_cut)][[PREDECESSOR_FREQUENCY]]
  
  # exclude predecessors
  ngram <- ngram[ ngram[[PREDECESSOR_FREQUENCY]] >= pred_frequency_to_include ] 
  setkeyv(ngram, cols = c(TYPES_COLNAMES[1:n-1], PREDECESSOR_FREQUENCY))
  key(ngram)
  
  # just for test
  # ngram[ .("at","the")][, head(.SD,10) ]
  
  # limit max predictions for each predecessor
 
  ngram2 <-ngram[ , .SD[1:10], by = c(TYPES_COLNAMES[1:n-1])]
}

###########################################################
#                 TEST TEMP
###########################################################

# 
force_calc <- F
rie(qc_full, force_calc, , readQCorp, data_dir_corpus_in())
# 
dtf_ngram_sep_list <- produce_ngram_bare_dtf(qc_full, force_calc)
reduced <- reduce_dtfs(dtf_ngram_sep_list,5 , 5 , 5)

n3 <- reduced[[4]]
print(head(n3[ .("at","the")], 20))

n2 <- reduced[[3]]
print(head(n2[ .("to")], 20))



