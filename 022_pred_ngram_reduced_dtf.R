# ####################################################################
#                       MODULE MISSION
# reduce the dtf (data table frequency) with all the ngrams
# leaving only the N more frequent predecessors and
# for each predecessor
#    - at most M entries (the prediction items)
# ####################################################################



source("020_pred_globals.R")
source("020_pred_ngram_bare_dtf.R")



reduce_dtfs <- function(dtf_ngram_sep_list, reduct_matrix)
#' @param list of 4 elements:
#' 1 - OK, boolean
#' 2 - 1gram DT, 3 - 2gram DT, 4 3gram dt
#' @param reduct_matrix: row i for the igram (1 for 1gramm)
#' row has 2 elements: nr of predecessors to kee, 2 nr of predictions
#' @return 
{
  n <- 3
  ngram <- dtf_ngram_sep_list[[n+1]]
  dtf_3gram_reduced <- reduce_dtf(ngram, n, reduct_matrix[n, 1])
  
  n <- 2
  ngram <- dtf_ngram_sep_list[[n+1]]
  dtf_2gram_reduced <- reduce_dtf(ngram, n, reduct_matrix[n, 1])
  
  n <- 1
  ngram <- dtf_ngram_sep_list[[n+1]]
  dtf_1gram_reduced <- tail(ngram, reduct_matrix[n, 1])
  
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



