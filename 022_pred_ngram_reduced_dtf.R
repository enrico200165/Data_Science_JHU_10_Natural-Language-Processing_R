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
  n <- 1
  ngram <- dtf_ngram_sep_list[[n+1]]
  dtf_1gram_reduced <- head(ngram, reduct_matrix[n, 1])
  
  n <- 2
  ngram <- dtf_ngram_sep_list[[n+1]]
  dtf_2gram_reduced <- reduce_dtf(ngram, n, reduct_matrix[n, 1], reduct_matrix[n, 2]) 
  
  n <- 3
  ngram <- dtf_ngram_sep_list[[n+1]]
  dtf_3gram_reduced <- reduce_dtf(ngram, n, reduct_matrix[n, 1], reduct_matrix[n, 2])
  
  ret <- T
  return(list(ret, dtf_1gram_reduced, dtf_2gram_reduced, dtf_3gram_reduced))
}


reduce_dtf <- function(ngram, n, n_success_cut, pdcess_cut)
  #' @param dtf_ngram_sep_list 
  #' list(dtf_1gram_sep, dtf_2gram_sep, dtf_3gram_sep)
  #'
  #' @return
{

  preds_cols <- TYPES_COLNAMES[1:n-1]

  cols_to_keep <- c(TYPES_COLNAMES[1:n], PREDECESSOR_FREQUENCY, FREQUENCY_COL)
  cols_to_remove <- setdiff(names(ngram), cols_to_keep)
  extract_cols <- c(preds_cols, PREDECESSOR_FREQUENCY)
  
  # remove unnecessary columns
  if (length(cols_to_remove) > 0)
      ngram[  ,(cols_to_remove) := NULL]
  
  # calculate predecessor's frequency, () needed to get variable content
  ngram[ , (PREDECESSOR_FREQUENCY) := sum(frequency) , by = preds_cols]

  if (n_success_cut > 0 ) {
  # reduce the number of successors to a max
    prt(n,"gram reduce DTF successors (prediction choises) to a max - Start")
    ngram <-ngram[ , .SD[1:min(c(n_success_cut, .N))], by = c(TYPES_COLNAMES[1:n-1])]
    prt(n,"gram reduce DTF successors (prediction choises) to a max - End")
  } else {
    prt_warn("NOT reducing successors(predictions)")
  }
  
  if (pdcess_cut > 0) {
  # get index of last predecessor
  # ngram must be ordered by predecessor frequency, descending
    prt(n,"gram reduce DTF PREdecessors to a max - Start")
    unique_predec <- unique(ngram[ , ..extract_cols])
    setkeyv(unique_predec, cols = c(PREDECESSOR_FREQUENCY, TYPES_COLNAMES[1:n-1]))
    pred_frequency_to_include <- unique_predec[(.N-pdcess_cut)][[PREDECESSOR_FREQUENCY]]
    prt(n,"gram reduce nr Predecessors to a max - before actually removing lines")
    ngram <- ngram[ ngram[[PREDECESSOR_FREQUENCY]] >= pred_frequency_to_include ] 
    prt(n,"gram reduce nr PREdecessors to a max - End")
  }else {
    prt_warn("NOT reducing predecessors (predictORs)")
  }

  # prepare it for queries
  prt(n,"gram final set key - Start")
  setkeyv(ngram, cols = c(TYPES_COLNAMES[1:n-1], PREDECESSOR_FREQUENCY))
  prt(n,"gram final set key - End")
  
  ngram
}

###########################################################
#                 TEST TEMP
###########################################################

