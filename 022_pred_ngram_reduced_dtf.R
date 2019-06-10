# ####################################################################
#                       MODULE MISSION
# reduce the dtf (data table frequency) with all the ngrams
# leaving only the N more frequent predecessors and
# for each predecessor
#    - at most M entries (the prediction items)
# ####################################################################



source("020_pred_globals.R")
source("020_pred_ngram_bare_dtf.R")


reduce_dtf <- function(dtf_ngram_sep_list, n3_pdcess_cut)
  #' @param dtf_ngram_sep_list 
  #' list(dtf_1gram_sep, dtf_2gram_sep, dtf_3gram_sep)
  #'
  #' @return
{
  n = 3
  ngram <- dtf_ngram_sep_list[[n+1]]
  preds_cols <- TYPES_COLNAMES[1:n-1]

  # () needed to get variable content
  ngram[ , (PREDECESSOR_FREQUENCY) := sum(frequency) 
          , by = preds_cols]
  # get index of last predecessor
  # ngram must be ordered by predecessor frequency, descending
  extract_cols <- c(PREDECESSOR_FREQUENCY, preds_cols)
  unique_predec <- unique(ngram[ , ..extract_cols])
  setkeyv(unique_predec, cols = extract_cols)
  key(unique_predec)
  # get predecess l freq  n3_pdcess_cut (ascending order)
  pred_frequency_to_include <- unique_predec[(.N-n3_pdcess_cut)][[PREDECESSOR_FREQUENCY]]
  
  ngram2 <- ngram[ ngram[[PREDECESSOR_FREQUENCY]] > pred_frequency_to_include ] 

  
    # NB setkey always/only ascending
  setkeyv(ngram, cols = extract_cols,  order = -1L)
  key(ngram)
  
  
  print("OK")
}

###########################################################
#                 TEST TEMP
###########################################################

# 
rie(qc_full, force_calc, , readQCorp, data_dir_corpus_in())
# 
dtf_ngram_sep_list <- produce_ngram_bare_dtf(qc_full, force_calc)
reduce_dtf(dtf_ngram_sep_list[2:4])

head(ngram, 10)


# --- check that grouping works as expected --- 
# sum single frequencies
sum_single <- sum(ngram$frequency)
# sum frequencies of predecessors, for check
extract_cols <- c(preds_cols, PREDECESSOR_FREQUENCY)
unique_predec <- unique(ngram[ , ..extract_cols])
sum_occurencies_predecess <- sum(unique_predec$pred_freq)
identical(sum_single, sum_occurencies_predecess)

