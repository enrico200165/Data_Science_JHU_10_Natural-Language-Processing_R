
require(lintr)
require(testthat)

source("006_globals.R")
source("024_pred_classes.R")



# --------------------------------------------------------------------
build_small_test_objects <- function(force_calc) {
# --------------------------------------------------------------------

  prt("build_small_test_objects() - start")

  rie(dtf_1gram_sep, force_calc, , produce_ngram_bare_dtf_1)
  o_1gram_test <- DTF_Basic$new(dtf_1gram_sep[1:100, ])

  rie(dtf_2gram_sep, force_calc, , produce_ngram_bare_dtf_2)
  o_2gram_test <- DTF_Basic$new(dtf_2gram_sep[1:100, ])

  rie(dtf_3gram_sep, force_calc, , produce_ngram_bare_dtf_3)
  o_3gram_test <- DTF_Basic$new(dtf_3gram_sep[1:100, ])

  if (F) {
  }

  prt("build_small_test_objects() - end")

  list(
    o1 = o_1gram_test
    , o2 = o_2gram_test
    , o3 = o_3gram_test
  )
}


# --------------------------------------------------------------------
test_nfeat_for_size <- function(o1, o2, o3) {

  s1 <- o1$size()
  s1_target <- s1 * 0.75
  ret <- o1$nfeat_freq_for_size(s1_target)
  prt("index", ret$last_idx, "min freq", ret$lower_freq)

  s2 <- o2$size()
  s2_target <- s2 * 0.75
  ret <- o2$nfeat_freq_for_size(s2_target)
  prt("index", ret$last_idx, "min freq", ret$lower_freq)


  s3 <- o3$size()
  s3_target <- s3 * 0.75
  ret <- o3$nfeat_freq_for_size(s3_target)
  prt("index", ret$last_idx, "min freq", ret$lower_freq)
  
  ret 
}



# --------------------------------------------------------------------
test_freq_ge_feat_idx <- function(o1, o2, o3) {
  
  f <- 100
  idx <- o1$freq_ge_feat_idx(f)
  # prt("index", ret$last_idx, "for frequencies >= ", ret$lower_freq)
  prt("index", idx, "for frequencies >= ", f)
  frqs <- o1$get_frq_df()$frequency
  stopifnot(frqs[idx] >= f)
  stopifnot(frqs[idx + 1] < f)
  prt("min frq", min(frqs[frqs >= f]))
  new_o <- o1$create_subset_4min_freq(f)
  prt("minimum frequency", new_o$get_min_frq())


  f <- 50
  idx <- o2$freq_ge_feat_idx(f)
  #prt("index", ret$last_idx, "for frequencies >= ", ret$lower_freq)
  prt("index", idx, "for frequencies >= ", f)
  frqs <- o2$get_frq_df()$frequency
  stopifnot(frqs[idx] >= f)
  stopifnot(frqs[idx + 1] < f)
  prt("min frq", min(frqs[frqs >= f]))
  new_o <- o2$create_subset_4min_freq(f)
  prt("minimum frequency", new_o$get_min_frq())


  f <- 10
  idx <- o3$freq_ge_feat_idx(f)
  #prt("index", ret$last_idx, "for frequencies >= ", ret$lower_freq)
  prt("index", idx, "for frequencies >= ", f)
  frqs <- o3$get_frq_df()$frequency
  stopifnot(frqs[idx] >= f)
  stopifnot(frqs[idx + 1] < f)
  prt("min frq", min(frqs[frqs >= f]))
  new_o <- o3$create_subset_4min_freq(f)
  prt("minimum frequency", new_o$get_min_frq())
  
  T
}



# --------------------------------------------------------------------
test_pred_classes <- function(force_calc)
# --------------------------------------------------------------------
{
  
  rie(qc, force_calc, NULL, readQCorp, data_dir_corpus_in())
  dtf_ngram_sep_list <- produce_ngram_bare_dtf(qc, force_calc)

  reduce_matrix <- rbind(c(20,20), c(2000,20), c(3000,20))
  ngrams_reduced_l <- reduce_dtfs(dtf_ngram_sep_list,reduce_matrix)
  
  
  rie(o_1grams_basic, force_calc, ,  DTF_Basic$new, ngrams_reduced_l[[2]])
  #assign("o_1grams_basic", o_1grams_basic, .GlobalEnv)

  # o_2grams_basic <- DTF_Basic$new(dtf_2gram_sep)
  rie(o_2grams_basic, force_calc, , DTF_Basic$new, ngrams_reduced_l[[3]])
  # assign("o_2grams_basic", o_2grams_basic, .GlobalEnv)

  rie(o_3grams_basic, force_calc, , DTF_Basic$new, ngrams_reduced_l[[4]])
  assign("o_3grams_basic", o_3grams_basic, .GlobalEnv)

  # test_nfeat_for_size(o_1grams_basic ,o_2grams_basic ,o_3grams_basic)

  test_freq_ge_feat_idx(o_1grams_basic, o_2grams_basic, o_3grams_basic)

  if (F) {
    print(o_1grams_basic$coverageGraphs()[[1]])
    keypress()
    print(o_1grams_basic$coverageGraphs()[[2]])
    keypress()
    print(o_2grams_basic$coverageGraphs()[[1]])
    keypress()
    print(o_2grams_basic$coverageGraphs()[[2]])
    keypress()
    print(o_3grams_basic$coverageGraphs()[[1]])
    keypress()
    print(o_3grams_basic$coverageGraphs()[[2]])
    keypress()
  }
}



#####################################################################
#                           TEST
#####################################################################



fulldata <- T
# memory.limit(size = 24000)
silent <- F
keypressWait <- T
use_full_corpus(F)

build_small_test_objects()


test_pred_classes()
stop("ok SUCCESSO")

force_calc = F

# --------------------------------------------------------------------
test_that("test_pred_classes"
  ,{
    rie(dtf_1gram_sep, force_calc, , produce_ngram_bare_dtf_1)
    # o_1grams_basic <- DTF_Basic$new(dtf_1gram_sep)
    rie(o_1grams_basic, force_calc, , DTF_Basic$new, dtf_1gram_sep)
    assign("o_1grams_basic", o_1grams_basic, .GlobalEnv)
    
    rie(dtf_2gram_sep,force_calc, , produce_ngram_bare_dtf_2)
    # o_2grams_basic <- DTF_Basic$new(dtf_2gram_sep)
    rie(o_2grams_basic, force_calc, , DTF_Basic$new, dtf_2gram_sep)
    assign("o_2grams_basic", o_2grams_basic, .GlobalEnv)
    
    rie(dtf_3gram_sep,force_calc, , produce_ngram_bare_dtf_3)
    # o_3grams_basic <- DTF_Basic$new(dtf_3gram_sep)
    rie(o_3grams_basic, force_calc, , DTF_Basic$new, dtf_3gram_sep)
    assign("o_3grams_basic", o_3grams_basic, .GlobalEnv)
    
    # test_nfeat_for_size(o_1grams_basic ,o_2grams_basic ,o_3grams_basic)
    
    test_freq_ge_feat_idx(o_1grams_basic, o_2grams_basic
      , o_3grams_basic)
    
    if (T) {
      print(o_1grams_basic$coverageGraphs()[[1]])
      keypress()
      print(o_1grams_basic$coverageGraphs()[[2]])
      keypress()
      print(o_2grams_basic$coverageGraphs()[[1]])
      keypress()
      print(o_2grams_basic$coverageGraphs()[[2]])
      keypress()
      print(o_3grams_basic$coverageGraphs()[[1]])
      keypress()
      print(o_3grams_basic$coverageGraphs()[[2]])
      keypress()
    }
  })

