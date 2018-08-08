

# --------------------------------------------------------------------
build_small_test_objects <- function() 
# --------------------------------------------------------------------
{
  
  prt("build_small_test_objects() - start")

  rie(dtf_1gram_sep , produce_ngram_bare_dtf_1)
  o_1gram_test <- DTF_Basic$new(dtf_1gram_sep[1:100, ])
  
  rie(dtf_2gram_sep , produce_ngram_bare_dtf_2)
  o_2gram_test <- DTF_Basic$new(dtf_2gram_sep[1:100, ])
  
  rie(dtf_3gram_sep , produce_ngram_bare_dtf_3)
  o_3gram_test <- DTF_Basic$new(dtf_3gram_sep[1:100, ])

  if (F) {  
}
  
  prt("build_small_test_objects() - end")
  
  list(
    o1 = o_1gram_test
    ,o2 = o_2gram_test
    ,o3 = o_3gram_test
  )
}


# --------------------------------------------------------------------
test_nfeat_for_size <- function(o1, o2, o3) {

  s1 <- o1$size()
  s1_target = s1*0.75
  ret = o1$nfeat_freq_for_size(s1_target)
  prt("index",ret$last_idx,"min freq",ret$lower_freq)
  
  s2 <- o2$size()
  s2_target = s2*0.75
  ret = o2$nfeat_freq_for_size(s2_target)
  prt("index",ret$last_idx,"min freq",ret$lower_freq)

  
  s3 <- o3$size()
  s3_target = s3*0.75
  ret = o3$nfeat_freq_for_size(s3_target)
  prt("index",ret$last_idx,"min freq",ret$lower_freq)

  
}



# --------------------------------------------------------------------
test_freq_ge_feat_idx <- function(o1, o2, o3) {

  f <- 100
  idx = o1$freq_ge_feat_idx(f)
  prt("index",ret$last_idx,"for frequencies >= ",ret$lower_freq)
  frqs <- o1$get_frq_df()$frequency
  stopifnot(frqs[idx] >= f)
  stopifnot(frqs[idx+1] < f)
  prt("min frq",min(frqs[frqs >= f]))
  new_o <- o1$create_subset_4min_freq(f)
  prt("minimum frequency",new_o$get_min_frq())
  
    
  f <- 50
  idx = o2$freq_ge_feat_idx(f)
  prt("index",ret$last_idx,"for frequencies >= ",ret$lower_freq)
  frqs <- o2$get_frq_df()$frequency
  stopifnot(frqs[idx] >= f)
  stopifnot(frqs[idx+1] < f)
  prt("min frq",min(frqs[frqs >= f]))
  new_o <- o2$create_subset_4min_freq(f)
  prt("minimum frequency",new_o$get_min_frq())
  

  f <- 10
  idx = o3$freq_ge_feat_idx(f)
  prt("index",ret$last_idx,"for frequencies >= ",ret$lower_freq)
  frqs <- o3$get_frq_df()$frequency
  stopifnot(frqs[idx] >= f)
  stopifnot(frqs[idx+1] < f)
  prt("min frq",min(frqs[frqs >= f]))
  new_o <- o3$create_subset_4min_freq(f)
  prt("minimum frequency",new_o$get_min_frq())

}



# --------------------------------------------------------------------
test_pred_classes <- function()
# --------------------------------------------------------------------
{

  rie(dtf_1gram_sep , produce_ngram_bare_dtf_1)
  # o_1grams_basic <- DTF_Basic$new(dtf_1gram_sep)
  rie(o_1grams_basic, DTF_Basic$new, dtf_1gram_sep)
  assign("o_1grams_basic" , o_1grams_basic, .GlobalEnv)
  
  rie(dtf_2gram_sep , produce_ngram_bare_dtf_2)
  #o_2grams_basic <- DTF_Basic$new(dtf_2gram_sep)
  rie(o_2grams_basic, DTF_Basic$new, dtf_2gram_sep)
  assign("o_2grams_basic" , o_2grams_basic, .GlobalEnv)
  
  rie(dtf_3gram_sep , produce_ngram_bare_dtf_3)
  # o_3grams_basic <- DTF_Basic$new(dtf_3gram_sep)
  rie(o_3grams_basic, DTF_Basic$new, dtf_3gram_sep)
  assign("o_3grams_basic" , o_3grams_basic, .GlobalEnv)

  # test_nfeat_for_size(o_1grams_basic ,o_2grams_basic ,o_3grams_basic)
  
  test_freq_ge_feat_idx(o_1grams_basic ,o_2grams_basic 
    ,o_3grams_basic)
  
  if (F) {

  
  print(o_1grams_basic$coverageGraphs()[[1]]); keypress()
  print(o_1grams_basic$coverageGraphs()[[2]]); keypress()
  print(o_2grams_basic$coverageGraphs()[[1]]); keypress()
  print(o_2grams_basic$coverageGraphs()[[2]]); keypress()
  print(o_3grams_basic$coverageGraphs()[[1]]); keypress()
  print(o_3grams_basic$coverageGraphs()[[2]]); keypress()
  
}  


}



#####################################################################
#                           TEST
#####################################################################


source("01_globals.R")
source("02_pred_classes.R")


fulldata <- T ;  memory.limit(size = 24000)
silent <- F
keypressWait <- T
strict(F)
use_full_corpus(F)

build_small_test_objects()


# 
test_pred_classes()
stop("OK OK OK")



# 
print(o_1gram_test$coverageGraphs()[[2]]); 
o_1gram_test$print_coverage()
keypress()


print(o_2gram_test$coverageGraphs()[[2]]); 
o_2gram_test$print_coverage()
keypress()

print(o_3gram_test$coverageGraphs()[[2]]); 
o_3gram_test$print_coverage()
keypress()


