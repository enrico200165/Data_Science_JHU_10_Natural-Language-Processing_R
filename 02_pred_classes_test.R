

# --------------------------------------------------------------------
build_small_test_objects <- function() 
# --------------------------------------------------------------------
{
  
  prt("build_small_test_objects() - start")

    rie(dtf_1gram_sep , produce_ngram_bare_dtf_1)
  o_1gram_test <<- DTF_Basic$new(dtf_1gram_sep[1:100, ])
  
if (F) {  
}
  rie(dtf_2gram_sep , produce_ngram_bare_dtf_2)
  o_2gram_test <<- DTF_Basic$new(dtf_2gram_sep[1:100, ])
  rie(dtf_3gram_sep , produce_ngram_bare_dtf_3)
  o_3gram_test <<- DTF_Basic$new(dtf_3gram_sep[1:100, ])
  
  prt("build_small_test_objects() - end")
}


# --------------------------------------------------------------------
test_pred_classes <- function()
# --------------------------------------------------------------------
{

if (T) {
  
  rie(dtf_1gram_sep , produce_ngram_bare_dtf_1)
  o_1grams_basic <- DTF_Basic$new(dtf_1gram_sep)
  print(o_1grams_basic$coverageGraphs()[[1]]); keypress()
  print(o_1grams_basic$coverageGraphs()[[2]]); keypress()

  rie(dtf_2gram_sep , produce_ngram_bare_dtf_2)
  o_2grams_basic <- DTF_Basic$new(dtf_2gram_sep)
  print(o_2grams_basic$coverageGraphs()[[1]]); keypress()
  print(o_2grams_basic$coverageGraphs()[[2]]); keypress()
}  

  rie(dtf_3gram_sep , produce_ngram_bare_dtf_3)
  o_3grams_basic <- DTF_Basic$new(dtf_3gram_sep)
  print(o_3grams_basic$coverageGraphs()[[1]]); keypress()
  print(o_3grams_basic$coverageGraphs()[[2]]); keypress()

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

# 

# 
build_small_test_objects()
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


# test_pred_classes()