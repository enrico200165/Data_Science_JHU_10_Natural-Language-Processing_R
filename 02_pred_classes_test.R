

source("01_globals.R")
source("02_pred_classes.R")


#####################################################################
#                           TEST
#####################################################################

clean_rds("*rds")
fulldata <- T ;  memory.limit(size = 24000)
silent <- F
strict(F)



build_test_objects <- function() {
  
  dtf_1gram_test <<- dtf_1gram_sep[1:100, ]

  dtf_2gram_test <<- dtf_2gram_sep[1:100, ]

  dtf_3gram_test <<- dtf_3gram_sep[1:100, ]
}



produce_ngram_bare_dtf()
build_test_objects()  


o_1grams_basic <- DTF_Basic$new(dtf_1gram_test)
print(o_1grams_basic$coverageGraphs()[[2]]); keypress()
print(o_1grams_basic$coverageGraphs()[[1]]); keypress()

o_2grams_basic <- DTF_Basic$new(dtf_2gram_test)
print(o_2grams_basic$coverageGraphs()[[2]]); keypress()
print(o_2grams_basic$coverageGraphs()[[1]]); keypress()

o_3grams_basic <- DTF_Basic$new(dtf_3gram_test)
print(o_3grams_basic$coverageGraphs()[[2]]); keypress()
print(o_3grams_basic$coverageGraphs()[[1]]); keypress()



grid.arrange( plots[[1]], plots[[2]], nrow = 1)
    
