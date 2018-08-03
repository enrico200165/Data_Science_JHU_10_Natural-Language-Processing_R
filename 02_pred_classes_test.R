

source("01_globals.R")
source("02_pred_classes.R")


#####################################################################
#                           TEST
#####################################################################


fulldata <- F
silent <- F
strict(F)



build_test_objects <- function() {
  
  rie(dtf_1gram_sep,produce_ngram_bare_dtf)
  dtf_1gram_test <<- dtf_1gram_sep[1:100, ]

  rie(dtf_2gram_sep ,produce_ngram_bare_dtf)
  dtf_2gram_test <<- dtf_2gram_sep[1:100, ]

  rie(dtf_3gram_sep ,produce_ngram_bare_dtf)
  dtf_3gram_test <<- dtf_3gram_sep[1:100, ]
}



silent <- F

rie(dtf_1gram_test ,build_test_objects)
rie(dtf_2gram_test ,build_test_objects)
rie(dtf_3gram_test ,build_test_objects)


o_1grams_basic <- DTF_Basic$new(dtf_1gram_test)
o_2grams_basic <- DTF_Basic$new(dtf_2gram_test)
o_3grams_basic <- DTF_Basic$new(dtf_3gram_test)




x <- DTF_Basic$new(dtf_1gram_test)
plots <- x$coverageGraphs()

grid.arrange( plots[[1]], plots[[2]], nrow = 1)
    
