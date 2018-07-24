

source("02_pred_classes.R")


#####################################################################
#                           TEST
#####################################################################

dtf_test <- if (exists("dtf_test")) dtf_test else NULL

get_test_dtf <- function() {
  rie(dtf_2gram_sep,produce_ngram_bare_dtf)
  dtf_test <<- dtf_2gram_sep[1:100, ]
}


rie(dtf_test, get_test_dtf)

x <- DTF_Basic$new(NULL)
