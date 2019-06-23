
require(dplyr)
require(data.table)

require(quanteda)
require(readtext)

source("006_globals.R")
source("007_utils.R")
source("014_corpus.R")
source("020_pred_globals.R")


# ####################################################################
#                       MODULE MISSION
# from text files produce 3 (global ngrams) data.tables 
# "separated"  each token of an ngram in one column  
# ex.. "tok1" "tok2" "tok3" instead of "tok1_tok2_to3"
#
# ####################################################################
#                     EXTERNAL FUNCIONS
#
#  produce_ngram_bare_dtf()
#
######################################################################


# --------------------------------------------------------------------
pred_ngrams_re_init <- function() 
# --------------------------------------------------------------------  
{
  prt("start pred_ngrams_re_init()")
  
  set_parallelism(4,NULL)

  txts_merged <- NULL
  
  # data table frequencies
  # dtf_1gram <<- NULL ;dtf_2gram <<- NULL ;dtf_3gram <<- NULL
  # dfm_ngram <<- c(NULL, NULL, NULL) 
  prt("completed pred_ngrams_re_init()")
  pre_ngram_bare_dtf_inited <<- T
}


# --------------------------------------------------------------------
produce_ngram_bare_dtf <- function(qcorpus, force_calc)
# --------------------------------------------------------------------
{
  set_parallelism(6,NULL)
  
  rie(dtf_1gram_sep, force_calc, , build_ngram_bare_dtf, qcorpus, force_calc, 1)
  rie(dtf_2gram_sep, force_calc, , build_ngram_bare_dtf, qcorpus, force_calc, 2)
  rie(dtf_3gram_sep, force_calc, , build_ngram_bare_dtf, qcorpus, force_calc, 3)

  OK = T
  return(list(OK, dtf_1gram_sep, dtf_2gram_sep, dtf_3gram_sep))
}

###########################################################
#               private
###########################################################


# column that contains the ngrams in quanteda
feature <- "feature"


# --------------------------------------------------------------------
build_DFM <- function(qcorpus, n) 
# --------------------------------------------------------------------
#' @description EVGuess: tokenizes and calculates
#' @param qcorpus quanteda corpus
#' @param n n of ngrams, currently values are 1, 2 or 3
#' @return dfm_ret {quanteda} dfm, ie document frequency matrix
#' note that there seems to be a frequency per document
{
  
  # stopifnot(class(txts_par) %in% c("tokens","character"))
  
  if (is.null(qcorpus)) {
    prt_error("qcorpus è null in build_DFM(qcorpus, n) ")
    stop(1)
  }
  
  dfm_ret <- dfm(qcorpus
                 ,ngrams = n
                 ,remove_numbers = T
                 ,remove_punct = T
                 ,remove_symbols = T
                 ,remove_separators = T
                 ,remove_twitter = T
                 ,remove_hyphens = FALSE
                 ,remove_url = FALSE,
  )
  
  prt("docs: ",ndoc(dfm_ret), "features",nfeat(dfm_ret))
  
  dfm_ret
}


# --------------------------------------------------------------------
build_ngram_bare_dtf <- function(qcorpus, force_calc, n) 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{
  # dfm
  prt("dtf_ngram() - lazy calling build_DFM(txts_merged, n) - n=",n)
  alias <- paste0("dfm_", n, "ngram","_sep")
  rie_str(alias, force_calc, paste0("dtf_ngram",n,collapse = "")
          ,build_DFM, qcorpus,n)
  
  # textstats
  cur_ngram <- textstat_frequency(environment()[[alias]]); 
  setDT(cur_ngram)
  cur_ngram[ ,  c("rank", "group", "docfreq") := NULL]
  gc()
  

  prt("splitting",n,"grams")
  splits <- strsplit(cur_ngram[[feature]],"_" ,fixed = T)
  cur_ngram[ ,  c(feature) := NULL]
  
  for (i in 1:n) {
    col_name <- TYPES_COLNAMES[i]
    col_val <- sapply(splits,function(x) x[i])
    cur_ngram[ , (col_name) := col_val]
  }

  setcolorder(cur_ngram, c(TYPES_COLNAMES[1:n],FREQUENCY_COL))
  assign(alias, cur_ngram, .GlobalEnv)

  cur_ngram
}


###########################################################
#          Temporary Test (Just to use debug)
###########################################################


clean_rds("ngram")

# --------------------------------------------------------------------
test_ngram_bare_dtf <- function(force_calc = F) 
# --------------------------------------------------------------------
{
  silent <<- F
  keypressWait <<- F
  fulldata <<- F
  
  use_full_corpus(F,ngram_bare_re_init)
  
  rie(qc_full, force_calc, , readQCorp, data_dir_corpus_in())
  
  dtfs_gram_sep <- produce_ngram_bare_dtf(qc_full, force_calc)
  
  ret = dtfs_gram_sep[1]
  
  dtf_1gram_sep <- dtfs_gram_sep[[2]]
  dtf_2gram_sep <- dtfs_gram_sep[[3]]
  dtf_3gram_sep <- dtfs_gram_sep[[4]]
  
  
  dtf_info(dtf_1gram_sep, T)
  dtf_info(dtf_2gram_sep, T) 
  dtf_info(dtf_3gram_sep, T)
  
}

