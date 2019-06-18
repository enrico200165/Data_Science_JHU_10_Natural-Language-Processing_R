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
# "separated"  each token of an ngram in one column  "tok1" "tok2" "tok3"
# (rather than in a single column "tok1_tok2_to3")
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
  
  set_parallelism(6,NULL)

  txts_merged <- NULL
  
  # data table frequencies
  dtf_1gram <<- NULL ;dtf_2gram <<- NULL ;dtf_3gram <<- NULL
  dfm_ngram <<- c(NULL, NULL, NULL) 
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
build_dfm_ngrams <- function(qcorpus, n) 
# --------------------------------------------------------------------
#' @description EVGuess: tokenizes and calculates
#' @param qcorpus quanteda corpus
#' @param n n of ngrams, currently values are 1, 2 or 3
#' @return dfm_ret {quanteda} dfm, ie document frequency matrix
#' note that there seems to be a frequency per document
{
  
  # stopifnot(class(txts_par) %in% c("tokens","character"))
  
  if (is.null(qcorpus)) {
    prt_error("qcorpus è null in build_dfm_ngrams(qcorpus, n) ")
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
dtf_ngram <- function(qcorpus, n, force_calc)
# --------------------------------------------------------------------
# NOT SURE IT IS USED, It may crash for lack of memory
{
  prt("dtf_ngram() - begininning - n=",n)
  stopifnot(1 <= n && n <= 3)

  prt("dtf_ngram() - lazy calling build_dfm_ngrams(txts_merged, n) - n=",n)
  
  alias <- paste0("dfm_ngram", n)
  rie_str(alias, force_calc, paste0("dtf_ngram",n,collapse = "")
      ,build_dfm_ngrams, qcorpus,n)

    # textstats
  prt("dtf_ngram() - textstat_frequency(dfm_ngram) - n=",n)
  texstat_ngram <- textstat_frequency(environment()[[alias]]); 
  rm(dfm_ngram, envir = global_env()); 
  rm(dfm_ngram, envir = environment()); 
  gc()

  prt("dtf_ngram() - setDT(texstat_ngram) - n=",n)

  setDT(texstat_ngram)
}


# --------------------------------------------------------------------
build_ngram_bare_dtf <- function(qcorpus, force_calc, n) 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{

  # get textstatfrequency
  alias <- paste0("dtf_", n, "gram", collapse = "")
  rie_str(alias, force_calc, NULL, dtf_ngram ,qcorpus, n, force_calc)
  cur_ngram <- environment()[[alias]]
  
  if (!feature %in% colnames(cur_ngram)) {
    prt_error("not found",feature, "column")
    stop(1)
  }
    
  prt("splitting",n,"grams")
  splits <- strsplit(cur_ngram[[feature]],"_" ,fixed = T)
  
  for (i in 1:n) {
    col_name <- TYPES_COLNAMES[i]
    col_val <- sapply(splits,function(x) x[i])
    cur_ngram[ , (col_name) := col_val]
  }

  assign(alias, cur_ngram, .GlobalEnv)

  cur_ngram
}


# --------------------------------------------------------------------
dtf_info <- function(dtf)
# --------------------------------------------------------------------
{
  prt("info about",deparse(substitute(dtf))
    ,"size",XiB(pryr::object_size(dtf))
    ,"nr features:",nrow(dtf)
    ,"memory/feature ratio", round(pryr::object_size(dtf)/nrow(dtf),0)
    )
}




###########################################################
#             Temporary Test (Just to use debug)
###########################################################



# --------------------------------------------------------------------
test_ngram_bare_dtf <- function(force_calc = F) 
  # --------------------------------------------------------------------
{
  silent <<- F
  keypressWait <<- T 
  fulldata <<- F
  
  use_full_corpus(F,ngram_bare_re_init)
  
  rie(qc_full, force_calc, , readQCorp, data_dir_corpus_in())
  
  dtfs_gram_Sep <- produce_ngram_bare_dtf(qc_full, force_calc)
  
  ret = dtfs_gram_Sep[1]

  dtf_1gram_sep <- dtfs_gram_Sep[[2]]
  dtf_2gram_sep <- dtfs_gram_Sep[[3]]
  dtf_3gram_sep <- dtfs_gram_Sep[[4]]
  
  
  dtf_info(dtfs_gram_Sep[[2]]) 
  dtf_info(dtfs_gram_Sep[[3]]) 
  dtf_info(dtfs_gram_Sep[[4]]) 
  
}


###########################################################
#            TEMPORARY TEST 
###########################################################
  clean_rds("ngram[1-3][^_]")
  
  
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
    
    
    dtf_info(dtfs_gram_sep[[2]]) 
    dtf_info(dtfs_gram_sep[[3]]) 
    dtf_info(dtfs_gram_sep[[4]]) 
    
  }
  pred_ngrams_re_init()
  #
  test_ngram_bare_dtf(F)
  
# clean_rds("[1-3]")
# pred_ngrams_re_init()
# test_ngram_bare_dtf(F)