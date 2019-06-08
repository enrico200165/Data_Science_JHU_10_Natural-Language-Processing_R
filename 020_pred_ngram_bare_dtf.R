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
  dfm_ngram <<- NULL  
  prt("completed pred_ngrams_re_init()")
  pre_ngram_bare_dtf_inited <<- T
}


# --------------------------------------------------------------------
produce_ngram_bare_dtf <- function(qcorpus, force_calc)
# --------------------------------------------------------------------
{
  set_parallelism(6,NULL)
  
  # dtf_1gram_sep <- produce_ngram_bare_dtf_1(qcorpus, force_calc)
  # dtf_2gram_sep <- produce_ngram_bare_dtf_2(qcorpus, force_calc)
  # dtf_3gram_sep <- produce_ngram_bare_dtf_3(qcorpus, force_calc)
  
  rie(dtf_1gram_sep, force_calc, , produce_ngram_bare_dtf_1,qcorpus, force_calc)
  rie(dtf_2gram_sep, force_calc, , produce_ngram_bare_dtf_2,qcorpus, force_calc)
  rie(dtf_3gram_sep, force_calc, , produce_ngram_bare_dtf_3,qcorpus, force_calc)
  OK = T

  return(list(OK, dtf_1gram_sep, dtf_2gram_sep, dtf_3gram_sep))
}

###########################################################
#               private
###########################################################


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
dtf_ngram <- function(qcorpus , n, force_calc)
# --------------------------------------------------------------------
# NOT SURE IT IS USED, It may crash for lack of memory
{
  prt("dtf_ngram() - begininning - n=",n)
  stopifnot(1 <= n && n <= 3)

  prt("dtf_ngram() - lazy calling build_dfm_ngrams(txts_merged, n) - n=",n)
  rie(dfm_ngram, force_calc, paste("dtf_ngram","n",collapse = "")
      ,build_dfm_ngrams,qcorpus,n)

  # textstats
  prt("dtf_ngram() - textstat_frequency(dfm_ngram) - n=",n)
  texstat_ngram <- textstat_frequency(dfm_ngram); rm(dfm_ngram); 
  gc()

  prt("dtf_ngram() - setDT(texstat_ngram) - n=",n)

  setDT(texstat_ngram)
}


  
# --------------------------------------------------------------------
produce_ngram_bare_dtf_1 <- function(qcorpus, force_calc) 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{
  feature <- "feature"

  prt("builfrequency data tables sigle-string _sep")
  rie(dtf_1gram, force_calc, , dtf_ngram ,qcorpus , 1,force_calc)
  prt("splitting dts 1grams")
  
  if (feature %in% colnames(dtf_1gram)) {
    dt1_fun <- function(dt) { 
      setnames(dt, "feature", TYPES_COLNAMES[1])
    }
    rie(dtf_1gram_sep, force_calc, , dt1_fun, dtf_1gram)
  } else {
    prt_error("feature col not found")
    stop()
  }
  assign("dtf_1gram_sep" ,dtf_1gram_sep, .GlobalEnv)
  if (nrow(dtf_1gram) != nrow(dtf_1gram_sep))
    prt_error("different nr rows splitting unigram:",nrow(dtf_1gram),nrow(dtf_1gram_sep))

  kill_var(dtf_1gram)
  dtf_1gram_sep
}


# --------------------------------------------------------------------
produce_ngram_bare_dtf_2 <- function(qcorpus, force_calc) 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{
  feature <- "feature"

  prt("splitting dts 2grams")
  rie(dtf_2gram, force_calc, ,dtf_ngram ,qcorpus, 2, force_calc)
  if (feature %in% colnames(dtf_2gram)) {
    dt2_fun <- function(dt) {
      
      splits <- strsplit(dt$feature,"_" ,fixed = T)
      prim <- sapply(splits,function(x) x[1])
      sec  <- sapply(splits,function(x) x[2])
      # dt[ , TYPES_COLNAMES[1:2] := list(prim, sec) ]
      dt[ , `:=`(primo = prim, secondo = sec) ]
      dt[, feature := NULL]
      rm(splits , prim , sec); gc()
      dt
    }
    rie(dtf_2gram_sep, force_calc,,dt2_fun, dtf_2gram)
    assign("dtf_2gram_sep" ,dtf_2gram_sep, .GlobalEnv)
  } else {
    prt_error("feature col not found")
    stop()
  }
  if (nrow(dtf_2gram) != nrow(dtf_2gram_sep))
    prt("different nr rows splitting unigram:",nrow(dtf_2gram),nrow(dtf_2gram_sep))

  kill_var(dtf_2gram)
  dtf_2gram_sep
  }



# --------------------------------------------------------------------
produce_ngram_bare_dtf_3 <- function(qcorpus, force_calc) 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{
  feature <- "feature"

  rie(dtf_3gram , force_calc, , dtf_ngram ,qcorpus , 3, force_calc)
  prt("splitting dts 3grams")
  if (feature %in% colnames(dtf_3gram)) {

    dt3_fun <- function(dt) {
          
      splits <- strsplit(dt$feature,"_" ,fixed = T)
      prim <- sapply(splits,function(x) x[1])
      sec  <- sapply(splits,function(x) x[2])
      ter  <- sapply(splits,function(x) x[3])

      # dt[ , TYPES_COLNAMES[1:3] := list(prim, sec , ter) ] # seems to corrupt memory
      dt[ , `:=`(primo = prim, secondo = sec, terzo = ter) ]
      dt[, feature := NULL]
      rm(splits , prim , sec , ter); gc()

      dt
    }
    rie(dtf_3gram_sep ,force_calc, ,dt3_fun, dtf_3gram)
    assign("dtf_3gram_sep" ,dtf_3gram_sep, .GlobalEnv)
  } else {
    prt_error("feature col not found")
    stop()
  }
  if (nrow(dtf_3gram) != nrow(dtf_3gram_sep))
    prt("different nr rows splitting unigram:",nrow(dtf_3gram),nrow(dtf_3gram_sep))

  kill_var(dtf_3_gram)
  dtf_3gram_sep
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


silent <- F
fulldata <- F

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
  dtf_info(dtfs_gram_Sep[2]) 
  dtf_info(dtfs_gram_Sep[3]) 
  dtf_info(dtfs_gram_Sep[4]) 
  
  dtf_info(dtf_1gram_sep)
  dtf_info(dtf_2gram_sep)
  dtf_info(dtf_3gram_sep)
}
pred_ngrams_re_init()
#
test_ngram_bare_dtf(F)