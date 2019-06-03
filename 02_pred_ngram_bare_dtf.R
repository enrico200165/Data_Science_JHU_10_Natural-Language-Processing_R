require(dplyr)
require(data.table)

require(quanteda)
require(readtext)

source("01_globals.R")
source("00_utils.R")


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
build_dfm_ngrams <- function(txts_par, n) 
# --------------------------------------------------------------------
{
  
  # stopifnot(class(txts_par) %in% c("tokens","character"))

  dfm_ret <- dfm(txts_par
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
dtf_ngram <- function(txts_merged , n)
# --------------------------------------------------------------------
# NOT SURE IT IS USED, It may crash for lack of memory
{
  prt("dtf_ngram() - begininning - n=",n)
  stopifnot(1 <= n && n <= 3)

  prt("dtf_ngram() - calling build_dfm_ngrams(txts_merged, n) - n=",n)
  dfm_ngram <- build_dfm_ngrams(txts_merged, n)

  # textstats
  prt("dtf_ngram() - textstat_frequency(dfm_ngram) - n=",n)
  texstat_ngram <- textstat_frequency(dfm_ngram); rm(dfm_ngram); gc()

  prt("dtf_ngram() - setDT(texstat_ngram) - n=",n)

  setDT(texstat_ngram)
}


# --------------------------------------------------------------------
#   Global initialization - MUST BE IN Lib, to be passed when changing
#   data from full to subset and the other way round, that requires
#   reinitializing
#   Can NOT do very first initializations
#
pred_ngrams_re_init <- function() 
# --------------------------------------------------------------------  
{
  prt("start pred_ngrams_re_init()")

  set_parallelism(6,NULL)

  # read_dir <- read_dir()

  txts_merged <- NULL

  # data table frequencies
  dtf_1gram <- NULL ;dtf_2gram <- NULL ;dtf_3gram <- NULL

  
  prt("completed pred_ngrams_re_init()")
}


  
# --------------------------------------------------------------------
produce_ngram_bare_dtf_1 <- function() 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{
  rie(qc_full, readQCorp ,data_dir_corpus_in())
  feature <- "feature"

  prt("builfrequency data tables sigle-string _sep")
  rie(dtf_1gram ,dtf_ngram ,qc_full , 1)
  prt("splitting dts 1grams")
  
  if (feature %in% colnames(dtf_1gram)) {
    dt1_fun <- function(dt) { 
      setnames(dt, "feature", TYPES_COLNAMES[1])
    }
    rie(dtf_1gram_sep, dt1_fun, dtf_1gram)
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
produce_ngram_bare_dtf_2 <- function() 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{
  rie(qc_full, readQCorp ,data_dir_corpus_in())
  feature <- "feature"

  prt("splitting dts 2grams")
  rie(dtf_2gram ,dtf_ngram ,qc_full , 2)
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
    rie(dtf_2gram_sep , dt2_fun, dtf_2gram)
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
produce_ngram_bare_dtf_3 <- function() 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{
  rie(qc_full, readQCorp ,data_dir_corpus_in())
  feature <- "feature"


  rie(dtf_3gram ,dtf_ngram ,qc_full , 3)
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
    rie(dtf_3gram_sep , dt3_fun, dtf_3gram)
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
produce_ngram_bare_dtf <- function() 
# --------------------------------------------------------------------
{
  dtf_1gram_sep <- produce_ngram_bare_dtf_1()
  dtf_2gram_sep <- produce_ngram_bare_dtf_2()
  dtf_3gram_sep <- produce_ngram_bare_dtf_3()
  
  OK = T
  return(list(OK, dtf_1gram_sep, dtf_2gram_sep, dtf_3gram_sep))
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


# ====================================================================
#   Global initialization - MUST BE IN Lib, to be passed when changing
#   data from full to subset and the other way round, that requires
#   reinitializing
#   Can NOT do very first initializations
# ====================================================================
ngram_bare_re_init <- function() {
# --------------------------------------------------------------------  
  

  prt("start ngram_bare_re_init()")
  set_parallelism(6,NULL)

  read_dir <- read_dir()

  rie(qc_full,readQCorp,data_dir_corpus_in(), FALSE)
  qc_full <<- qc_full
 
  prt("completed ngram_bare_re_init()")
}



