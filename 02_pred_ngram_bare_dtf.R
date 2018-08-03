require(dplyr)
require(data.table)

require(quanteda)
require(readtext)

source("01_globals.R")
source("02_pred_globals.R")


# ####################################################################
#                       MODULE MISSION
# from text files produce ngrams data.tables - nothing more
# 
# ####################################################################
#                     EXTERNAL FUNCIONS
#
#  produce_ngram_bare_dtf()
#
######################################################################


# --------------------------------------------------------------------
read_texts <- function(data_dir_corpus) 
# --------------------------------------------------------------------
# lazy reads text files matching pattern into a single Quanteda corpus
{    
  prt("reading from dir:",data_dir_corpus)
  stopifnot(dir.exists(data_dir_corpus))

  prt("reading text files, fulldata", fulldata)
  read_pattern <- file.path(data_dir_corpus,"en_US*txt")
  txt <- readtext(read_pattern)
  
  prt("pasting texts into single huge text")
  full_txt <- paste(texts(txt), collapse = ". ")
  rm(txt); invisible(gc())

  full_txt
}


# --------------------------------------------------------------------
build_dfm_ngrams <- function(txts_par, n) 
# --------------------------------------------------------------------
{
  
  # stopifnot(class)
  
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
  
  stopifnot(1 <= n && n <= 3)
  
  dfm_ngram <- build_dfm_ngrams(txts_merged, n)
  # textstats
  texstat_ngram <- textstat_frequency(dfm_ngram)
  # get the data table
  dtf_ngram <- data.table(texstat_ngram)
}


# --------------------------------------------------------------------
build_dtfs <- function(txts_merged) 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{
  rie(dtf_1gram ,dtf_ngram ,txts_merged , 1)
  dtf_1gram <<- dtf_1gram
  
  rie(dtf_2gram ,dtf_ngram ,txts_merged , 2)
  dtf_2gram <<- dtf_2gram

  rie(dtf_3gram ,dtf_ngram ,txts_merged , 3)
  dtf_3gram <<- dtf_3gram

  gc()
  
  list(
    dtf_1gram = dtf_1gram
   ,dtf_2gram = dtf_2gram
   ,dtf_3gram = dtf_3gram
    )
}


# --------------------------------------------------------------------
split_ngrams_dts <- function(dtf_sstring1 ,dtf_sstring2 ,dtf_sstring3)
# --------------------------------------------------------------------
# from ngram frequency data table with ngram in single string with
# _ separator build data tables with 1 column for each type
# This SHOULD save space in memory as identical strings are not replicated
{
  feature <- "feature"
  prt("splitting dts 1grams")
  
  if (feature %in% colnames(dtf_sstring1)) {
    dt1_fun <- function(dt) {
      setnames(dt, "feature", TYPES_COLNAMES[1])
    }
    rie(dtf_1gram_sep, dt1_fun, dtf_sstring1)
  } else {
    prt("feature col not found")
  }
  dtf_1gram_sep <<- dtf_1gram_sep
  # mem_health(c("dtf_1gram_sep","dtf_2gram_sep","dtf_3gram_sep"))
  if (nrow(dtf_sstring1) != nrow(dtf_1gram_sep))
    prt_error("different nr rows splitting unigram:",nrow(dtf_sstring1),nrow(dtf_1gram_sep))
  
  # 2grams
  prt("splitting dts 2grams")
  if (feature %in% colnames(dtf_sstring2)) {
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
    rie(dtf_2gram_sep , dt2_fun, dtf_sstring2)
    dtf_2gram_sep <<- dtf_2gram_sep
  } else {
    prt_error("feature col not found")
  }
  if (nrow(dtf_sstring2) != nrow(dtf_2gram_sep))
    prt("different nr rows splitting unigram:",nrow(dtf_sstring2),nrow(dtf_2gram_sep))
  mem_health(c("dtf_1gram_sep","dtf_2gram_sep","dtf_3gram_sep"))
  

  # kill_var(dtf_3gram_sep)
  prt("splitting dts 3grams")
  if (feature %in% colnames(dtf_sstring3)) {

    dt3_fun <- function(dt) {
          
      splits <- strsplit(dt$feature,"_" ,fixed = T)
      prim <- sapply(splits,function(x) x[1])
      sec  <- sapply(splits,function(x) x[2])
      ter  <- sapply(splits,function(x) x[3])

      # dt[ , TYPES_COLNAMES[1:3] := list(prim, sec , ter) ] # seems to corrupt memory
      dt[ , `:=`(primo = prim, secondo = sec , terzo = ter) ]
      dt[, feature := NULL]
      rm(splits , prim , sec , ter); gc()

      dt
    }
    rie(dtf_3gram_sep , dt3_fun, dtf_sstring3)
  } else {
    prt_error("feature col not found")
  }
  dtf_3gram_sep <<- dtf_3gram_sep
  if (nrow(dtf_sstring3) != nrow(dtf_3gram_sep))
    prt("different nr rows splitting unigram:",nrow(dtf_sstring3),nrow(dtf_3gram_sep))
  mem_health(c("dtf_sep_1gram","dtf_sep_2gram","dtf_sep_3gram"))
  
  # list(
  #    dtf_1gram_sep = dtf_1gram_sep
  #   ,dtf_2gram_sep = dtf_2gram_sep
  #   ,dtf_3gram_sep = dtf_3gram_sep
  #)
  
  T
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
  produce_ngram_bare_dtf <- function(n)
# --------------------------------------------------------------------
{

  prt("reading files into single text")
  rie(txts_merged ,read_texts ,data_dir_corpus_in())
  
  prt("building frequency data tables sigle-string _sep")
  ret_dts <- build_dtfs(txts_merged)
  prt("number of features in 1grams, 2grams, 3grams",nrow(ret_dts[[1]])
      ,nrow(ret_dts[[2]]),nrow(ret_dts[[3]]))
  
  prt("splitting frequency data tables ngrams")
  split_ngrams_dts(ret_dts[[1]], ret_dts[[2]] ,ret_dts[[3]])

  kill_var(txts_merged)
  kill_var(dtf_1gram);
  kill_var(dtf_2gram) ; 
  kill_var(dtf_3gram) ; 
  gc()

}

# clean_rds()
# rm(list=ls(all=TRUE))
# produce_ngram_bare_dtf()