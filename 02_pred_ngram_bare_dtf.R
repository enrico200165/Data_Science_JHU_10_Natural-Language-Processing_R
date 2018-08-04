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
  
  # dfm_ret <- dfm(txts_par
  #   ,ngrams = n
  #   ,remove_numbers = T
  #   ,remove_punct = T
  #   ,remove_symbols = T
  #   ,remove_separators = T
  #   ,remove_twitter = T
  #   ,remove_hyphens = FALSE
  #   ,remove_url = FALSE,
  #   )

    dfm_ret <- dfm(txts_par
    ,ngrams = n
    ,remove_numbers = F
    ,remove_punct = F
    ,remove_symbols = F
    ,remove_separators = F
    ,remove_twitter = F
    ,remove_hyphens = F
    ,remove_url = F,
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
produce_ngram_bare_dtf <- function() 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{

  prt("reading files into single text")
  rie(txts_merged ,read_texts ,data_dir_corpus_in())

  
  feature <- "feature"

  prt("builfrequency data tables sigle-string _sep")
  rie(dtf_1gram ,dtf_ngram ,txts_merged , 1)
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
  # mem_health(c("dtf_1gram_sep","dtf_2gram_sep","dtf_3gram_sep"))
  if (nrow(dtf_1gram) != nrow(dtf_1gram_sep))
    prt_error("different nr rows splitting unigram:",nrow(dtf_sstring1),nrow(dtf_1gram_sep))
  kill_var(dtf_1gram)
  
  
  prt("splitting dts 2grams")
  rie(dtf_2gram ,dtf_ngram ,txts_merged , 2)
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
    prt("different nr rows splitting unigram:",nrow(dtf_sstring2),nrow(dtf_2gram_sep))
  kill_var(dtf_2gram)
  mem_health(c("dtf_1gram_sep","dtf_2gram_sep","dtf_3gram_sep"))
  
  
  rie(dtf_3gram ,dtf_ngram ,txts_merged , 3)
  prt("splitting dts 3grams")
  if (feature %in% colnames(dtf_3gram)) {

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
    rie(dtf_3gram_sep , dt3_fun, dtf_3gram)
    assign("dtf_3gram_sep" ,dtf_3gram_sep, .GlobalEnv)
  } else {
    prt_error("feature col not found")
    stop()
  }
  if (nrow(dtf_3gram) != nrow(dtf_3gram_sep))
    prt("different nr rows splitting unigram:",nrow(dtf_sstring3),nrow(dtf_3gram_sep))
  kill_var(dtf_3gram)
  mem_health(c("dtf_sep_1gram","dtf_sep_2gram","dtf_sep_3gram"))
  
  kill_var(txts_merged)

  list(
    dtf_1gram = dtf_1gram
   ,dtf_2gram = dtf_2gram
   ,dtf_3gram = dtf_3gram
    )
  
  }


