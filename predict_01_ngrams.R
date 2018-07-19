require(readtext)
require(data.table)

source("01_globals.R")
# source("01_preprocess_lib.R")



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
split_ngrams_dts <- function(dft_sstring1 ,dft_sstring2 ,dft_sstring3)
# --------------------------------------------------------------------
# from ngram frequency data table with ngram in single string with
# _ separator build data tables with 1 column for each type
# This SHOULD save space in memory as identical strings are not replicated
{
 
  # 1grams
  dt1_fun <- function(dt) { setnames(dt,"feature", "first") }
  rie(dtf_sep_1gram , dt1_fun, dft_sstring1)# rie(dtf_sep_1gram , dt1, dtf_1gram)
  dtf_sep_1gram <<- dtf_sep_1gram
  
  # 2grams
  # kill_var(dtf_sep_2gram)
dt2_fun <- function(dt) { 
  dt[ ,c("first", "second") := tstrsplit(feature, "_", fixed=TRUE)]
  dt[ , feature := NULL]
}
rie(dtf_sep_2gram , dt2_fun, dft_sstring2)


  # kill_var(dtf_sep_3gram)
dt3_fun <- function(dt) { 
  dt[ ,c("first", "second" , "third") := tstrsplit(feature, "_", fixed=TRUE)]
  dt[ , feature := NULL]
}
rie(dtf_sep_3gram , dt3_fun, dft_sstring2)

  list(dft_sstring1 = dft_sstring1
    ,dft_sstring2 = dft_sstring2
    ,dft_sstring3 = dft_sstring3)
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

  dfm_1gram <- NULL ;dfm_2gram <- NULL ;dfm_3gram <- NULL
  
  # data table frequencies
  dtf_1gram <- NULL ;dtf_2gram <- NULL ;dtf_3gram <- NULL

  
  prt("completed pred_ngrams_re_init()")
}


# --------------------------------------------------------------------  
  main <- function()
# --------------------------------------------------------------------
{

  prt("reading files into single text")
  rie(txts_merged ,read_texts ,data_dir_corpus_in())
  
  prt("building frequency data tables sigle-string _sep")
  ret_dts <- build_dtfs(txts_merged)
  
  prt("splitting frequency data tables ngrams")
  split_ngrams_dts(ret_dts[[1]][1:10,] ,ret_dts[[2]][1:10,] ,ret_dts[[3]][1:10,])

  prt("finished")
  
  print(debug)
}



fulldata <- F
silent <- F



use_full_corpus(T,pred_ngrams_re_init)
#pred_ngrams_re_init()
main()

