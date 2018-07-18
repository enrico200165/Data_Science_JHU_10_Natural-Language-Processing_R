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
build_dfms <- function(data_dir_corpus) 
# --------------------------------------------------------------------
# https://stackoverflow.com/questions/20345022/convert-a-data-frame-to-a-data-table-without-copy
#
{
  
  rie(txts_merged ,read_texts ,data_dir_corpus)
  
  rie(dtf_1gram ,dtf_ngram ,txts_merged , 1)
  dtf_1gram <<- dtf_1gram

  
  rie(dtf_2gram ,dtf_ngram ,txts_merged , 2)
  dtf_2gram <<- dtf_2gram

  rie(dtf_3gram ,dtf_ngram ,txts_merged , 3)
  dtf_3gram <<- dtf_3gram

  return(T)
    
  # --------------- SAFE WAY ------------
  rie(dfm_1gram ,build_dfm_ngrams ,txts_merged, 1)
  dfm_1gram <<- dfm_1gram
  # textstats 
  rie(texstat_1gram, textstat_frequency, dfm_1gram)
  # get the data table
  rie(dtf_1gram, data.table, texstat_1gram)
  dtf_1gram <<- dtf_1gram
  

  rie(dfm_2gram ,build_dfm_ngrams ,txts_merged, 2)  
  dfm_2gram <<- dfm_2gram
  # textstats 
  rie(texstat_2gram, textstat_frequency, dfm_2gram)
  # get the data table
  rie(dtf_2gram, data.table, texstat_2gram)
  dtf_2gram <<- dtf_2gram

  
  rie(dfm_3gram ,build_dfm_ngrams ,txts_merged, 3)  
  dfm_3gram <<- dfm_3gram
  # textstats 
  rie(texstat_3gram, textstat_frequency, dfm_3gram)
  # get the data table
  rie(dtf_3gram, data.table, texstat_3gram)
  dtf_3gram <<- dtf_3gram
  
  gc()
  
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

  read_dir <- read_dir()

  prt("completed pred_ngrams_re_init()")
}


# --------------------------------------------------------------------  
  main <- function()
# --------------------------------------------------------------------
{

  build_dfms(data_dir_corpus_in())
}



fulldata <- F
silent <- F

txts_merged <- NULL

dfm_1gram <- NULL
dfm_2gram <- NULL
dfm_3gram <- NULL


dtf_1gram <- NULL # data table 1gram frequencies
dtf_2gram <- NULL
dtf_3gram <- NULL



pred_ngrams_re_init()
main()
prt("debug")
prt("debug")

