
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
