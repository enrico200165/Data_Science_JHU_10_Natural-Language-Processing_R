
source("006_globals.R")
source("007_utils.R")
source("014_corpus.R")


# ====================================================================
#                         Unit Tests
# ====================================================================

# ---------------------------------------------------------
test_readQCorp <- function()
  # ---------------------------------------------------------
{
  corp_dir <- data_dir_corpus_in()
  rie(qc,readQCorp,,,corp_dir)
  return(qc)
}


# --------------------------------------------------------------------
test_corpus.R <- function()
  # --------------------------------------------------------------------
{
  
  fulldata <<- FALSE
  silent <<- T
  print(" --- Unit Testing --- ")
  
  qc <<- test_readQCorp()
  print(qc)
  
  print(" --- Tests Completed --- ")
}

#
clean_rds("qc")
test_corpus.R()

