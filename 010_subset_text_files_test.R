
source("007_utils.R")
source("010_subset_text_files.R")


# ====================================================================
#                         Unit Tests
# ====================================================================


# --------------------------------------------------------------------
test_subsetTextFilesByLines <- function()
# --------------------------------------------------------------------
{
  silent <<- F
  
  # remove existing files
  unlink(file.path(data_dir_corpus_subset,"*.txt"))
  if (length(list.files(data_dir_corpus_subset)) > 0) {
    prt("failed to clean directory: ", data_dir_corpus_subset)
  }
  
  T && subsetTextFilesByLines(data_dir_corpus_full 
                              ,data_dir_corpus_subset ,10,10000 , forceIt = T)
  
  if(F) {
    print(list.files(data_dir_corpus_full))
    
    start_time <- proc.time()
    readQCorp(data_dir_corpus_full, FALSE)
    end_time <- proc.time()
    time_spent_with_serialization <- round(end_time - start_time,2)
    print(paste("exec time",paste(time_spent_with_serialization ,collapse = " ")))
  } 
  
  prt("subset files: ", list.files(data_dir_corpus_subset))
}


# ---------------------------------------------------------
test_readQCorp <- function()
# ---------------------------------------------------------
{
    return(readQCorp(data_dir_corpus_in()))
}


# --------------------------------------------------------------------
  test_01_preprocess_libs.R <- function()
# --------------------------------------------------------------------
{

  fulldata <<- FALSE
  print(" --- Unit Testing --- ")

  test_subsetTextFilesByLines()

  print(" --- Tests Completed --- ")
}

#
  test_01_preprocess_libs.R()

