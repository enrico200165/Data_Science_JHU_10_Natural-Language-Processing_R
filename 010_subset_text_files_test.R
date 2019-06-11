
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
  
  prt("subset files: ", list.files(data_dir_corpus_subset))
}


# --------------------------------------------------------------------
test_010_subset_text_files.R <- function()
  # --------------------------------------------------------------------
{
  
  fulldata <<- FALSE
  print(" --- Unit Testing --- ")
  
  test_subsetTextFilesByLines()
  
  print(" --- Tests Completed --- ")
}

#
test_010_subset_text_files.R()


