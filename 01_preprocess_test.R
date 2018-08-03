


source("01_preprocess_lib.R")

# ====================================================================
#                         Unit Tests
# ====================================================================



# --------------------------------------------------------------------
  test_01_preprocess_libs.R <- function()
# --------------------------------------------------------------------
{

  print(" --- Unit Testing --- ")

  silent <<- F

  T && subsetTextFilesByLines(data_dir_corpus_full 
      ,data_dir_corpus_subset ,5,10000 , forceIt = F)


  if(T) {
    print(list.files(data_dir_corpus_full))

    start_time <- proc.time()
    readQCorp(data_dir_corpus_full, FALSE)
    end_time <- proc.time()
    time_spent_with_serialization <- round(end_time - start_time,2)
    print(paste("exec time",paste(time_spent_with_serialization ,collapse = " ")))
  }
  
  print(" --- Tests Completed --- ")
}

#
  test_01_preprocess_libs.R()


