# OBJECTIVE 
# EVGuess after long time: 
# subset corpora files by rading original files
# and writing subsetted copies with a subset of lines


require(readtext)
require(pryr)

#######################################################
#           DICTIONARY
#######################################################
# from https://github.com/dwyl/english-words

########################  DB ###########################
# https://www.r-bloggers.com/using-sqlite-in-r/
# install.packages("RSQLite", dependencies = TRUE)
# install.packages("sqldf")


source("006_globals.R")

# --------------------------------------------------------------------
subsetTextFilesByLines <- function(in_dir, out_dir, nrLinesKept
                                   ,nrLinesRead, forceIt)
  #' @param in_dir 
  #' @param out_dir 
  #' @param nrLinesKept  lines inserted in subset
  #' @param nrLinesRead  lines read in each chunk
  #' @param forceIt 
  #'
  #' @return T if ok
{    
  print(paste("out dir:",out_dir," - in dir: ",in_dir))
  
  stopifnot(dir.exists(in_dir)) ;stopifnot(dir.exists(out_dir))
  
  textFiles <- list.files(in_dir, pattern = ".*\\.txt" ,include.dirs = FALSE)
  # remove dirs
  textFiles <- textFiles[!dir.exists(file.path(in_dir,textFiles))]
  
  if (length(textFiles) <= 0) {
    prt_error(paste("no files found in",in_dir))
    return(FALSE)
  }
  
  # clean destination directory
  sapply(list.files(out_dir)
         ,function(x) unlink(file.path(out_dir,x)))
  
  sapply(textFiles, FUN = function(x) { 
    subsetLines(x, in_dir, out_dir,nrLinesKept, nrLinesRead, forceIt); TRUE
  } )
  
  T
} 


###########################################################
#             PRIVATE
###########################################################


# ---------------------------------------------------------
subsetFileName <- function(fname, descr) {
  # --------------------------------------------------------- 
  # derive file name for subset files, from "father" file name
  fname <- strsplit(fname,"\\.[^\\.]*$")[[1]]
  fnameOut <- paste(fname,"_subset",descr,".txt", sep="")
  fnameOut
}


# ---------------------------------------------------------
subsetLines <- function(fname, in_dir, out_dir, nrLinesKept
                        ,nrLinesRead,forceIt) {
# ---------------------------------------------------------
  
  stopifnot(dir.exists(in_dir))
  stopifnot(dir.exists(out_dir))
  stopifnot(nrLinesKept <= nrLinesRead)
  
  descr <- paste0("_", nrLinesKept, "_", nrLinesRead)
  # remove/clean up eventual prexisting subset
  fnameOut <- gsub("_?subset.*\\.", ".", fname)
  fnameOut <- subsetFileName(fnameOut, descr)
  fnameOut <- file.path(out_dir,fnameOut)
  fname <- file.path(in_dir,fname)
  stopifnot(file.exists(fname))
  
  prt("subsetting\n", fname, " to\n", fnameOut)
  if (!forceIt && file.exists(fnameOut)) {
    print(paste("subset already exists, not overwriting it:",fnameOut))
    return(TRUE)
  }
  
  # enc <- iconvlist()[309] # utf8
  enc <- getOption("encoding")
  con <- file(fname, "rt", encoding = enc)
  out <- file(fnameOut, "wt",encoding = enc)
  
  tryCatch({
    finishedRead = FALSE
    while (!finishedRead) {
      linesRead = readLines(con, nrLinesRead, skipNul = T
                            ,encoding=enc)
      i = 1
      for (l in linesRead) {
        writeLines(l, out)
        i = i+1
        if (i > nrLinesKept) {
          #print(paste("read lines:",nrLinesRead, " i = ", i,"stop writing"))
          break
        }
      }
      if (length(linesRead) < nrLinesRead) {
        finishedRead = TRUE
      }
    }
  },
  error = function(x) { print("error"); print(x)},
  warning = function(x) { print("warning"); print(x)},
  finally = {
    close.connection(out)
    close.connection(con)
    gc() 
  }
  )
  TRUE
}




# ---------------------------------------------------------
checkUSANewsFileProblem <- function(fname, nrLinesToRead)
# ---------------------------------------------------------
{
  stopifnot(file.exists(fname))
  
  enc <- getOption("encoding")
  con <- file(fname, "rt", encoding = enc)
  # enc <- iconvlist()[309] # utf8
  
  linesRead <- character(0)
  tryCatch({
    linesRead <- readLines(con, nrLinesToRead, skipNul = T)
  }
  ,error = function(x) { print("error"); print(x)}
  ,warning = function(x) { print("warning"); print(x)}
  ,finally = { 
    last_line_read <- linesRead[length(linesRead)]
    prt("read nr. lines: ",length(linesRead)
        ,"last line read:", substr(last_line_read,1,32)
        ,"length",nchar(last_line_read))
    close.connection(con); gc(); return(linesRead) 
  }
  )
  
  last_line_read <- linesRead[length(linesRead)]
  prt("read nr. lines: ",length(linesRead)
      ,"last line read:", substr(last_line_read,1,32)
      ,"length",length(last_line_read))
  
  linesRead
}


###########################################################
#                 TEMPORARY TEST
###########################################################
