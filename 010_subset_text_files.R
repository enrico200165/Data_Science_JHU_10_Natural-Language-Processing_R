# OBJECTIVE 
# EVGuess after long time: 
# subset corpora files by rading original files
# and writing subsetted copies with a subset of lines


require(DBI); 
require(RSQLite); 
require(sqldf)
require(quanteda) # for docvars
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

# ---------------------------------------------------------
buildWordsDBFromFile <- function(words_file, db_fname)
# ---------------------------------------------------------
# reads text file and put words in an sqlite table
{
  words_table <- "words_en"
  words_col = "words"
  
  con = dbConnect(drv=RSQLite::SQLite(), dbname = db_fname)
  
  tables = dbListTables(con)
  if (length(tables) == 0) {
    print("creating DB")
    dbExecute(conn = con,"CREATE TABLE :tab ( :col TEXT);",
              params = list(tab = words_table, col = words_col));
  } else {
    print(paste("table",words_table, "already exists"))
  }
  
  results <- dbGetQuery(con, paste("SELECT count(*) FROM "
                                   ,words_table,";"))
  res = as.numeric(results)
  # dbClearResult(con)
  if (is.na(res) | res <= 0) {
    print("filling DB from file")
    dbWriteTable(conn = con, name =  words_table
                 ,value = words_file, row.names = FALSE, header = FALSE
                 ,overwrite = T, sep="#")
    dbDisconnect(con)
    unlink(db_fname)
  } else {
    print(paste("DB",db_fname,"already loaded"))
  }
  
  dbDisconnect(con)
  unlink(db_fname)
}


# ---------------------------------------------------------
subsetFileName <- function(fname, descr) {
  # --------------------------------------------------------- 
  # derive file name for subset files, from "father" file name
  fname <- strsplit(fname,"\\.[^\\.]*$")[[1]]
  fnameOut <- paste(fname,"_subset",descr,".txt", sep="")
  fnameOut;
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
readTxtFileToStringVectors <- function(fname, nrLinesToRead) {
  # ---------------------------------------------------------
  enc <- getOption("encoding")
  con <- file(fname, "rt", encoding = enc)
  # enc <- iconvlist()[309] # utf8
  
  linesRead <- character(0)
  tryCatch({
    linesRead <- readLines(con, nrLinesToRead, skipNul = T)
  }
  ,error = function(x) { print("error"); print(x)}
  ,warning = function(x) { print("warning"); print(x)}
  ,finally = { close.connection(con); gc(); return(linesRead) }
  )
  
  linesRead
}



# --------------------------------------------------------------------
subsetTextFilesByLines <- function(in_dir, out_dir, nrLinesKept
                                   ,nrLinesRead, forceIt)
  # --------------------------------------------------------------------
{    
  print(paste("out dir:",out_dir," - in dir: ",in_dir))
  
  stopifnot(dir.exists(in_dir)) ;stopifnot(dir.exists(out_dir))
  
  textFiles <- list.files(in_dir ,include.dirs = FALSE)
  # remove dirs
  textFiles <- textFiles[!dir.exists(file.path(in_dir,textFiles))]
  
  if (length(textFiles) <= 0) {
    prt_error(paste("no files found in",in_dir))
    return(FALSE)
  }
  
  sapply(textFiles, FUN = function(x) { 
    subsetLines(x, in_dir, out_dir,nrLinesKept, nrLinesRead, forceIt); TRUE
  } )
  
  T
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
