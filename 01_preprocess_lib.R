# Sources
# https://rpubs.com/lmullen/nlp-chapter
# http://www.mjdenny.com/Text_Processing_In_R.html

# install.packages("stringr", dependencies = TRUE)
require(quanteda)
require(stringr)
require(readtext)
require(dplyr)

#######################################################
#           DICTIONARY
#######################################################
# from https://github.com/dwyl/english-words

########################  DB ###########################
# https://www.r-bloggers.com/using-sqlite-in-r/
# install.packages("RSQLite", dependencies = TRUE)
# install.packages("sqldf")
library(DBI); library(RSQLite); library(sqldf)


source("01_globals.R")

# ---------------------------------------------------------
  buildWordsDBFromFile <- function(words_file, db_fname) {
# ---------------------------------------------------------
# reads text file and put words in an sqlite table
    
    
# Write to DB EN word list
    
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
  subsetTextFile <- function(fname, ndCharsToRead) {
# ---------------------------------------------------------
# abandoned for a simpler solution, for now    
    fnameOut <- subsetFileName(fname)
    print(paste("subsetting", fname, " to ", fnameOut))
    fileDf <-
      data.frame(
        fname = c(
          "./data_in/data-set/en_US/en_US.blogs.txt"
          ,
          "./data_in/data-set/en_US/en_US.news.txt"
          ,
          "./data_in/data-set/en_US/en_US.twitter.txt"
        )
      )
    fileDf$fname <- as.character(fileDf$fname)
    
    fileDf$fsizes <- file.size(fileDf$fname)
    totBytes <- sum(fileDf$fsizes)
    fileDf$prop <- fileDf$fsizes / totBytes
    totMB <- totBytes / (1024 * 1024)
    print(paste("tot size: ", round(totMB, 0)))
    bytesToReadTotal = 10 * 1024 * 1024
    fileDf$bytesToRead = bytesToReadTotal * fileDf$prop
    
    # read contiguous blocks
    NrContiguousLines = 100
    
    for (fi in 1:nrow(fileDf)) {
      f <- fileDf[fi,]
      con <- file(f$fname, "r")
      # readLines(con, 5) ## Read in the next 5 lines of text
      bytesRead = 0
      finishedRead = FALSE
      while (!finishedRead) {
        linesRead = readLines(con, n = 100)
        for (l in linesRead) {
          if (finishedRead)
            next
          llen <- nchar(l)
          if (llen == 0) {
            close(con)
            finishedRead = TRUE
            next
          }
          if ((bytesRead = bytesRead + llen) >= f$bytesToRead) {
            close(con)
            print(paste("read enough bytes: ",bytesRead," planned: ",f$bytesToRead))
            finishedRead = TRUE
            next
          }
          # print(l)
        }#for lines
      } #while true
    }#for f
  }
  

# ---------------------------------------------------------
  subsetLines <- function(fname, nrLinesKept, nrLinesRead) {
# ---------------------------------------------------------
    descr <- paste0("_", nrLinesKept, "_", nrLinesRead)
    fnameOut <- subsetFileName(fname, descr)
    cat("subsetting\n", fname, " to\n", fnameOut)

    # enc <- iconvlist()[309] # utf8
    enc <- getOption("encoding")
    con <- file(fname, "rt", encoding = enc)
    out <- file(fnameOut, "wt",encoding = enc)
    
    tryCatch({
    finishedRead = FALSE
    while (!finishedRead) {
      linesRead = readLines(con, nrLinesRead, skipNul = T)
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
    },
    error = function(x) { print("error"); print(x)},
    warning = function(x) { print("warning"); print(x)},
    finally = {
      close.connection(con)
      gc() 
      return(linesRead)
    }
  )
}
  
  
  
# ---------------------------------------------------------
  subsetTextFilesByLines <- function(targetDir,nrLinesKept, nrLinesRead) {
# ---------------------------------------------------------
    
  print(paste("workdir:",getwd(),"looking for files in dir: ",targetDir))
  textFiles <- list.files(targetDir)
  if (length(textFiles) <= 0) {
    print(paste("no files found in",targetDir))
    return(FALSE)
  }
  textFiles <- grep("en.*",textFiles,value = TRUE)
  # textFiles <- grep(".*tw.*",textFiles,value = TRUE)
  textFiles <- grep("subset",textFiles,value = TRUE,invert = T)
  
  textFiles <- file.path(data_dir_corpus, textFiles)
  sapply(textFiles, FUN = function(x) { 
    subsetLines(x, nrLinesKept, nrLinesRead); TRUE
    } )
} 
    

enricoReadText <- function(fname, nrLinesToRead, replaceNewLine) {
  # read
  lines <- readTxtFileToStringVectors(fname,nrLinesToRead)
  
  # blocca il PC
  # lines <- gsub("\n", "\r\n", lines)
  
  nrLines <- length(lines)
  i <- 0
  vars <- strsplit(basename(fname),"[_.]")[[1]]
  lang <- rep(vars[inc(i)], nrLines)
  source <- rep(vars[inc(i)], nrLines)
  inc(i) # country code duplicates language???
  inc(i) # subset
  l1 <- vars[inc(i)]
  l2  <- vars[inc(i)]
  pctage <- rep(as.numeric(l1)*100/as.numeric(l2), nrLines)
  dvars = data.frame(lang = lang, source = source,pctage = pctage)

  docnames <- rep(basename(fname),nrLines)
  
  return(list(lines = lines, vars = dvars, dnames = docnames))
}


# -----------------------------------------------
  readtextIfEmpty <- function(mydf, in_dir, nomeFile) {
# -----------------------------------------------
  varName <- deparse(substitute(mydf))
  
  if (!missing(in_dir) & !is.na(in_dir) & nchar(in_dir) > 0) {
    nomeFile <- paste0(in_dir,"/",nomeFile)
  }

  exists = TRUE
  exists <- exists && exists(varName)
  filled <- exists && (object.size(mydf) > 256)
  
  hasSerialization = TRUE
  rdsFName <- paste0(SERIAL_PREFIX,varName,".rds")
  hasSerialization <- hasSerialization && file.exists(rdsFName)
  
  if (filled) {
    print(paste(varName,"exists and is filled, do nothing"))
    return(mydf)
  }
  
  if (!exists | !filled) {
    
    if (hasSerialization) {
      print(paste(varName,"reading serialization"))
      mydf <- readRDS(rdsFName)
      # assign(varName,mydf,.GlobalEnv) # pass it outside,
      assign(varName,mydf,envir=parent.frame(n = 1))
      return(mydf)
    } else {
      print(paste(varName,"reading file from",nomeFile," and writing serialization"))
      my_rt <- readtext(nomeFile, docvarsfrom = "filenames"
                       ,docvarnames = c("lng","country","type","b","c","d")
                       ,dvsep = "[_.]", encoding = "UTF-8"
                       , verbosity = 3)
      
      # cannot set docvars like in a corpus with docvars(corpus) <- 
      # this is not a corpus, is a df, more basically
      print(head(my_rt,1))
      sampled_pctg <- as.numeric(my_rt[7])*100/as.numeric(my_rt[8])
      my_rt <- select(my_rt,-c(6:ncol(my_rt)))
      my_rt$sample_pctg <- sampled_pctg
      # print(docvars(mydf))
      
      mydf <- corpus(my_rt)
      # print(paste("blogs",format(object.size(my_rt), units = "MiB")))      
    }
  }
  if (!file.exists(rdsFName))
    saveRDS(mydf, file = rdsFName)
  return(mydf)
}


# ---------------------------------------------------------
  readInQCorp <- function(data_dir_corpus, subsetPar) {
# ---------------------------------------------------------
# lazy reads files matching regex into a corpus
  filesInDir <- list.files(data_dir_corpus)
  # print(filesInDir)
  # files2readBool <- str_detect(filesInDir, file_regex)
  
  s <- if(subsetPar) {"subset*"} else {"full*"}
  qc_blogs <<- readtextIfEmpty(qc_blogs,data_dir_corpus
                               ,paste0("en*blogs*",s))
  qc_news  <<- readtextIfEmpty(qc_news,data_dir_corpus
                               ,paste0("en*news*",s))
  qc_twitts <<- readtextIfEmpty(qc_twitts,data_dir_corpus
                                ,paste0("en*twitt*",s))
}


  
# ---------------------------------------------------------
#     Test
# ---------------------------------------------------------

# 
# 

myTest <- function() {
  ptm <- proc.time()
  print(list.files(data_dir_corpus))
  readInQCorp(data_dir_corpus, FALSE)
  print("time")
  print(proc.time() - ptm)
  # print(docvars(qc_blogs))
  print(paste("blogs",format(object.size(qc_blogs), units = "MiB")))
  print(paste("news",format(object.size(qc_news), units = "MiB")))
  print(paste("twitts",format(object.size(qc_twitts), units = "MiB")))
}

#myTest()

