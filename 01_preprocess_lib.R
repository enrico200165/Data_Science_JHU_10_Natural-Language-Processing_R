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
require(DBI); 
require(RSQLite); 
require(sqldf)


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
  subsetLines <- function(fname, in_dir, out_dir, nrLinesKept
  ,nrLinesRead,forceIt) {
# ---------------------------------------------------------

    stopifnot(all(dir.exists(in_dir),dir.exists(out_dir)))
    
    descr <- paste0("_", nrLinesKept, "_", nrLinesRead)
    fnameOut <- subsetFileName(fname, descr)
    fnameOut <- file.path(out_dir,fnameOut)
    fname <- file.path(in_dir,fname)
    stopifnot(file.exists(fname))
    cat("subsetting\n", fname, " to\n", fnameOut)
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
  subsetTextFilesByLines <- function(in_dir, out_dir, nrLinesKept
    ,nrLinesRead, forceIt) {
# ---------------------------------------------------------
    
  print(paste("out dir:",out_dir," - in dir: ",in_dir))
  stopifnot(dir.exists(in_dir))
  stopifnot(dir.exists(out_dir))
  textFiles <- list.files(in_dir)
  if (length(textFiles) <= 0) {
    print(paste("no files found in",in_dir))
    return(FALSE)
  }
  # textFiles <- grep("en.*",textFiles,value = TRUE)
  # textFiles <- grep(".*tw.*",textFiles,value = TRUE)
  textFiles <- grep("subset",textFiles,value = TRUE,invert = T)
  
  # textFiles <- file.path(data_dir_corpus, textFiles)
  sapply(textFiles, FUN = function(x) { 
    subsetLines(x, in_dir, out_dir,nrLinesKept, nrLinesRead, forceIt); TRUE
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
  readtextIfEmpty <- function(mydf, in_dir, nFile) {
# -----------------------------------------------
  varName <- deparse(substitute(mydf))
  
  if ( missing(in_dir) || is.na(in_dir) || (nchar(in_dir) <= 0)
    ) {
    nomeFile <- nFile
  } else {
    stopifnot(dir.exists(in_dir))
    nomeFile <- paste0(in_dir,"/",nFile)
  }

  
  exists <- exists(varName)
  filled <- exists && (!is.null(nrow(mydf)) &&  nrow(mydf)> 0)
  if (filled) {
    print(paste(varName,"exists and is filled, do nothing"))
    return(mydf)
  }
  
  if (!exists || !filled) {
    rdsFName <- paste0(SERIAL_PREFIX,nFile,".rds")
    hasSerialization <- file.exists(rdsFName)
    if (hasSerialization) {
      print(paste(varName,"reading serialization from:",rdsFName))
      mydf <- readRDS(rdsFName)
      # assign(varName,mydf,.GlobalEnv) # pass it outside,
      assign(varName,mydf,envir=parent.frame(n = 1))
      return(mydf)
    } 
    if (is.null(nomeFile) || length(nomeFile) <= 0  || !file.exists(nomeFile)) {
        print(paste("no files found for:",nomeFile))
         return(NA)
    }
    print(paste(varName,"reading file from",nomeFile," and writing serialization"))
    # split1 <- strsplit(nFile,"_")[[1]]
    # split2 <- strsplit(split1[2],".",fixed=T)[[1]]
    # docv <- data.frame(lng = split2[1], type = split2[1])
    
    my_rt <- readtext(nomeFile
    , docvarsfrom = "filenames", docvarnames = c("country","lng","type",
                                                 "dummy1", "lines_in", "lines_tot") ,dvsep = "[_.]"
                      , encoding = "UTF-8"
                       , verbosity = 3)
    #docvars(my_rt) <- docv
    # cannot set docvars like in a corpus with docvars(corpus) <- 
    # this is not a corpus, is a df, more basically
    print(head(my_rt,1))
    sampled_pctg <- 100*docvars(my_rt)$lines_in/docvars(my_rt)$lines_tot
    my_rt <- select(my_rt,-c(6:ncol(my_rt)))
    my_rt$sample_pctg <- sampled_pctg
    my_rt
    # print(docvars(mydf))
    # mydf <- corpus(my_rt)
    # print(single Quanteda paste("blogs",format(text object.size(my_rt), units = "MiB")))text       
  }
  
  
  if (!file.exists(rdsFName)) {
    print(paste("saving serialization to: ",rdsFName))
    saveRDS(my_rt, file = rdsFName)
  }

  my_rt
}

# Reads a single file for pattern
readtextIfEmpty_Wrapper <- function(text_df,data_dir_corpus, fnamePattern) {
  
  stopifnot(dir.exists(data_dir_corpus))
  
  fname <- list.files(data_dir_corpus,paste0(fnamePattern,".*\\.txt"))
  stopifnot(length(fname) == 1)
  
  readtextIfEmpty(text_df,data_dir_corpus,fname)
}

# --------------------------------------------------------------------
readInQCorp <- function(data_dir_corpus, subsetPar) 
# --------------------------------------------------------------------
# lazy reads text files matching pattern into a single Quanteda corpus
{    
  
  print(paste("readInQCorp",data_dir_corpus))
  stopifnot(dir.exists(data_dir_corpus))
  filesInDir <- list.files(data_dir_corpus,"*bset*"); print(filesInDir)
  
  
  en_US_blogs   <- readtextIfEmpty_Wrapper(en_US_blogs,data_dir_corpus,  "en_US.blogs")
  texts_df <- en_US_blogs
  en_US_news    <- readtextIfEmpty_Wrapper(en_US_news,data_dir_corpus,   "en_US.news")
  texts_df <- bind_rows(texts_df,en_US_news)
  en_US_twitter <- readtextIfEmpty_Wrapper(en_US_twitter,data_dir_corpus,"en_US.twitter")
  texts_df <- bind_rows(texts_df,en_US_twitter)
  
  de_DE_blogs   <- readtextIfEmpty_Wrapper(de_DE_blogs,data_dir_corpus,  "de_DE.blogs")
  texts_df <- bind_rows(texts_df, de_DE_blogs)
  de_DE_news    <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus,   "de_DE.news")
  texts_df <- bind_rows(texts_df, de_DE_news)
  de_DE_twitter <- readtextIfEmpty_Wrapper(de_DE_twitter,data_dir_corpus,"de_DE.twitter")
  texts_df <- bind_rows(texts_df, de_DE_twitter)
  
  fi_FI_blogs   <- readtextIfEmpty_Wrapper(fi_FI_blogs,data_dir_corpus,"fi_FI.blogs")
  texts_df <- bind_rows(texts_df, fi_FI_blogs)
  fi_FI_news    <- readtextIfEmpty_Wrapper(fi_FI_news,data_dir_corpus, "fi_FI.news")
  texts_df <- bind_rows(texts_df, fi_FI_news)
  fi_FI_twitter <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus, "fi_FI.twitter")
  texts_df <- bind_rows(texts_df, fi_FI_twitter)
  
  ru_RU_blogs   <- readtextIfEmpty_Wrapper(ru_RU_blogs,data_dir_corpus,"ru_RU.blogs")
  texts_df <- bind_rows(texts_df, ru_RU_blogs)
  ru_RU_news    <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus, "ru_RU.news")
  texts_df <- bind_rows(texts_df, ru_RU_news)
  ru_RU_twitter <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus, "ru_RU.twitter")
  texts_df <- bind_rows(texts_df, ru_RU_twitter)
  
  corpus(texts_df)
}



# --------------------------------------------------------------------
#                         Unit Tests
# --------------------------------------------------------------------


unitTests <- function() {

  print(" --- Unit Testing --- ")
    
  #if (T) {
  if (F) {
    subsetTextFilesByLines(data_dir_corpus_in
                           ,data_dir_corpus_work
                           ,5,1000, F)
  }

  if (T) {
    qc <- readInQCorp(data_dir_corpus_work, FALSE)
    smr <- summary(qc)
    rm(qc)
    print(str(smr))
    print("")
  }  
  
#  if(T) {
  if(F) {
    ptm <- proc.time()
    print(list.files(data_dir_corpus))
    readInQCorp(data_dir_corpus, FALSE)
    print(paste("exec time",paste(proc.time() - ptm,collapse = " ")))
    print(proc.time() - ptm)
  # print(docvars(qc_blogs))
    print(paste("blogs",format(object.size(qc_blogs), units = "MiB")))
    print(paste("news",format(object.size(qc_news), units = "MiB")))
    print(paste("twitts",format(object.size(qc_twitts), units = "MiB")))
  }
  
  print(" --- Tests Completed --- ")
}


unitTests()
