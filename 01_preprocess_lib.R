
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
require(DBI); 
require(RSQLite); 
require(sqldf)


source("01_globals.R")

# ---------------------------------------------------------
  buildWordsDBFromFile <- function(words_file, db_fname) {
# ---------------------------------------------------------
# reads text file and put words in an sqlite table
    
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
    



# --------------------------------------------------------------------
  readtextIfEmpty <- function(mydf, in_dir, nFile)
# --------------------------------------------------------------------
# 
{
  varName <- deparse(substitute(mydf))
  
  if ( missing(in_dir) || is.na(in_dir) || (nchar(in_dir) <= 0)) {
    nomeFile <- nFile
  } else {
    stopifnot(dir.exists(in_dir))
    nomeFile <- file.path(in_dir,nFile)
  }

  exists <- exists(varName)
  filled <- exists && (!is.null(nrow(mydf)) &&  nrow(mydf)> 0)
  if (filled) {
    prt(paste(varName,"exists and is filled, do nothing"))
    return(mydf)
  }
  
  if (!exists || !filled) {
    rdsFName <- paste0(SERIAL_PREFIX,nFile,".rds")
    hasSerialization <- file.exists(rdsFName)
    if (hasSerialization) {
      # print(paste(varName,"reading serialization from:",rdsFName))
      mydf <- readRDS(rdsFName)
      # assign(varName,mydf,.GlobalEnv) # pass it outside,
      assign(varName,mydf,envir=parent.frame(n = 1))
      return(mydf)
    } 
    if (is.null(nomeFile) || length(nomeFile) <= 0  || !file.exists(nomeFile)) {
        print(paste("no files found for:",nomeFile))
         return(NA)
    }

    prt(paste(varName,"reading file from",nomeFile," and writing serialization"))
    my_rt <- readtext(nomeFile
    , docvarsfrom = "filenames", docvarnames = c(TXT_LNG,TXT_CTR, TXT_TYP,
                                                 "dummy1", "lines_in", "lines_tot") ,dvsep = "[_.]"
                      , encoding = "UTF-8"
                       , verbosity = 1)
    sampled_pctg <- 100*docvars(my_rt)$lines_in/docvars(my_rt)$lines_tot
    my_rt <- select(my_rt,-c(6:ncol(my_rt)))
    my_rt$sample_pctg <- sampled_pctg
  }
  
  if (!file.exists(rdsFName)) {
    # prt(paste("saving serialization to: ",rdsFName))
    saveRDS(my_rt, file = rdsFName)
  }
  
  my_rt
}

  
# --------------------------------------------------------------------
  readtextIfEmpty_Wrapper <- function(text_df,data_dir_corpus
                                    ,fnamePattern) 
# --------------------------------------------------------------------
# Reads a single file at a time even though we use a pattern
{
  stopifnot(dir.exists(data_dir_corpus))
  
  fname <- list.files(data_dir_corpus,paste0(fnamePattern,".*\\.txt"))
  if (!(length(fname) == 1)) {
    print(length(fname))
  }
  stopifnot(length(fname) == 1)
  
  readtextIfEmpty(text_df,data_dir_corpus,fname)
}


# --------------------------------------------------------------------
  readQCorp <- function(data_dir_corpus, subsetPar) 
# --------------------------------------------------------------------
# lazy reads text files matching pattern into a single Quanteda corpus
{    
  print(paste("readQCorp",data_dir_corpus))
  stopifnot(dir.exists(data_dir_corpus))
  filesInDir <- list.files(data_dir_corpus,"*bset*"); print(filesInDir)
  
  en_US_blogs   <- readtextIfEmpty_Wrapper(en_US_blogs,data_dir_corpus,  "en_US.blogs")
  en_US_news    <- readtextIfEmpty_Wrapper(en_US_news,data_dir_corpus,   "en_US.news")
  en_US_twitter <- readtextIfEmpty_Wrapper(en_US_twitter,data_dir_corpus,"en_US.twitter")
  
  if (F) {
    de_DE_blogs   <- readtextIfEmpty_Wrapper(de_DE_blogs,data_dir_corpus,  "de_DE.blogs")
    de_DE_news    <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus,   "de_DE.news")
    de_DE_twitter <- readtextIfEmpty_Wrapper(de_DE_twitter,data_dir_corpus,"de_DE.twitter")
  
    fi_FI_blogs   <- readtextIfEmpty_Wrapper(fi_FI_blogs,data_dir_corpus,"fi_FI.blogs")
    fi_FI_news    <- readtextIfEmpty_Wrapper(fi_FI_news,data_dir_corpus, "fi_FI.news")
    fi_FI_twitter <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus, "fi_FI.twitter")
  
    ru_RU_blogs   <- readtextIfEmpty_Wrapper(ru_RU_blogs,data_dir_corpus,"ru_RU.blogs")
    ru_RU_news    <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus, "ru_RU.news")
    ru_RU_twitter <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus, "ru_RU.twitter")

  
    texts_df <- bind_rows(
    en_US_blogs, en_US_news, en_US_twitter
    ,de_DE_blogs, de_DE_news, de_DE_twitter
    ,fi_FI_blogs, fi_FI_news, fi_FI_twitter
    ,ru_RU_blogs, ru_RU_news, ru_RU_twitter)
  }
  
  
  texts_df <- bind_rows(
   en_US_blogs, en_US_news, en_US_twitter
  )
  
  qc <- corpus(texts_df)
  docvars(qc,TXT_LNG) <- tolower(docvars(qc,TXT_LNG))
  
  invisible(gc)
  qc
}


# --------------------------------------------------------------------
zap_files_serializations <- function(patternPar) 
# --------------------------------------------------------------------
{
  
  if (missing(patternPar)) patternPar <- "*txt*.rds"
  
  fnames_to_delete <- list.files(".", patternPar)
  file.remove(fnames_to_delete)
  length(fnames_to_delete)
}
# zap_files_serializations()



# ---------------------------------------------------------
  checkUSANewsFileProblem <- function(fname, nrLinesToRead) {
# ---------------------------------------------------------
  
    
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


