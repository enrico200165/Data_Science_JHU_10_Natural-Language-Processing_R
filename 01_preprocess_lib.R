# install.packages("stringr", dependencies = TRUE)
require(quanteda)
require(stringr)
require(readtext)
require(dplyr)
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

    stopifnot(all(dir.exists(in_dir),dir.exists(out_dir)))
    
    descr <- paste0("_", nrLinesKept, "_", nrLinesRead)
    # remove/clean up eventual prexisting subset
    fnameOut <- gsub("_?subset.*\\.", ".", fname)
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
  
  
  
# --------------------------------------------------------------------
  subsetTextFilesByLines <- function(in_dir, out_dir, nrLinesKept
    ,nrLinesRead, forceIt)
# --------------------------------------------------------------------
{    
  print(paste("out dir:",out_dir," - in dir: ",in_dir))

  stopifnot(dir.exists(in_dir))
  stopifnot(dir.exists(out_dir))
  
  textFiles <- list.files(in_dir)
  if (length(textFiles) <= 0) {
    print(paste("no files found in",in_dir))
    return(FALSE)
  }
  
  # remove eventual subset string
  # textFiles <- gsub("_?subset.*\\.", ".", textFiles)
  # textFiles <- grep(".*tw.*",textFiles,value = TRUE)
  # textFiles <- grep("subset",textFiles,value = TRUE,invert = T)
  
  # textFiles <- file.path(data_dir_corpus, textFiles)
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
  
  if ( missing(in_dir) || is.na(in_dir) || (nchar(in_dir) <= 0)
    ) {
    nomeFile <- nFile
  } else {
    stopifnot(dir.exists(in_dir))
    nomeFile <- file.path(in_dir,nFile)
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

    print(paste(varName,"reading file from",nomeFile," and writing serialization"))
    my_rt <- readtext(nomeFile
    , docvarsfrom = "filenames", docvarnames = c(TXT_LNG,TXT_CTR, TXT_TYP,
                                                 "dummy1", "lines_in", "lines_tot") ,dvsep = "[_.]"
                      , encoding = "UTF-8"
                       , verbosity = 1)
    sampled_pctg <- 100*docvars(my_rt)$lines_in/docvars(my_rt)$lines_tot
    my_rt <- select(my_rt,-c(6:ncol(my_rt)))
    my_rt$sample_pctg <- sampled_pctg
    
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
  
  qc <- corpus(texts_df)
  docvars(qc,TXT_LNG) <- tolower(docvars(qc,TXT_LNG))
  
  qc
}


# ====================================================================
# GLOBAL CODE, must be so to keep things simple
# ====================================================================
# NB relies on global with fixed name

# clean_rds(".*")
read_dir = if (use_full_corpus()) data_dir_corpus_full else data_dir_corpus_subset
if (F) {
  if (!readIfEmpty(dfm_full)) {
    # need the corpus
    if (!readIfEmpty(qc_full)) {
      print(paste("reading corpus from dir:",read_dir))
      qc_full <- readQCorp(read_dir, FALSE)
      serializeIfNeeded(qc_full, FALSE)
    }
    # choose one
    dfm_full <- dfm(qc_full) 
    dfm_full <- dfm(qc_full, remove_punct = T)
    serializeIfNeeded(dfm_full, FALSE)
    }
}

# 
#initalize_vars()
# rm(qc_full); gc() # > 1 GiB and should not need it



# # --------------------------------------------------------------------
#   readQCorp_HIDE_FOR_NOW <- function(data_dir_corpus, subsetPar) 
# # --------------------------------------------------------------------
# # lazy reads text files matching pattern into a single Quanteda corpus
# {    
#   
#   print(paste("readQCorp",data_dir_corpus))
#   stopifnot(dir.exists(data_dir_corpus))
#   
#   
#   filesInDir <- list.files(data_dir_corpus,"*bset*"); print(filesInDir)
#   
#   en_US_blogs   <- readtextIfEmpty_Wrapper(en_US_blogs,data_dir_corpus,  "en_US.blogs")
#   qc <- corpus(en_US_blogs); rm(en_US_blogs);gc(); mem = pryr::object_size(qc)
#   en_US_news    <- readtextIfEmpty_Wrapper(en_US_news,data_dir_corpus,   "en_US.news")
#   qc <-qc+corpus(en_US_news);rm(en_US_news);gc();mem = pryr::object_size(qc)
#   en_US_twitter <- readtextIfEmpty_Wrapper(en_US_twitter,data_dir_corpus,"en_US.twitter")
#   qc <-qc+corpus(en_US_twitter);rm(en_US_twitter);gc();mem = pryr::object_size(qc)
#   
#   de_DE_blogs   <- readtextIfEmpty_Wrapper(de_DE_blogs,data_dir_corpus,  "de_DE.blogs")
#   qc <-qc+corpus(de_DE_blogs);rm(de_DE_blogs);gc();mem = pryr::object_size(qc)
#   de_DE_news    <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus,   "de_DE.news")
#   qc <-qc+corpus(de_DE_news);rm(de_DE_news);gc();mem = pryr::object_size(qc)
#   de_DE_twitter <- readtextIfEmpty_Wrapper(de_DE_twitter,data_dir_corpus,"de_DE.twitter")
#   qc <-qc+corpus(de_DE_twitter);rm(de_DE_twitter);gc();mem = pryr::object_size(qc)
#   
#   fi_FI_blogs   <- readtextIfEmpty_Wrapper(fi_FI_blogs,data_dir_corpus,"fi_FI.blogs")
#   qc <-qc+corpus(fi_FI_blogs);rm(fi_FI_blogs);gc();mem = pryr::object_size(qc)
#   fi_FI_news    <- readtextIfEmpty_Wrapper(fi_FI_news,data_dir_corpus, "fi_FI.news")
#   qc <-qc+corpus(fi_FI_news);rm(fi_FI_news);gc();mem = pryr::object_size(qc)
#   fi_FI_twitter <- readtextIfEmpty_Wrapper(fi_FI_twitter,data_dir_corpus, "fi_FI.twitter")
#   qc <-qc+corpus(fi_FI_twitter);rm(fi_FI_twitter);gc();mem = pryr::object_size(qc)
#   
#   ru_RU_blogs   <- readtextIfEmpty_Wrapper(ru_RU_blogs,data_dir_corpus,"ru_RU.blogs")
#   qc <-qc+corpus(ru_RU_blogs);rm(ru_RU_blogs);gc();mem = pryr::object_size(qc)
#   ru_RU_news    <- readtextIfEmpty_Wrapper(ru_RU_news,data_dir_corpus, "ru_RU.news")
#   qc <-qc+corpus(ru_RU_news);rm(ru_RU_news);gc();mem = pryr::object_size(qc)
#   ru_RU_twitter <- readtextIfEmpty_Wrapper(ru_RU_twitter,data_dir_corpus, "ru_RU.twitter")
#   qc <-qc+corpus(ru_RU_twitter);rm(ru_RU_twitter);gc();mem = pryr::object_size(qc)
#   
#   print(paste("exiting readQCorp???(), corpus size GiB: ", GiB(mem)))
#   
#   qc  
# }



# ====================================================================
#                         Unit Tests
# ====================================================================



# --------------------------------------------------------------------
  test_01_preprocess_libs.R <- function()
# --------------------------------------------------------------------
{

  print(" --- Unit Testing --- ")

  T && subsetTextFilesByLines(data_dir_corpus_full 
      ,data_dir_corpus_subset ,50 ,1000 , forceIt = F)

  
  F && test_read_corpuses()
  

  if (F) {
    qc_full <- readQCorp(data_dir_corpus_in, FALSE)
    print("finished read the corpus, serializing it if needed")
    serializeIfNeeded(qc_full, FALSE) 
    print("finished serializing")
    # saveRDS(qc,file = "qc.rds")
    # print(gc())
    # save.image(file="compact.RData") 
    # rm(list=ls())
    # print(gc())
    # load(file="compact.RData")
    # print(gc())
    # qc <- readRDS(file = "qc.rds")
    # smr <- summary(qc)
    # print(smr)
    # print(str(smr))
  }  
  
#  if(T) {
  if(F) {
    ptm <- proc.time()
    print(list.files(data_dir_corpus))
    readQCorp(data_dir_corpus, FALSE)
    print(paste("exec time",paste(proc.time() - ptm,collapse = " ")))
    print(proc.time() - ptm)
  # print(docvars(qc_blogs))
    print(paste("blogs",format(object.size(qc_blogs), units = "MiB")))
    print(paste("news",format(object.size(qc_news), units = "MiB")))
    print(paste("twitts",format(object.size(qc_twitts), units = "MiB")))
  }
  
  
  if (F) {
    mydf <- NA
    #readtextIfEmpty_Wrapper(mydf, data_dir_corpus_in,"")
  }
  
  print(" --- Tests Completed --- ")
}

# 
  test_01_preprocess_libs.R()



