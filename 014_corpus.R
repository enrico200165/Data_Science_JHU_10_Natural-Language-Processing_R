# OBJECTIVE 
# EVGuess after long time: 
# subset corpora files by rading original files
# and writing subsetted copies with a subset of lines


require(quanteda) # for docvars
require(readtext)
require(pryr)

source("007_utils.R")

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
readQCorp <- function(data_dir_corpus, subsetPar) 
  # --------------------------------------------------------------------
# lazy reads text files matching pattern into a single Quanteda corpus
{    
  # print(paste("readQCorp",data_dir_corpus))
  prt("readQCorp",data_dir_corpus)
  stopifnot(dir.exists(data_dir_corpus))
  filesInDir <- list.files(data_dir_corpus,"*bset*"); 
  # prt(filesInDir)
  
  en_US_blogs   <- readtextIfEmpty_Wrapper(en_US_blogs,data_dir_corpus,  "en_US.blogs")
  en_US_news    <- readtextIfEmpty_Wrapper(en_US_news,data_dir_corpus,   "en_US.news")
  en_US_twitter <- readtextIfEmpty_Wrapper(en_US_twitter,data_dir_corpus,"en_US.twitter")
  
  # if (F) {
  #   de_DE_blogs   <- readtextIfEmpty_Wrapper(de_DE_blogs,data_dir_corpus,  "de_DE.blogs")
  #   de_DE_news    <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus,   "de_DE.news")
  #   de_DE_twitter <- readtextIfEmpty_Wrapper(de_DE_twitter,data_dir_corpus,"de_DE.twitter")
  #   
  #   fi_FI_blogs   <- readtextIfEmpty_Wrapper(fi_FI_blogs,data_dir_corpus,"fi_FI.blogs")
  #   fi_FI_news    <- readtextIfEmpty_Wrapper(fi_FI_news,data_dir_corpus, "fi_FI.news")
  #   fi_FI_twitter <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus, "fi_FI.twitter")
  #   
  #   ru_RU_blogs   <- readtextIfEmpty_Wrapper(ru_RU_blogs,data_dir_corpus,"ru_RU.blogs")
  #   ru_RU_news    <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus, "ru_RU.news")
  #   ru_RU_twitter <- readtextIfEmpty_Wrapper(de_DE_news,data_dir_corpus, "ru_RU.twitter")
  #   
  #   
  #   texts_df <- bind_rows(
  #     en_US_blogs, en_US_news, en_US_twitter
  #     ,de_DE_blogs, de_DE_news, de_DE_twitter
  #     ,fi_FI_blogs, fi_FI_news, fi_FI_twitter
  #     ,ru_RU_blogs, ru_RU_news, ru_RU_twitter)
  # }
  
  
  texts_df <- bind_rows(
    en_US_blogs, en_US_news, en_US_twitter
  )
  
  qc <- corpus(texts_df)
  docvars(qc,TXT_LNG) <- tolower(docvars(qc,TXT_LNG))
  
  invisible(gc)
  qc
}

###########################################################
#                 PRIVATE
###########################################################



# --------------------------------------------------------------------
readtextIfEmpty_Wrapper <- function(text_df,data_dir_corpus
                                    ,fnamePattern) 
  # --------------------------------------------------------------------
# Reads a single file at a time even though we use a pattern
{
  stopifnot(dir.exists(data_dir_corpus))
  
  fname <- list.files(data_dir_corpus,paste0(fnamePattern,".*\\.txt"))
  if (!(length(fname) == 1)) {
    print(paste("ERROR found > 1  file for pattern"))
    print(paste(fname, collapse = " "))
    print(paste("Terminating"))
    stopifnot(length(fname) == 1)
  }
  readtextIfEmpty(text_df,data_dir_corpus,fname)
}


# --------------------------------------------------------------------
readtextIfEmpty <- function(mydf, in_dir, nFile)
# --------------------------------------------------------------------
# 
{

  # build full path name if dir is provided  
  if ( missing(in_dir) || is.na(in_dir) || (nchar(in_dir) <= 0)) {
    nomeFile <- nFile
  } else {
    stopifnot(dir.exists(in_dir))
    nomeFile <- file.path(in_dir,nFile)
  }
  
  varName <- deparse(substitute(mydf))
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


###########################################################
#                  TEMPORARY TESTS
###########################################################
