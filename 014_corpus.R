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


# --------------------------------------------------------------------
readQCorp <- function(data_dir_corpus, subsetPar) 
  # --------------------------------------------------------------------
# lazy reads text files matching pattern into a single Quanteda corpus
{    
  # print(paste("readQCorp",data_dir_corpus))
  prt("readQCorp",data_dir_corpus)
  stopifnot(dir.exists(data_dir_corpus))
  filesInDir <- list.files(data_dir_corpus,"*bset*"); 
  prt(filesInDir)
  
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


