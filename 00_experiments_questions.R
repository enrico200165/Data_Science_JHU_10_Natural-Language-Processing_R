require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)


source("01_globals.R")

# ---------------------------------------------------------
#   Useful, Scott
# ---------------------------------------------------------
# convert(x, to = c("lda", "tm", "stm", "austin", "topicmodels", "lsa",
#                   "matrix", "data.frame"), docvars = NULL)


# ---------------------------------------------------------
# find ngram by name
# ---------------------------------------------------------
# probably slow
# quantdfm[   , which(colnames(quantdfm) == "counsel")]

# ---------------------------------------------------------
# Quanteda
# ---------------------------------------------------------
#class(toks)
# [1] "tokens"
# str(toks)
# List of 5965
# $ text166543: chr [1:1139] "" "man" "" "shot" ...
# $ text166544: chr [1:924] "Mitt" "Romney" "warned" "" ...
# $ text166545: chr [1:1632] "" "rape" "survivor" "support" ...
# $ text166546: chr [1:1163] "" "man" "" "shot" ...
# $ text166547: chr [1:1613] "" "" "sun" "went" ...

# ---------------------------------------------------------
#  looking for a value in a column
# ---------------------------------------------------------
# kwds <- textstat_keyness(rbind(dfm_europe, dfm_britain), target = seq_along(toks_europe)) #
# class(kwds)
# # [1] "keyness"    "textstat"   "data.frame"
# kwds[which(kwds[,1] == "uk"), ]


# ---------------------------------------------------------
#   System Commands
# ---------------------------------------------------------
# No system, system2:
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/system2.html


wcForFile <- function(fdir,fname) {

  print(paste("wcForFile() file:",fname))
  file_path <- file.path(fdir,fname)
  command = "wc"
  args <- "--bytes"
  args <- paste(args,"--chars")
  args <- paste(args,"--words")
  args <- paste(args,"--lines")
  args <- paste(args,"--max-line-length")
  args <- paste(args, file_path)
  out <- system2(command = command
                 ,args = args,
                 stdout = TRUE, stderr = "", stdin = "", input = NULL
                 #,env = character(), wait = TRUE
                 #,minimized = FALSE, invisible = TRUE, timeout = 0
  )
  
  # print(out)
  spl <- strsplit(trimws(out)," +")
  spl <- spl[[1]] # extract from list
  splNums <- as.integer(spl[1:length(spl)-1])
  
  wcDfRow <- data.frame(
    file_name = fname
    ,n_byte = splNums[4]
    ,n_char = splNums[3]
    ,n_word = splNums[1]
    ,newline = splNums[1]
    ,max_line_len = splNums[5]
    ,stringsAsFactors=FALSE
    )
  # print(wcDfRow)
  wcDfRow
}



getWCInfo <- function(data_dir, ptrn) {

  flist <- list.files(data_dir,ptrn)
  
  # wcForFile(data_dir_corpus,flist[[1]])
  
  myrows <- lapply(flist, function(x) wcForFile(data_dir,x))
  wcDfSeed <-myrows[[1]]; 
  wcDf <- bind_rows(wcDfSeed,myrows[2:length(myrows)])
  wcDf
}

mydf <- getWCInfo(data_dir_corpus,"*.txt")

print(mydf)
