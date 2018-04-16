
require(quanteda)
require(readtext)
source("01_globals.R")
source("01_preprocess.R")


corp <- NA

fnames_pattern <- file.path(data_dir_corpus,"*subset*")
texts <- readtext(fnames_pattern
   ,docvarsfrom = "filenames", dvsep = "[_.]"
   ,docvarnames = c("lang","dummy_1","title","dummy_2","nlines_kept","nlines_read"))
  
corp <- corpus(texts)
print(summary(corp))
  
corp_lines <- corpus_reshape(corp, to = c("paragraphs"))
print(summary(corp_lines))





ignore <- function() {

for(fname in file.path(data_dir_corpus,files)) {
  result <- enricoReadText(fname, -1)
  texts <- result[["lines"]]
  vars <- result[["vars"]]
  dnames <- as.list(result[["dnames"]])
  # print(substr(texts[1],1,32)); print(vars)
  
  if (is.na(corp)) {
     corp <- corpus(texts,docnames = dnames , docvars = vars)
  } else {
    print(paste("before",nrow(summary(corp))))
    corp + corpus(texts, docvars = vars)
    print(paste("after",nrow(summary(corp))))
  }
  # print(summary(corp))
}
}

# fnames_pattern <- file.path(data_dir_corpus,"*subset*")
# 
# blogs <- readtext(fnames_pattern
#  ,docvarsfrom = "filenames", dvsep = "[_.]"
# ,docvarnames = c("lang","ignore_1","title","subset","nlines_kept","nlines_read"))
# 
# 
# str(blogs)
# 
# myCorpus <- corpus(blogs)
# print(summary(myCorpus))
# 
# 

# lunghezza tokens con quanteda
# eventualmente applica grouping, supportato in quanteda
nchar(data_corpus_inaugural[1:7])/ntoken(data_corpus_inaugural[1:7])
# remove punctuation
# ntoken(data_corpus_inaugural[1:7], remove_punct = TRUE)

