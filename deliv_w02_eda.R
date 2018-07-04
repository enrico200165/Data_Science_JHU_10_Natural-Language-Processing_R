
require(quanteda)
require(readtext)


# source("01_globals.R")
# source("01_preprocess.R")
source("ev_nlp_eda_lib.R")

# examples
# http://rpubs.com/erodriguez/milestone1
# 



corp <- NA

fnames_pattern <- file.path(data_dir_corpus_in,"*subset*")
texts <- readtext(fnames_pattern
   ,docvarsfrom = "filenames", dvsep = "[_.]"
   ,docvarnames = c("lang","dummy_1","title","dummy_2","nlines_kept","nlines_read"))
  
corp <- corpus(texts)
print(summary(corp))
  
corp_lines <- corpus_reshape(corp, to = c("paragraphs"))
print(summary(corp_lines))





ignore <- function(data_dir) {

for(fname in file.path(data_dir,files)) {
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

ignore(data_dir_corpus_work)