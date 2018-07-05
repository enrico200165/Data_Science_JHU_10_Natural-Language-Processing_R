
require(quanteda)
require(readtext)


# source("01_globals.R")
# source("01_preprocess.R")
source("ev_nlp_eda_lib.R")

# examples
# http://rpubs.com/erodriguez/milestone1
# 


#---------------------------------------------------------------------
  distributionWordFrequencies <- function()
#---------------------------------------------------------------------
{
  
}





recover_or_delete <- function(data_dir) {

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

