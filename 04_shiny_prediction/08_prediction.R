
require(shiny)
if (!shiny::isRunning()) {
  print("removing all user variables, remove this or Shiny will fail")
  rm(list = ls())
} else {
  print("prediction source file: in shiny, not cleaning up")
}

require(data.table)
require(pryr)

source("shiny_globals.R")

# --------------------------------------------------------------------
#                  CONSTANSTS
# --------------------------------------------------------------------

TYPE1_COLNAME <- "primo"   # column with type 1 of an ngram
TYPE2_COLNAME <- "secondo" # column with type 2 of an ngram
TYPE3_COLNAME <- "terzo"   # column with type 3 of an ngram
TYPES_COLNAMES <- c(TYPE1_COLNAME, TYPE2_COLNAME , TYPE3_COLNAME)

PREDECESSOR_FREQUENCY <- "pdcessor_freq"
FREQUENCY_COL <- "frequency"

SHINY_LOCAL_DATA_DIR <- file.path(".","data")
PRED_NGRAM_FNAMES <- paste0("pred_",1:3,"gram.rds")



# ---------------------------------------------------------
read_models <- function() 
# ---------------------------------------------------------
{
  ngrams_freqs_loc <- vector("list",3)
  
  dir_size <- 0L
  sapply(list.files(SHINY_LOCAL_DATA_DIR), function(x) { 
    dir_size <<- dir_size+file.info(file.path(SHINY_LOCAL_DATA_DIR,x))$size})
  print(paste("data dir size:",dir_size,"Mb", "this is"
              ,dir_size/32000000,"% of 32 mb"))
  
  for (i in 1:3) {
    fname <- file.path(SHINY_LOCAL_DATA_DIR,PRED_NGRAM_FNAMES[i])
    tmp <- readRDS(fname)
    # print(paste("model for",i,"grams: nr rows:",nrow(tmp),"memory:"
    #             ,format(object.size(tmp),"Mb")))
    ngrams_freqs_loc[[i]] <- tmp
  }

  print(paste("total model RAM size:",format(object.size(ngrams_freqs_loc),"Mb"),
              "this is",object.size(ngrams_freqs_loc)/32000000,"% of 32 mb"))
  
  ngrams_freqs_loc
}


pred_success_core <- function(predecessors) {
  
  if (any(is.null(predecessors), length(predecessors) == 0)) {
    successors <- ngrams_freqs[[1]]
    return(successors)
  }
    
  n_predecessors <- length(predecessors)
  pred_ngram <<- ngrams_freqs[[n_predecessors+1]]
  
  pre_list <- as.list(predecessors)
  successors <- pred_ngram[pre_list]
  has_nas <- any(is.na(successors[[TYPES_COLNAMES[n_predecessors+1]]]))
  if (any(nrow(successors) == 0, has_nas)) {
    if (length(predecessors) >= 2) {
      predecessors <- predecessors[2:length(predecessors)]
    } else if (length(predecessors) <= 1) {
      predecessors <- NULL
    }
    return(pred_success_core(predecessors))
  } 
  
  # print(successors)
  
  successors
}


pred_successors <- function(predecessors, start_sentence = F, reduce_to) {

  if (start_sentence) {
    if (any(!is.null(predecessors), length(predecessors) >= 0)) {
       print("ERROR: start_sentence predecessors found")
      stop()
    }
    predecessors <- c("sss")
  }
  
  successors <- pred_success_core(predecessors)
  if(reduce_to < nrow(successors)) {
    setkeyv(successors, FREQUENCY_COL)
    successors <- successors[.N:(.N-reduce_to+1)]
  }
  
  successors
}

###########################################################
#           GLOBAL/INIZIALIZATION CODE
###########################################################

ngrams_freqs <- read_models()
if (any(is.null(ngrams_freqs),length(ngrams_freqs) != 3) ) {
  shstop("failed to read models")
  for (i in 1:length(ngrams_freqs)) {
    if (nrow(ngrams_freqs[[i]]) <= 0)
      shstop("models not read correctly")
  }
}


# ---------------------------------------------------------
#           TEMPORARY TESTS
# ---------------------------------------------------------

if (!shiny::isRunning() & F) {

ret <- pred_successors(c("asdsa","sss"),F,  5)
print(ret)



ret <- pred_successors(c("sss","asdasda"),F,  5)
print(ret)


ret <- pred_successors(c("a£$%£%","%&//"),F,  5)
print(ret)


ret <- pred_successors(c("the","usual"),F,  5)
print(ret)


ret <- pred_successors(c("a£$%£%","usual"),F,  5)
print(ret)


ret <- pred_successors(c("a£$%£%","%&//"),F,  5)
print(ret)

pred_successors(NULL, F,  5)
print(ret)

# to make 2pred fail and fall down to 1 prede
predecessors2_nosecond <- setdiff(
  ngrams_freqs[[2]][["primo"]],ngrams_freqs[[3]][["secondo"]])


ret <- pred_successors(c("and","the"), F,5)
print(ret)

ret <- pred_successors(c("and","you"), F,5)
print(ret)

ret <- pred_successors(c("you","are"), F,5)
print(ret)

# last predecessor fails

ret <- pred_successors(c("the","usual"), F,5)
print(ret)

print(format(object.size(ngrams_freqs), units = "MB"), F,5)

print(format(object.size(ngrams_freqs[[3]]), units = "MB"), F,5)

}
