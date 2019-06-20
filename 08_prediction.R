


source("020_pred_globals.R")

ngrams_freqs <- vector("list",3)


pred_init <- function() {
  
  for (i in 1:3) {
    tmp <- readRDS(PRED_NGRAM_FNAMES[[i]])
    # print(paste("read ngram prediction file:", PRED_NGRAM_FNAMES[[i]]))
    ngrams_freqs[[i]] <<- tmp
    # print(head(ngrams_freqs[[i]]))
  }
    
}

pred_init()

pred_successors <- function(predecessors) {
  
  n_predecessors <- length(predecessors)
  pred_ngram <<- ngrams_freqs[[n_predecessors+1]]
  
  pre_list <- as.list(predecessors)
  successors <- pred_ngram[pre_list]
  
  # print(successors)
  
  successors
}


ret <- pred_successors(c("and","the"))
print(ret)

ret <- pred_successors(c("and","you"))
print(ret)

ret <- pred_successors(c("you","are"))
print(ret)



