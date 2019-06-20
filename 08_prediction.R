


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
    return(pred_successors(predecessors))
  } 
  
  # print(successors)
  
  successors
}


ret <- pred_successors(c("asdsa","sss"))
print(ret)



ret <- pred_successors(c("sss","asdasda"))
print(ret)


ret <- pred_successors(c("a£$%£%","%&//"))
print(ret)


ret <- pred_successors(c("the","usual"))
print(ret)


ret <- pred_successors(c("a£$%£%","usual"))
print(ret)


ret <- pred_successors(c("a£$%£%","%&//"))
print(ret)


print(pred_successors(NULL))

# to make 2pred fail and fall down to 1 prede
predecessors2_nosecond <- setdiff(
  ngrams_freqs[[2]][["primo"]],ngrams_freqs[[3]][["secondo"]])


ret <- pred_successors(c("and","the"))
print(ret)

ret <- pred_successors(c("and","you"))
print(ret)

ret <- pred_successors(c("you","are"))
print(ret)

# last predecessor fails

ret <- pred_successors(c("the","usual"))
print(ret)


