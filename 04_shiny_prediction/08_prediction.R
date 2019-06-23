
# create conditions similar to shiny server to avoid that it will
# work locally but not when deployed
require(shiny)
if (!shiny::isRunning()) {
  print("removing all user variables, remove this or Shiny will fail")
  rm(list = ls())
} else {
  print("prediction file: running in shiny, not clearing variables")
}

require(data.table)
require(pryr)

source("shiny_globals.R")
source("utils.R")

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
  class(dir_size) <- "object_size"
  print(paste("data dir size:",format(dir_size,"Mb"),"Mb", "this is"
              ,format(unclass(dir_size)/32000000,digits = 2),"% of 32 mb"))
  
  for (i in 1:3) {
    fname <- file.path(SHINY_LOCAL_DATA_DIR,PRED_NGRAM_FNAMES[i])
    tmp <- readRDS(fname)
    ngrams_freqs_loc[[i]] <- tmp
  }
  print(paste("total model RAM size:",format(object.size(ngrams_freqs_loc),"Mb")))
  
  print(paste("all vars RAM size, Mb:",colSums(gc())[4]))
  
  ngrams_freqs_loc
}


# ---------------------------------------------------------
pred_success_core <- function(predecessors)
# ---------------------------------------------------------
{
  if (any(is.null(predecessors), length(predecessors) == 0)) {
    successors <- ngrams_freqs[[1]]
    return(successors)
  }
  
  n_predecessors <- length(predecessors)
  if (n_predecessors > 2) {
    print("WARN: pred_success_core() to many predecessors")
    predecessors <- predecessors[(n_predecessors-1):n_predecessors]
    n_predecessors <- length(predecessors)
  }
    
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
  
  # extract successors
  predecessors_used <-  ""
  preds_used <- max(which(TYPES_COLNAMES %in% names(successors))) - 1
  if (preds_used == 0)
    predecessors_used <- paste(predecessors_used,"<none>")
  else {
    preds <- paste(predecessors[1:max(1,preds_used)], collapse = " ")
    predecessors_used <- paste(predecessors_used,preds)
  }
  
  succ_col_name <- TYPES_COLNAMES[preds_used+1]
  just_successors <- successors[[succ_col_name]]
  
  list(just_successors, preds_used, predecessors_used)
}



pred_successors_aggregate <- function(predecessors, start_sentence = F, nr_to_get) 
{
  nr_predictions <- 0
  continue <- T
  cur_predecessors <- predecessors
  results_dt <- NULL
  while (T) {
    ret <- pred_successors(cur_predecessors, start_sentence, nr_to_get)
    cur_successors <- ret[[1]]; nr_predecessors_used_cur <- ret[[2]]; predecessors_used_cur <- ret[[3]]
    
    cur_results_dt <- data.table(successors = cur_successors
      ,nr_predecessors_used = nr_predecessors_used_cur, predecessors_used = predecessors_used_cur)
    if (is.null(results_dt))
      results_dt <- cur_results_dt
    else
      results_dt <-rbind(results_dt,cur_results_dt)
    
    nr_predictions <- nr_predictions + length(cur_successors)
    # change into data table
    # merge into results data table
    nr_predict_missing <- nr_to_get - nr_predictions
    if (nr_predict_missing <= 0| nr_predecessors_used_cur <= 0)
      break
    
    if (nr_predecessors_used_cur <= 1) {
      cur_predecessors <- NULL 
    }
    else {
      l <- length(cur_predecessors)
      cur_predecessors <- cur_predecessors[(l-nr_predecessors_used_cur+2):l]
      print("")
    }
  }
  results_dt <- results_dt[1:min(nr_to_get,nrow(results_dt))]
  
  results_dt
}







# ---------------------------------------------------------
print_pred_result <- function(pred_result_list)
# ---------------------------------------------------------
{
  print("");print("")
  print(paste("predecessors length:",pred_result_list[[2]]))
  print(paste("predecessors used:",pred_result_list[[3]]))
  results <- pred_result_list[[1]]
  for (i in 1:length(results)) {
    print(paste0("[",i,"] ",results[i]))
  }
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

if (!shiny::isRunning() ) {

  
#pred_successors_aggregate(c("your","time","on"),F,  5)
pred_successors_aggregate(c("your","time","on"),F,  2)
  

  
pred_successors(c("asas","sdsds","your","time","on"),F,  5)
  
    
pred_successors(c("a","baby","was","sdsds"),F,  5)
  
  
ret <- pred_successors(c("a","baby"),F,  5)
print_pred_result(ret)



ret <- pred_successors(c("sss","asdasda"),F,  5)
print_pred_result(ret)


ret <- pred_successors(c("a£$%£%","%&//"),F,  5)
print_pred_result(ret)


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