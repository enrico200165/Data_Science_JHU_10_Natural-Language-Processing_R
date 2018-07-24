
require(R6)

source("02_pred_globals.R")
source("02_pred_ngram_bare_dtf.R")

qtiles_vec <- c(0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.97,0.98,0.99)

# --------------------------------------------------------------------
DTF_Basic <- R6Class(
# wraps an ngram frequency data table
# --------------------------------------------------------------------

  "DTF_Basic"
  
 ,public = list(
   
  initialize = function(dtf_par) {
    prt("in initialize")

    stopifnot(nrow(dtf_par$primo) > 0)
    
    private$..dtf <- dtf_par
    
    private$..nfeat <- nrow(private$..dtf );
    
    private$..coverage <- coverage_of_freq_list(private$..dtf$frequency
        ,qtiles_vec)
  }
   
   
  ,finalize = function() {
    #rm(private$dtf)
    gc()
  }
   
  ,types_to_cover = function(qtile) {
    
    idx <- match(qtile,qtiles_vec , , nomatch = NA_integer_)
    
    if (is.na(idx)) {
      prt_error("illegal quantile:",qtile)
      stop()
    }
    
    private$..coverage$idxs[idx]
   }
   
   
   ,dump = function() {
     ret <- ""
     ret <- paste0(ret,"(private$..nfeat=",private$..nfeat,")")
     ret <- paste0(ret,"(private$..coverage",private$..coverage[[1]]
       ," - ",private$..coverage[[2]],")")
     
   }
  ) # public 

 ,active = list(
    dummy = function(value) {
      if (missing(value)) {
        # private$.age
      } else {
        #stop("`$age` is read only", call. = FALSE)
      }
    }
  )
    
  ,private = list(
    ..nfeat = NA_integer_
    ,..dtf = NULL
    ,..coverage = NA_real_
  )

    
)


#####################################################################
#                           TEST
#####################################################################


strict(F)

dtf_test <- if (exists("dtf_test")) dtf_test else NULL

get_test_dtf <- function() {
  rie(dtf_2gram_sep,produce_ngram_bare_dtf)
  dtf_test <<- dtf_2gram_sep[1:100, ]
}


rie(dtf_test, get_test_dtf)

x <- DTF_Basic$new(dtf_test)
prt(x$dump())


x$types_to_cover(qtiles_vec[4])

# x <- DTF_Basic$new(NULL)
