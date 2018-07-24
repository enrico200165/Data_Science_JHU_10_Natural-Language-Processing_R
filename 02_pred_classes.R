
require(R6)

source("02_pred_globals.R")
source("02_pred_ngram_bare_dtf.R")

# --------------------------------------------------------------------
DTF_Basic <- R6Class(
# wraps an ngram frequency data table
# --------------------------------------------------------------------

  "DTF_Basic"
  
 ,public = list(
  initialize = function(dtf_par) {
    prt("in initialize")
    # private$.dtf <- dtf_par
  }
  ,finalize = function() {
    #rm(private$dtf)
    gc()
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
    .dtf = NULL
  )

    
)

