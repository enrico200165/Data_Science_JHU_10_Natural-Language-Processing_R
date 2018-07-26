
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
    private$..coverage <- coverage_of_freq_list(
      private$..dtf$frequency ,qtiles_vec)
    if (all(TYPES_COLNAMES %in% names(private$..dtf))) {
      # trigrams
      setkeyv(private$..dtf ,TYPES_COLNAMES)
      # below here maybe redundant or just unnecessary
      setindexv(private$..dtf ,TYPES_COLNAMES[1:2])
      setindexv(private$..dtf ,TYPES_COLNAMES[1])
      setindexv(private$..dtf ,TYPES_COLNAMES[2])
    }
    else if (all(TYPES_COLNAMES[1:2] %in% names(private$..dtf))) {
      # bigrams
      setkeyv(private$..dtf ,TYPES_COLNAMES[1:2])
      # below here maybe redundant or just unnecessary
      setindexv(private$..dtf ,TYPES_COLNAMES[1])
      setindexv(private$..dtf ,TYPES_COLNAMES[2])
    }
    else if (TYPES_COLNAMES[1] %in% names(private$..dtf)) {
      # 1grams
      setkeyv(private$..dtf ,TYPES_COLNAMES[1])
      # nearly surely unnecessary
      setindexv(private$..dtf ,TYPES_COLNAMES[1])
    }
    else {
      prt_error("unexpected situation setting keys")
      stop()      
    }
    # 
    prt("key",key(private$..dtf))
  }
   
  ,finalize = function() {
    #rm(private$dtf)
    gc()
  }
   
   
   ,nfeat = function() private$..nfeat
   
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

dtf_1gram_test <- if (exists("dtf_1gram_test")) dtf_1gram_test else NULL
dtf_2gram_test <- if (exists("dtf_2gram_test")) dtf_2gram_test else NULL
dtf_3gram_test <- if (exists("dtf_3gram_test")) dtf_3gram_test else NULL


build_test_objects <- function() {
  
  rie(dtf_1gram_sep,produce_ngram_bare_dtf)
  dtf_1gram_test <<- dtf_1gram_sep[1:100, ]

  rie(dtf_2gram_sep ,produce_ngram_bare_dtf)
  dtf_2gram_test <<- dtf_2gram_sep[1:100, ]

  rie(dtf_3gram_sep ,produce_ngram_bare_dtf)
  dtf_3gram_test <<- dtf_3gram_sep[1:100, ]
}
  
build_test_objects()


o_1grams_basic <- DTF_Basic$new(dtf_1gram_test)
o_2grams_basic <- DTF_Basic$new(dtf_2gram_test)
o_3grams_basic <- DTF_Basic$new(dtf_3gram_test)




prt(o_3grams_basic$dump())

#prt("nr of features",x$nfeat())
# x$types_to_cover(qtiles_vec[4])

# x <- DTF_Basic$new(NULL)
