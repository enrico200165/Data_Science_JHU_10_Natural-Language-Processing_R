
require(R6)
require(gridExtra)
require(ggplot2)

source("02_pred_globals.R")
source("02_pred_ngram_bare_dtf.R")

qtiles_vec <- c(0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.97,0.98,0.99, 0.995)

# --------------------------------------------------------------------
DTF_Basic <- R6Class("DTF_Basic"
# wraps an ngram frequency data table
# -------------------------------------------------------------------

  # ==================================================================
 ,public = list(
  # ==================================================================
   
  # ------------------------------------------------------------------
   initialize = function(dtf_par) {
    prt("in initialize")

    stopifnot(nrow(dtf_par$primo) > 0)
    
    private$..dtf <- dtf_par
    private$..nfeat <- nrow(private$..dtf );
    private$..coverage <- coverage_of_freq_list(
      private$..dtf$frequency ,qtiles_vec)
    if (all(TYPES_COLNAMES %in% names(private$..dtf))) {
      # trigrams
      private$..ngram = 3
      setkeyv(private$..dtf ,TYPES_COLNAMES)
      # below here maybe redundant or just unnecessary
      setindexv(private$..dtf ,TYPES_COLNAMES[1:2])
      setindexv(private$..dtf ,TYPES_COLNAMES[1])
      setindexv(private$..dtf ,TYPES_COLNAMES[2])
    }
    else if (all(TYPES_COLNAMES[1:2] %in% names(private$..dtf))) {
      # bigrams
      private$..ngram = 2
      setkeyv(private$..dtf ,TYPES_COLNAMES[1:2])
      # below here maybe redundant or just unnecessary
      setindexv(private$..dtf ,TYPES_COLNAMES[1])
      setindexv(private$..dtf ,TYPES_COLNAMES[2])
    }
    else if (TYPES_COLNAMES[1] %in% names(private$..dtf)) {
      # 1grams
      private$..ngram = 1
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
   
  ,coverers = function() private$..coverage
 
  ,nfeat = function() private$..nfeat

  ,ngram = function() private$..ngram
   
   # -----------------------------------------------------------------
  ,coverageGraphs = function() {
    
    info <- paste0(self$ngram(),"gram_",self$nfeat(),"features")
    
    df_cov_idx <- data.frame(qt = qtiles_vec, idx = self$coverers()[[1]]) 
    
    p_cov_idx <- ggplot(df_cov_idx , aes(x = qt, y = idx)) 
    p_cov_idx <- p_cov_idx +  geom_point(size = 3) 
    p_cov_idx <- p_cov_idx +  geom_abline(intercept = 0
      ,slope = max(self$coverers()[[1]]), color = "blue" )
    p_cov_idx <- p_cov_idx +  ggtitle(paste("Coverage",info))
    ggsave(paste0(info,"_nr",".png"), plot = p_cov_idx
      ,width = 10, height = 8, units = "cm")

    # percentuale coperta, diviso percentuale elementi per coprire
    df_cov_ratio <- data.frame(qt = qtiles_vec, idx = self$coverers()[[2]],
    qtf = paste(qtiles_vec ,round(self$coverers()[[1]],4)
      ,as.character(round(self$coverers()[[2]],4)) ,sep = " ")) 
    p_cov_ratio <- ggplot(df_cov_ratio ,aes(x = qt, y = qt/idx,label = qtf)) 
    p_cov_ratio <- p_cov_ratio +  geom_text(check_overlap = TRUE ,hjust = 0) 
    p_cov_ratio <- p_cov_ratio +  scale_x_continuous(limits = c(0.5,1.1))
    p_cov_ratio <- p_cov_ratio +  geom_hline(yintercept = 1 ,color = "red" 
      ,size = 2)
    p_cov_ratio <- p_cov_ratio +ggtitle(paste("Coverage",info))
    ggsave(paste0(info,"_ratio",".png"), plot = p_cov_ratio
    ,width = 10, height = 8, units = "cm")

    list(p_cov_idx = p_cov_idx
      ,p_cov_ratio)
  }
   
   
      
   # -----------------------------------------------------------------
  ,types_to_cover = function(qtile) {
    
    # only accept probs in global vector
    idx <- match(qtile,qtiles_vec , , nomatch = NA_integer_)
    if (is.na(idx)) {
      prt_error("illegal quantile:",qtile)
      stop()
    }
    
    private$..coverage$idxs[idx]
   }
   
   
   # -----------------------------------------------------------------
   ,dump = function() {
     ret <- ""
     ret <- paste0(ret,"(private$..nfeat=",private$..nfeat,")")
     ret <- paste0(ret,"(private$..coverage",private$..coverage[[1]]
       ," - ",private$..coverage[[2]],")")
     
   }
  ) # public 

  # ==================================================================
 ,active = list(
  # ==================================================================

   # -----------------------------------------------------------------
   dummy = function(value) {
      if (missing(value)) {
        # private$.age
      } else {
        #stop("`$age` is read only", call. = FALSE)
      }
    }
  )
    
  # ==================================================================
  ,private = list(
  # ==================================================================
    ..nfeat = NA_integer_
    ,..dtf = NULL
    ,..coverage = NA_real_
    ,..ngram = 0
  )

    
)

# ====================================================================
#                      GLOBALS, STANDARD
# ====================================================================

o_1grams_basic <- if (exists("o_1grams_basic")) o_1grams_basic else NULL
o_2grams_basic <- if (exists("o_2grams_basic")) o_2grams_basic else NULL
o_3grams_basic <- if (exists("o_3grams_basic")) o_3grams_basic else NULL


