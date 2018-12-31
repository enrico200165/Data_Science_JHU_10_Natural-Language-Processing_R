
require(R6)
require(gridExtra)
require(ggplot2)

source("02_pred_globals.R")
source("02_pred_ngram_bare_dtf.R")


qtiles_vec <- c(0.5, 0.55 ,0.6 ,0.65 ,0.7 ,0.75 ,0.8,0.85 ,0.9, 0.95
  ,0.96,0.97,0.98,0.99, 0.995)

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

    if (!(any("data.table" %in% class(dtf_par)))) {
      prt_error("class(dtf_par) = ",class(dtf_par))
      browser()
    }
    stopifnot(any("data.table" %in% class(dtf_par)))
    stopifnot(nrow(dtf_par$primo) > 0)
    
    
    # remove unneeded columns
    # dtf_par[ ,  c("docfreq","group","rank"):=NULL]
    
    private$..dtf <- dtf_par
    private$..nfeat <- nrow(private$..dtf );
    tmp <- coverage_of_freq_list(
      private$..dtf$frequency ,qtiles_vec ,self$size())
    private$..coverage <- data.frame(tmp)
    if (all(TYPES_COLNAMES %in% names(private$..dtf))) {
      prt("building 3 grams object")
      private$..ngram = 3
      # setkeyv(private$..dtf ,TYPES_COLNAMES[1:3])
      # below here maybe redundant or just unnecessary
      setindexv(private$..dtf ,TYPES_COLNAMES[1:2])
      setindexv(private$..dtf ,TYPES_COLNAMES[1])
      setindexv(private$..dtf ,TYPES_COLNAMES[2])
    } else if (all(TYPES_COLNAMES[1:2] %in% names(private$..dtf))) {
      prt("building 2 grams object")
      private$..ngram = 2
      # setkeyv(private$..dtf ,TYPES_COLNAMES[1:2])
      # below here maybe redundant or just unnecessary
      setindexv(private$..dtf ,TYPES_COLNAMES[1])
      setindexv(private$..dtf ,TYPES_COLNAMES[2])
    }
    else if (TYPES_COLNAMES[1] %in% names(private$..dtf)) {
      prt("building 1 grams object")
      private$..ngram = 1
      # setkeyv(private$..dtf ,TYPES_COLNAMES[1])
      # nearly surely unnecessary
      setindexv(private$..dtf ,TYPES_COLNAMES[1])
    }
    else {
      prt_error("unexpected situation setting keys")
      stop()      
    }
    # 
    prt("key",key(private$..dtf),"indexes",indices(private$..dtf))
  }
   
  ,finalize = function() {
    #rm(private$dtf)
    gc()
  }
   
  ,size = function() pryr::object_size(self)
   
  ,coverage_tables = function() private$..coverage

  ,print_coverage = function() { print(private$..coverage) }

  ,get_frq_df = function() { private$..dtf }

  ,get_min_frq = function() { self$get_frq_df()$frequency[self$nfeat()] }

  ,nfeat = function() private$..nfeat

  ,ngram = function() private$..ngram

   
   # -----------------------------------------------------------------
   ,create_subset_4min_freq = function(frq)
   # -----------------------------------------------------------------
   # returns nr of features and minimum frequency
   {
     cut_idx <- self$freq_ge_feat_idx(frq)
     subset_frqs <- self$get_frq_df()
     
     new_o <- DTF_Basic$new(subset_frqs[1:cut_idx])
     
     new_o
   }
   
   
   
   # -----------------------------------------------------------------
   ,nfeat_freq_for_size = function (target_size_bytes)
   # -----------------------------------------------------------------
   # Estimates, probably very inaccurately, number of features that
   # should correspond to a given size
   # assumes frequencies in decreasing order
   # returns nr of features and minimum frequency
   {
     ratio <- as.numeric(target_size_bytes/self$size())
     if (ratio >= 1) {
       prt_warn("ratio > = 1, forcing to 0.98")
       ratio <- 0.98
     }
     last_idx <- floor((nrow(private$..dtf)*ratio))
     lower_freq <- private$..dtf$frequency[last_idx] -1 #conservative, 
     # several elems might have 
     
     # they might be misaligned, because of both rounding and
     # memory measure being inaccurate due to sharing, optimizations
     list(
       last_idx = last_idx
      ,lower_freq = lower_freq
     )
   }
   
   
         
   # -----------------------------------------------------------------
  ,coverageGraphs = function() {
    
    # info <- paste0(self$ngram(),"gram_",self$nfeat(),"_features")
    info <- paste0(self$ngram(),"gram_",formatC(self$nfeat(), format="f", big.mark=",", digits=0)
                   ,"_features")
    
    df_cov_idx <- data.frame(qt = qtiles_vec
      ,idx = self$coverage_tables()[["idxs"]]) 
    
    p_cov_idx <- ggplot(df_cov_idx , aes(x = qt, y = idx)) 
    p_cov_idx <- p_cov_idx +  geom_point(size = 3) 
    p_cov_idx <- p_cov_idx +  geom_abline(intercept = 0
      ,slope = max(self$coverage_tables()[["idxs"]]), color = "blue" )
    p_cov_idx <- p_cov_idx +  ggtitle(paste("Coverage",info))
    ggsave(paste0(info,"_nr",".png"), plot = p_cov_idx
      ,width = 10, height = 8, units = "cm")

    # percentuale coperta, diviso percentuale elementi per coprire
    df_cov_ratio <- data.frame(qt = qtiles_vec
      ,idx = self$coverage_tables()[["pcts"]]
      ,qtf = paste(qtiles_vec 
      ,as.character(round(self$coverage_tables()[["pcts"]],4)) 
      ,formatC(self$coverage_tables()[["frqs"]],format="f", big.mark=",", digits=0)
      ,self$coverage_tables()[["sizs"]] ,sep = " ")
    )
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
  ,freq_ge_feat_idx = function(frq) 
  # greatest index, inclusive, of frequencies >= frq
  # created to use to cut down data volume
  {
    
    stopifnot(!is.null(frq) && frq > 1)
    
    frqs_vect <- self$get_frq_df()$frequency # just alias
    
    idx <- which(frqs_vect < frq )[1] # first smaller index
    stopifnot(idx > 0) # paranoid check
    idx <- if (idx >= 2) (idx-1) else idx
   }

   
      
   # -----------------------------------------------------------------
   ,dump = function() {
     ret <- ""
     ret <- paste0(ret,"(private$..nfeat=",private$..nfeat,")")
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

