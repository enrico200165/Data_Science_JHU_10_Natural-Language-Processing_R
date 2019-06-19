
require(R6)
require(gridExtra)
require(ggplot2)

source("020_pred_globals.R")
# source("020_pred_ngram_bare_dtf.R")
source("022_pred_ngram_reduced_dtf.R")


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

    private$..dtf <- dtf_par
    private$..nfeat <- nrow(private$..dtf );
    tmp <- coverage_of_freq_list(
      private$..dtf$frequency ,qtiles_vec ,self$size())
    private$..coverage <- data.frame(tmp)
    if (all(TYPES_COLNAMES[1:3] %in% names(private$..dtf))) {
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

    } else if (TYPES_COLNAMES[1] %in% names(private$..dtf)) {
      prt("building 1 grams object")
      private$..ngram = 1
      # assume key are already set
      # setkeyv(private$..dtf ,TYPES_COLNAMES[1])
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
    info <- paste0(self$ngram(),"gram_",formatC(self$nfeat(), format="f"
        , big.mark=",", digits=0), "_features")
    
    df_cov_idx <- data.frame(qt = qtiles_vec,idx = self$coverage_tables()[["idxs"]]) 
    
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



###########################################################
#         TEST SUPPORT, CAN BE MOVED IN TEST FILE
###########################################################

# --------------------------------------------------------------------
build_small_test_objects <- function(force_calc) {
  # --------------------------------------------------------------------
  
  prt("build_small_test_objects() - start")
  
  rie(dtf_1gram_sep, force_calc, , produce_ngram_bare_dtf_1)
  o_1gram_test <- DTF_Basic$new(dtf_1gram_sep[1:100, ])
  
  rie(dtf_2gram_sep, force_calc, , produce_ngram_bare_dtf_2)
  o_2gram_test <- DTF_Basic$new(dtf_2gram_sep[1:100, ])
  
  rie(dtf_3gram_sep, force_calc, , produce_ngram_bare_dtf_3)
  o_3gram_test <- DTF_Basic$new(dtf_3gram_sep[1:100, ])
  
  if (F) {
  }
  
  prt("build_small_test_objects() - end")
  
  list(
    o1 = o_1gram_test
    , o2 = o_2gram_test
    , o3 = o_3gram_test
  )
}


# --------------------------------------------------------------------
test_nfeat_for_size <- function(o1, o2, o3) {
  
  s1 <- o1$size()
  s1_target <- s1 * 0.75
  ret <- o1$nfeat_freq_for_size(s1_target)
  prt("index", ret$last_idx, "min freq", ret$lower_freq)
  
  s2 <- o2$size()
  s2_target <- s2 * 0.75
  ret <- o2$nfeat_freq_for_size(s2_target)
  prt("index", ret$last_idx, "min freq", ret$lower_freq)
  
  
  s3 <- o3$size()
  s3_target <- s3 * 0.75
  ret <- o3$nfeat_freq_for_size(s3_target)
  prt("index", ret$last_idx, "min freq", ret$lower_freq)
  
  ret 
}



# --------------------------------------------------------------------
test_freq_ge_feat_idx <- function(o1, o2, o3) {
  
  f <- 100
  idx <- o1$freq_ge_feat_idx(f)
  # prt("index", ret$last_idx, "for frequencies >= ", ret$lower_freq)
  prt("index", idx, "for frequencies >= ", f)
  frqs <- o1$get_frq_df()$frequency
  stopifnot(frqs[idx] >= f)
  stopifnot(frqs[idx + 1] < f)
  prt("min frq", min(frqs[frqs >= f]))
  new_o <- o1$create_subset_4min_freq(f)
  prt("minimum frequency", new_o$get_min_frq())
  
  
  f <- 50
  idx <- o2$freq_ge_feat_idx(f)
  #prt("index", ret$last_idx, "for frequencies >= ", ret$lower_freq)
  prt("index", idx, "for frequencies >= ", f)
  frqs <- o2$get_frq_df()$frequency
  stopifnot(frqs[idx] >= f)
  stopifnot(frqs[idx + 1] < f)
  prt("min frq", min(frqs[frqs >= f]))
  new_o <- o2$create_subset_4min_freq(f)
  prt("minimum frequency", new_o$get_min_frq())
  
  
  f <- 10
  idx <- o3$freq_ge_feat_idx(f)
  #prt("index", ret$last_idx, "for frequencies >= ", ret$lower_freq)
  prt("index", idx, "for frequencies >= ", f)
  frqs <- o3$get_frq_df()$frequency
  stopifnot(frqs[idx] >= f)
  stopifnot(frqs[idx + 1] < f)
  prt("min frq", min(frqs[frqs >= f]))
  new_o <- o3$create_subset_4min_freq(f)
  prt("minimum frequency", new_o$get_min_frq())
  
  T
}



# --------------------------------------------------------------------
test_pred_classes <- function(force_calc)
# --------------------------------------------------------------------
{
  rie(qc, force_calc, NULL, readQCorp, data_dir_corpus_in())
  dtf_ngram_sep_list <- produce_ngram_bare_dtf(qc, force_calc)
  
  reduce_matrix <- rbind(c(20,20), c(2000,20), c(3000,20))
  ngrams_reduced_l <- reduce_dtfs(dtf_ngram_sep_list,reduce_matrix)
  
  
  rie(o_1grams_basic, force_calc, NULL,  DTF_Basic$new, ngrams_reduced_l[[2]])
  #assign("o_1grams_basic", o_1grams_basic, .GlobalEnv)
  
  # o_2grams_basic <- DTF_Basic$new(dtf_2gram_sep)
  rie(o_2grams_basic, force_calc, NULL, DTF_Basic$new, ngrams_reduced_l[[3]])
  # assign("o_2grams_basic", o_2grams_basic, .GlobalEnv)
  
  rie(o_3grams_basic, force_calc, NULL, DTF_Basic$new, ngrams_reduced_l[[4]])
  assign("o_3grams_basic", o_3grams_basic, .GlobalEnv)
  
  # test_nfeat_for_size(o_1grams_basic ,o_2grams_basic ,o_3grams_basic)
  
  test_freq_ge_feat_idx(o_1grams_basic, o_2grams_basic, o_3grams_basic)
  
  if (F) {
    print(o_1grams_basic$coverageGraphs()[[1]])
    keypress()
    print(o_1grams_basic$coverageGraphs()[[2]])
    keypress()
    print(o_2grams_basic$coverageGraphs()[[1]])
    keypress()
    print(o_2grams_basic$coverageGraphs()[[2]])
    keypress()
    print(o_3grams_basic$coverageGraphs()[[1]])
    keypress()
    print(o_3grams_basic$coverageGraphs()[[2]])
    keypress()
  }
}



###########################################################
#               TEMPORARY TEST
###########################################################



fulldata <- T
# memory.limit(size = 24000)
silent <- F
force_calc <- F
keypressWait <- T
use_full_corpus(F)

if (!exists("qc")) qc <- NULL

# build_small_test_objects()


test_pred_classes(force_calc)
stop("ok SUCCESSO")

force_calc = F

# --------------------------------------------------------------------
test_that("test_pred_classes"
          ,{
            
            
            
            
            rie(dtf_1gram_sep, force_calc, , produce_ngram_bare_dtf_1)
            # o_1grams_basic <- DTF_Basic$new(dtf_1gram_sep)
            rie(o_1grams_basic, force_calc, , DTF_Basic$new, dtf_1gram_sep)
            assign("o_1grams_basic", o_1grams_basic, .GlobalEnv)
            
            rie(dtf_2gram_sep,force_calc, , produce_ngram_bare_dtf_2)
            # o_2grams_basic <- DTF_Basic$new(dtf_2gram_sep)
            rie(o_2grams_basic, force_calc, , DTF_Basic$new, dtf_2gram_sep)
            assign("o_2grams_basic", o_2grams_basic, .GlobalEnv)
            
            rie(dtf_3gram_sep,force_calc, , produce_ngram_bare_dtf_3)
            # o_3grams_basic <- DTF_Basic$new(dtf_3gram_sep)
            rie(o_3grams_basic, force_calc, , DTF_Basic$new, dtf_3gram_sep)
            assign("o_3grams_basic", o_3grams_basic, .GlobalEnv)
            
            # test_nfeat_for_size(o_1grams_basic ,o_2grams_basic ,o_3grams_basic)
            
            test_freq_ge_feat_idx(o_1grams_basic, o_2grams_basic
                                  , o_3grams_basic)
            
            if (T) {
              print(o_1grams_basic$coverageGraphs()[[1]])
              keypress()
              print(o_1grams_basic$coverageGraphs()[[2]])
              keypress()
              print(o_2grams_basic$coverageGraphs()[[1]])
              keypress()
              print(o_2grams_basic$coverageGraphs()[[2]])
              keypress()
              print(o_3grams_basic$coverageGraphs()[[1]])
              keypress()
              print(o_3grams_basic$coverageGraphs()[[2]])
              keypress()
            }
          })

