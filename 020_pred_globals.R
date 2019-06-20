
# ####################################################################
#                       MODULE MISSION
# globals for predicton model - nothing more
# 
# ####################################################################

# --------------------------------------------------------------------
#                  CONSTANSTS
# --------------------------------------------------------------------

TYPE1_COLNAME <- "primo"   # column with type 1 of an ngram
TYPE2_COLNAME <- "secondo" # column with type 2 of an ngram
TYPE3_COLNAME <- "terzo"   # column with type 3 of an ngram
TYPES_COLNAMES <- c(TYPE1_COLNAME, TYPE2_COLNAME , TYPE3_COLNAME)

PREDECESSOR_FREQUENCY <- "pdcessor_freq"
FREQUENCY_COL <- "frequency"


PRED_NGRAM_FNAMES <- paste0("pred_",1:3,"gram.rds")

# --- bare DTF with ngrams probabilities
if (!exists("dtf_1gram_sep")) dtf_1gram_sep <- NULL
if (!exists("dtf_2gram_sep")) dtf_2gram_sep <- NULL
if (!exists("dtf_3gram_sep")) dtf_3gram_sep <- NULL

if (!exists("dtf_1gram")) dtf_1gram <- NULL
if (!exists("dtf_2gram")) dtf_2gram <- NULL
if (!exists("dtf_3gram")) dtf_3gram <- NULL


qtiles_vec <- c(0.5, 0.55 ,0.6 ,0.65 ,0.7 ,0.75 ,0.8,0.85 ,0.9, 0.95
                ,0.96,0.97,0.98,0.99, 0.995)


# --------------------------------------------------------------------
dtf_info <- function(dtf, verbose = F)
# --------------------------------------------------------------------
{
  prt("info about",deparse(substitute(dtf))
      ,"size",XiB(pryr::object_size(dtf))
      ,"nr features:",nrow(dtf)
      ,"memory/feature ratio", round(pryr::object_size(dtf)/nrow(dtf),0)
  )
  prt("frequencies, min",min(dtf[[FREQUENCY_COL]]), " max: ", max(dtf[[FREQUENCY_COL]]))

  has_pred_freq <- dtf[ , if(exists(PREDECESSOR_FREQUENCY)) T else F ]
  if(has_pred_freq)
    prt("predec freq, min",min(dtf[[PREDECESSOR_FREQUENCY]])
        ," max: ", max(dtf[[PREDECESSOR_FREQUENCY]]))

  if (verbose) {
    n <- min(42,nrow(dtf)/2)
    print(head(dtf, n))
    print(tail(dtf, n))
  }
}


# --------------------------------------------------------------------
coverage_of_freq_list <- function(frq_vect, qtiles_vec, size = 0)
  # --------------------------------------------------------------------
{
  stopifnot(!is.null(frq_vect) && exists("frq_vect"))
  stopifnot(!is.null(qtiles_vec) && exists("qtiles_vec"))
  
  # create empty vectors
  idxs <- numeric(length =length(qtiles_vec))
  pcts <- numeric(length =length(qtiles_vec))
  frqs <- numeric(length =length(qtiles_vec))
  sizs <- numeric(length =length(qtiles_vec))
  
  # naive attempt to avoid underflow
  molt <- 100 * 1000
  props <- frq_vect*molt
  props <- props/sum(frq_vect)
  stopifnot(!strict() || round(sum(props)-(1*molt),5) == 0)
  
  # get proportions
  cumul <- cumsum(props) # cumulative
  
  for (i in seq_along(qtiles_vec)) {
    
    qtile <- qtiles_vec[i]
    idx_set <- which(cumul >= qtile*molt)
    if (length(idx_set) <= 0) {
      prt_warn("unable to find quantile")
      prt_warn("freq vector:", frq_vect)
      stop()
    }
    idxs[i] <- idx_set[1]
    pcts[i] <- idxs[i]/length(frq_vect)
    frqs[i] <- frq_vect[idx_set[1]]
    sizs[i] <- XiB(pcts[i]*size)
  }
  
  list(
    qtls = qtiles_vec
    ,idxs = idxs
    ,pcts = pcts
    ,frqs = frqs
    ,sizs = sizs)
}


# ---------------------------------------------------------
coverageGraphs = function(dtf, n, qtiles_vec) 
# ---------------------------------------------------------

{
  nfeatures <- nrow(dtf)
  info <- paste0(n,"gram_",formatC(nfeatures, format="f"
    ,big.mark=",", digits=0), "_features ", tstmp_fname())

  tmp <- coverage_of_freq_list(dtf$frequency ,qtiles_vec ,nfeatures)
  coverage_tables <- data.frame(tmp)
  
  df_cov_idx <- data.frame(qt = qtiles_vec, idx = coverage_tables[["idxs"]]) 
  
  p_cov_idx <- ggplot(df_cov_idx , aes(x = qt, y = idx)) 
  p_cov_idx <- p_cov_idx +  geom_point(size = 3) 
  p_cov_idx <- p_cov_idx +  geom_abline(intercept = 0
                                        ,slope = max(coverage_tables[["idxs"]]), color = "blue" )
  p_cov_idx <- p_cov_idx +  ggtitle(paste("Coverage",info))
  ggsave(paste0(info,"_nr",".png"), plot = p_cov_idx
         ,width = 16, height = 8, units = "cm")

  # percentuale coperta, diviso percentuale elementi per coprire
  df_cov_ratio <- data.frame(qt = qtiles_vec
                             ,idx = coverage_tables[["pcts"]]
                             ,qtf = paste(qtiles_vec, as.character(round(coverage_tables[["pcts"]],4)) 
                                          ,formatC(coverage_tables[["frqs"]],format="f", big.mark=",", digits=0)
                                          ,coverage_tables[["sizs"]] ,sep = " ")
  )
  
  p_cov_ratio <- ggplot(df_cov_ratio ,aes(x = qt, y = qt/idx,label = qtf)) 
  p_cov_ratio <- p_cov_ratio +  geom_text(check_overlap = TRUE ,hjust = 0) 
  p_cov_ratio <- p_cov_ratio +  scale_x_continuous(limits = c(0.5,1.1))
  p_cov_ratio <- p_cov_ratio +  geom_hline(yintercept = 1 ,color = "red" 
                                           ,size = 2)
  p_cov_ratio <- p_cov_ratio +ggtitle(paste("Coverage",info))
  ggsave(paste0(info,"_ratio",".png"), plot = p_cov_ratio
         ,width = 10, height = 8, units = "cm")

  list(p_cov_idx = p_cov_idx, p_cov_ratio)
}

