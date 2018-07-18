
  
source("ev_nlp_eda_lib.R")  

# -------------------------------------------------------------------
  test_basicPlot <- function(mydf) 
# -------------------------------------------------------------------
  {
    if (!(missing(mydf)) && !is.null(mydf)) {
      print(basicPlot(mydf ,TXT_TYP,TXT_NTOKENS ,TXT_LNG))
      print(basicPlot(mydf ,TXT_LNG ,TXT_NTOKENS ,TXT_TYP))
    } else {
      print("test_basicPlot() did not receive a usable df")
    }
  }

  
  
# -------------------------------------------------------------------
  test_physical_analysis_plots <- function(data_dir) 
# -------------------------------------------------------------------
{

  ret <- physical_analysis_plots(data_dir)
  grid.arrange(
      ret[[1]]  # plot_phys_an_fsize 
      ,ret[[2]] # plot_phys_an_ntokens 
      ,ret[[3]] # plot_phys_an_nlines 
      ,ret[[4]] # plot_phys_an_max_line_len
    )
  # keypress()
}
  

# -------------------------------------------------------------------
  test_readIfEmpty_serializeIfNeeded <- function(data_dir_corpus_in)
# -------------------------------------------------------------------
  {
    
    if (readIfEmpty(linux_wc)) {
    } else {
      print("NO, I could NOT read serialization")
      linux_wc <- function(x) wcForFile(data_dir_corpus_in,x)
      wc_df <- getWCInfo(data_dir_corpus_in,"*.txt",linux_wc)
      # print(wc_df)
      serializeIfNeeded(linux_wc,FALSE)  
    }
    linux_wc <- NULL
    if (readIfEmpty(linux_wc)) {
    } else {
      print("NO, I could NOT read serialization")
      linux_wc <- function(x) wcForFile(data_dir_corpus_in,x)
      wc_df <- getWCInfo(data_dir_corpus_in,"*.txt",linux_wc)
      # print(wc_df)
      serializeIfNeeded(linux_wc,FALSE)  
    }
    
    if (readIfEmpty(linux_ls)) {
    } else {
      print("NO, I could NOT read it")
      linux_ls <- function(x) lsForFile(x,data_dir_corpus_in, "--block-size=M") 
      wc_df <- getWCInfo(data_dir_corpus_in,"*.txt",linux_ls)
      print(wc_df)
      serializeIfNeeded(linux_ls,FALSE)  
    }
    if (readIfEmpty(linux_ls)) {
    } else {
      print("NO, I could NOT read it")
      linux_ls <- function(x) lsForFile(x,data_dir_corpus_in, "--block-size=M") 
      wc_df <- getWCInfo(data_dir_corpus_in,"*.txt",linux_ls)
      print(wc_df)
      serializeIfNeeded(linux_ls,FALSE)  
    }
    
  }
  

# -------------------------------------------------------------------
  test_freq_distrib <- function()
# -------------------------------------------------------------------

{
  freq_d <- freq_distrib(dfm_full
    ,"en" ,rem_stopw = T ,proportions = F)

  freq_d
}


# --------------------------------------------------------------------
test_freq_of_freq_an_simple <- function()
# --------------------------------------------------------------------
{
    # generate text with frequencies of frequencies 1
  # ie. frequency 1 to 4 each once
  freqcies_1 <- sapply(1:5, function(i) 
    rep(paste0(letters[i],", ",collapse = ""),i))
  freqcies_1 <- unlist(sapply(freqcies_1,function(x) paste(x, collapse = "")))
  freqcies_1 <- paste0(freqcies_1, collapse = "")

  # 10 frequencies ten, ie. ten letters/numbers each with frequency 10
  freqcies_10 <-rep(paste0(letters[11:20],". ", collapse = ""),10)
  freqcies_10 <- paste0(freqcies_10, collapse = "")
  # 5 frequencies 11, ie. ten letters/numbers each with frequency 10
  freqcies_5 <-rep(paste0(letters[21:25],". ", collapse = ""),11)
  freqcies_5 <- paste0(freqcies_5, collapse = "")
  
  text_freq1 <- paste0(freqcies_1,freqcies_10, collapse = "")
  text_freq2 <- paste0(freqcies_1,freqcies_5, collapse = "")
  
  qc_small <- corpus(c(text_freq1, text_freq2, ""))
  docvars(qc_small) <- data.frame(language = c("en","en","en")
    , type = c ("blog","news","blog"))
  texts(qc_small) <- gsub("[[:punct:]]"," ",texts(qc_small))

  ### caveat: frequencies occurring once are in TWO documents
  dfm_small <- dfm(qc_small)
  freq_small <- freq_distrib(dfm_small,"en",rem_stopw = F ,proportions = F)
  (freq_small)
  freq_of_freq_small <- freq_of_freq_cutted(freq_small,20,F)
  
  p <- plot_freq_distrib_user_managed(freq_of_freq_small
    ,faceted = F
    , remove_outliars = F
    ,"title write it"
    ,"X "
    ,"Y"
    ,"Text Type"
    , no_ticks = F
    ,0,0
  )
  print(p)
}  


# --------------------------------------------------------------------
test_freq_of_freq_an_realistic <- function()
  # --------------------------------------------------------------------
{
  
  rie(freq_of_freq_plots,freq_of_freq_an)
  
  # freq_of_freq_plots <- freq_of_freq_an()
  
  print(freq_of_freq_plots[[2]])
  keypress()
  
  print(freq_of_freq_plots[[1]])
  keypress()
}  


# ---------------------------------------------------------------------  
test_types_freq_an_q <- function(qc, fct) 
# ---------------------------------------------------------------------  
{
  if((missing(fct))) fct <- FALSE 

  # types_freq_an_q_plots <- types_freq_an_q(qc, fct)
  rie(types_freq_an_q_plots , types_freq_an_q ,qc ,fct)
  print(types_freq_an_q_plots[[1]])
  keypress()
  print(types_freq_an_q_plots[[2]])
  keypress()
  print(types_freq_an_q_plots[[3]])
  keypress()
  # print(types_freq_an_q_plots[[4]])
  # keypress()
  # print(types_freq_an_q_plots[[5]])
}



 
# ---------------------------------------------------------------------  
test_types_freq_an_wordcloud <- function(qc, fct) 
# ---------------------------------------------------------------------  
{
  if((missing(fct))) fct <- FALSE 

  types_freq_an_wcloud(qc, fct)
  # print(plots[[1]][[2]]);  
  # keypress()
  # 
  # print(plots[[2]][[2]])
  # keypress()
  # print(plots[[3]][[2]])
  # keypress()
  # print(plots[[4]][[2]])
  # keypress()
  # print(plots[[5]][[2]])
}

  

#---------------------------------------------------------------------
 test_types_coverage <- function(to_cover_par = 0.5)  
#---------------------------------------------------------------------
{
  silent <<- F

  types_coverage_an(qc_full ,to_cover_par)
}


# -------------------------------------------------------------------
  test_ev_nlp_eda_lib.R <- function()
# -------------------------------------------------------------------
{
 
  use_full_corpus(F, eda_re_init)
  
  test_physical_analysis_plots(data_dir_corpus_full); #keypress()
  
  test_freq_distrib(); #keypress()

  test_freq_of_freq_an_simple();   #keypress()
  
  test_freq_of_freq_an_realistic(); #keypress()
  
  test_types_freq_an_q(qc_full, fct = F) ;#keypress()
   
  test_types_freq_an_wordcloud(qc_full, F) ;#keypress()
    
  test_types_coverage() ;#keypress()
  
}
  
silent <- F
fulldata <- T

eda_re_init()
test_ev_nlp_eda_lib.R()




