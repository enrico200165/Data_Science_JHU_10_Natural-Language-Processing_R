require(dplyr)
require(ggplot2)
require(stringr)

source("01_globals.R")

#---------------------------------------------------------------------
  wcForFile <- function(fdir,fname) 
#---------------------------------------------------------------------
# wc *nix command on file
# ret: dataframe row with full wc output
{ 
  # print(paste("wcForFile() file:",fname))
  
  stopifnot(dir.exists(fdir))
  file_path <- file.path(fdir,fname)
  stopifnot(file.exists(file_path))
  
  command = "wc"
  args <- "--bytes"
  args <- paste(args,"--chars")
  args <- paste(args,"--words")
  args <- paste(args,"--lines")
  args <- paste(args,"--max-line-length")
  args <- paste(args, file_path)
  out <- system2(command = command
   ,args = args, stdout = TRUE
   ,stderr = "" ,stdin = "" 
   ,input = NULL
  #,env = character(), wait = TRUE
  #,minimized = FALSE, invisible = TRUE, timeout = 0
  )
  
  # print(out)
  spl <- strsplit(trimws(out)," +")
  spl <- spl[[1]] # extract from list
  splNums <- as.integer(spl[1:length(spl)-1])
  
  wcDfRow <- data.frame(
    TXT_FNAME = fname
    ,TXT_BYTES = splNums[4]
    ,TXT_NCHAR = splNums[3]
    ,TXT_NTOKENS = splNums[1]
    ,TXT_NNLINES = splNums[1]
    ,max_line_len = splNums[5]
    ,stringsAsFactors=FALSE
  )
  names(wcDfRow) <- c(TXT_FNAME,TXT_BYTES,TXT_NCHAR,TXT_NTOKENS,TXT_NNLINES,"max_line_len")
  # print(wcDfRow)

  wcDfRow
}

  
  
#---------------------------------------------------------------------
  lsForFile <- function(fname, fdir, sizeOpt) 
#---------------------------------------------------------------------
# ls -l -M *nix command on file
# ret: dataframe row with full wc output
{ 
    # print(paste("lsForFile() file:",fname))
    
    stopifnot(dir.exists(fdir))
    file_path <- file.path(fdir,fname)

    command = "ls"
    args <- "-l"
    if (!missing(sizeOpt)) {
      args <- paste(args,sizeOpt)
    }
    args <- paste(args,file_path)
    out <- system2(command = command
     ,args = args ,stdout = TRUE, stderr = "" ,stdin = ""
     ,input = NULL
    #,env = character(), wait = TRUE
    #,minimized = FALSE, invisible = TRUE, timeout = 0
    )
    # print(out)

    spl <- strsplit(trimws(out)," +")
    spl <- spl[[1]] # extract from list
    size = str_extract(spl[5], "[0-9]+")
    units = str_extract(spl[5], "[^0-9]+$")
    
    dfRow <- data.frame(
      TXT_FNAME = fname
      ,TXT_SIZE = size
      ,TXT_SIZE_U = units
      ,stringsAsFactors=FALSE
    )
    names(dfRow) <- c(TXT_FNAME,TXT_SIZE,TXT_SIZE_U)
    # print(dfRow)

    dfRow
  }
  

# --------------------------------------------------------------------
  getWCInfo <- function(data_dir, ptrn,userFN)
# --------------------------------------------------------------------
# runs userFN on a set of file
# ret: wc output in dataframe
{
  stopifnot(dir.exists(data_dir))
  
  flist <- list.files(data_dir,ptrn)
  stopifnot(length(flist) > 1)
  
  myrows <- lapply(flist, userFN)
  wcDf <- bind_rows(myrows)
}

  
# --------------------------------------------------------------------
  physicalAnalysis <- function(data_dir_corpus_in)
#---------------------------------------------------------------------
{

  linux_wc_bound <- function(x) wcForFile(data_dir_corpus_in,x)
  lsForFile_bound <- function(x) lsForFile(x,data_dir_corpus_in, "--block-size=M") 
  
  ls_df <- getWCInfo(data_dir_corpus_in,"*.txt",lsForFile_bound)
  wc_df <- getWCInfo(data_dir_corpus_in,"*.txt",linux_wc_bound)
  basic_df <- lngCountryTypeDF(ls_df[,1])

  ret = basic_df
  ret = merge(ret,ls_df,key = 1)
  ret = merge(ret,wc_df,key = 1)
  ret
}


#---------------------------------------------------------------------
  lngCountryTypeDF <- function(fnames) 
#---------------------------------------------------------------------
{
    mydf <- data.frame(dummy = 1:length(fnames))
    
    # print(fnames)
    splits <- str_split(fnames,"[_\\.]")
    
    mydf[[TXT_FNAME]] = fnames 
    mydf[[TXT_CTR]] = sapply(splits, function(x) x[1]) 
    mydf[[TXT_LNG]] = sapply(splits, function(x) x[2]) 
    mydf[[TXT_TYP]] = sapply(splits, function(x) x[3]) 

    mydf[["dummy"]] = NULL
    
    mydf
}


# --------------------------------------------------------------------
  basicPlot <- function(dfPar,xPar,yPar,fillPar)  
# --------------------------------------------------------------------
{
  p <- ggplot(data=dfPar, aes_string(x = xPar, y = yPar)) 
  p <- p + geom_bar(stat="identity"
                      ,aes_string(fill = fillPar), position = "dodge")
  # p <- p + facet_grid(~language)
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

  
#--------------------------------------------------------------------
  freq_distrib <- function(doc_fm, lang, rem_stopw,
    proportions) 
#--------------------------------------------------------------------
  {
    # https://tutorials.quanteda.io/statistical-analysis/frequency/
    # the dfm() function applies certain options by default, 
    # such as tolower() – a separate function for lower-casing texts 
    # – and removes punctuation. All of the options to tokens() can 
    # be passed to dfm(), however.
    
    if (missing(rem_stopw))
      rem_stopw <- FALSE
    if (missing(proportions))
      proportions = FALSE
    
    
    if (rem_stopw) {
      stopw <- stopwords(tolower(lang))
      doc_fm <- dfm(doc_fm, remove = stopw)
    }
    
    dfm_lang <- dfm_subset(doc_fm ,language == tolower(lang))
    stopifnot(ndoc(dfm_lang) == 3)
    
    if(proportions)
      dfm_lang <- dfm_weight(dfm_lang, scheme = "prop")

    frq_grp <- textstat_frequency(dfm_lang
      ,groups = TXT_TYP)
    
    frq_grp
}


#--------------------------------------------------------------------
plot_freq_distrib_q <- function(frq_d,faceted
    ,title_par, x_axis_lab_par, y_axis_lab_par
    ,legend_title_par, no_ticks) 
#--------------------------------------------------------------------
{
  if(missing(no_ticks)) no_ticks <- FALSE
  
  # move from frequencies to frequencies of frequencies
  frequenze <- frq_d$frequency
  # frequenze <- log10(frequenze)
  frequenze <- frequenze/sum(frequenze)
  max_freq <- max(frequenze)
  avg_freq <- mean(frequenze)
  sd_freq <- sd(frequenze)
  left_lim <- 0
  right_lim <- max_freq
  nr_cuts <- 200
  cp <- seq(left_lim,right_lim
            ,by = (right_lim-left_lim)/(nr_cuts-1))
  # labels
  cut_labels  <- round(cp*100,2)
  cut_labels  <- as.character(cut_labels)[-1]
  cut_labels <- paste(cut_labels,"%",sep = "")

  freq_cut <- cut(frequenze,breaks = cp, labels = cut_labels)

  nr_plot <- nr_cuts*1
  df_freq_cuts <- data.frame(frq = freq_cut[1:nr_plot]
     ,gruppo = frq_d$group[1:nr_plot]
    )
  
  p <- ggplot(data = df_freq_cuts, aes(x = frq)
    #, fill = gruppo
    )
  
  p <- p + geom_histogram(stat="count")
  p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 0,
    vjust = 0))
  # if (faceted) {
  #   p <- p + facet_grid(gruppo ~ .)
  # }
  
  p <- p + ggtitle(title_par)
  p <- p + xlab(x_axis_lab_par)
  p <- p + ylab(y_axis_lab_par)
  p <- p + guides(fill=guide_legend(title=legend_title_par))
  
  if (no_ticks) {
    p <- p  + theme(# axis.title.x=element_blank(),
      axis.text.x=element_blank() ,axis.ticks.x=element_blank()
      ,axis.text.y=element_blank() ,axis.ticks.y=element_blank())
    # p <- p + scale_x_discrete(labels = NULL)
    # p <- p + scale_y_continuous(labels = NULL)
  }
  
  p
  }
  
  
  
  
  #--------------------------------------------------------------------
  types_distrib <- function(qc_par, lng, ngram ,rem_stopw, faceted) 
    #--------------------------------------------------------------------
  {
    # https://tutorials.quanteda.io/statistical-analysis/frequency/
    # the dfm() function applies certain options by default, 
    # such as tolower() – a separate function for lower-casing texts 
    # – and removes punctuation. All of the options to tokens() can 
    # be passed to dfm(), however.
    
    if (missing(faceted))
      faceted <- FALSE
    
    if (missing(rem_stopw))
      rem_stopw <- FALSE
    
    qc <- corpus_subset(qc_par, language == char_tolower(lng))
    stopifnot(ndoc(qc) == 3)
    
    if (ngram == 1) { # can remove freely
      toks <- tokens(qc   
                     ,remove_numbers = T,remove_punct = T
                     ,remove_symbols = T, remove_twitter = T
                     ,remove_url = T)
      toks <- tokens_tolower(toks)
      if (rem_stopw) { # manage stopwords in/exclusion
        stopw <- stopwords(tolower(lng))
        toks <- tokens_remove(toks,stopw)
      }
    } else { # must not remove things
      # sentence splitting avoid silly ngrams
      # with it punct removal seems OK
      qc <- corpus_reshape(qc, to = "sentences")
      toks <- tokens(qc   
                     ,remove_numbers = F,remove_punct = T
                     ,remove_symbols = F, remove_twitter = T
                     ,remove_url = T)
      toks <- tokens_tolower(toks)
      if (rem_stopw) { # manage stopwords in/exclusion
        stopw <- stopwords(tolower(lng))
        toks <- tokens_remove(toks,stopw)
      }
      
    }
    
    dfm_lang <- dfm(toks,ngrams = ngram)
    
    ntypes <- if (faceted) 4 else 24
    grouping <- if (faceted) TXT_TYP else NULL
    # group by text type
    frq_grp <- textstat_frequency(dfm_lang, n = ntypes
                                  , groups = grouping
    )
    
    frq_grp    
  }
#--------------------------------------------------------------------
  types_freq_plot_q <- function(frq_grp, faceted
                                ,title_par, x_axis_lab_par, y_axis_lab_par,legend_title_par) 
  #--------------------------------------------------------------------
  {
    if (missing(faceted))
      faceted <- FALSE
    
    p <- ggplot(data = frq_grp, aes(x = reorder(feature, frequency)
                                    , y = frequency))
    p <- p + geom_point()
    p <- p + coord_flip() 
    p <- p + labs(x = NULL, y = "Frequency") 
    if (faceted) p <- p + facet_grid(group ~ .)
    p <- p + theme_minimal()
    
    p <- p + ggtitle(title_par)
    p <- p + xlab(x_axis_lab_par)
    p <- p + ylab(y_axis_lab_par)
    p <- p + guides(fill=guide_legend(title=legend_title_par))
    
    p
  }
  
  
# ------------------------------------------------------------------------ 
  types_coverage <- function(qc, lng) 
# ------------------------------------------------------------------------ 
  {
    tks  <- tokens(corpus_subset(qc,language == lng)
                   ,remove_punct = T)
    doc_fm <- dfm(tks, remove = stopwords(lng))
    frq <- textstat_frequency(doc_fm)
    frq$props <- frq$frequency/sum(frq$frequency)
    frq$cumul <- cumsum(frq$props)
    idx <- which(frq$cumul >= 0.5)[1]
    pct_types_for_50pct_coverage <- idx/nrow(frq)
    (frq$feature[1:idx])
    
    prt("nr. for coverage:",idx
        ,"pct features for 50% coverage:",pct_types_for_50pct_coverage
        ,"last feature:",frq$feature[idx])
    
    prt("vrification that we cover 50% summing simple probs",sum(frq$props[1:idx]))
    
    list(idx = idx,pct = pct_types_for_50pct_coverage)
  }

  
  # ====================================================================
  #                   Tests (quick verification)
  # ====================================================================
  
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
  test_physicalAnalysis <- function() 
    # -------------------------------------------------------------------
  {
    if (readIfEmpty(phys_df)) {
    } else {
      print("NO, I could NOT read it")
      phys_df <- physicalAnalysis(data_dir_corpus_subset)
      serializeIfNeeded(phys_df,FALSE)  
    }
    
    p <- ggplot(data=phys_df, aes(x = type, y = n_token)) 
    p <- p + geom_bar(stat="identity"
                      ,aes(fill = language), position = "dodge")  
    # p <- p + facet_grid(~language)
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(p)
    
    print(str(phys_df))
    print(phys_df)
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
  
  # p <- freq_distrib(d,"en",T)
  freq_d <- freq_distrib(dfm_full,"en" ,rem_stopw = T ,proportions = F)
  print(freq_d)
  freq_d
}


# -------------------------------------------------------------------
test_plot_freq_distrib_q <- function()
# -------------------------------------------------------------------
{
    freq_d <- freq_distrib(dfm_full,"en",rem_stopw = T 
      ,proportions = F)
    p <- plot_freq_distrib_q( freq_d,faceted = FALSE
      ,"title write it"
      ,"X "
      ,"Y"
      ,"Legend"
      , no_ticks = F
      )
    print(p)
}
  
# ---------------------------------------------------------------------  
test_types_freq <- function() 
  # ---------------------------------------------------------------------  
{
  read_dir = if (use_full_corpus) data_dir_corpus_full else data_dir_corpus_subset
  readIfEmpty(qc_full) || readQCorp(read_dir, FALSE)
  fct = F
  d <- types_distrib(qc_full,"en",2,rem_stopw = T, faceted = fct)
  p <- types_freq_plot_q(d, faceted = fct
                         ,"Most frequent words, ngrams"
                         ,"Words"
                         ,"Occurrences","")
  print(p)
}


test_types_coverage <- function(qc) 
{
  print(types_coverage(qc_full,"en"))
}   

  
# -------------------------------------------------------------------
  test_ev_nlp_eda_lib.R <- function()
# -------------------------------------------------------------------
{
  # read_dir = if (use_full_corpus) data_dir_corpus_full else data_dir_corpus_subset
  # readIfEmpty(qc_full) || readQCorp(read_dir, FALSE)
  
  # test_readIfEmpty_serializeIfNeeded(data_dir_corpus_subset)
  # keypress()
  
  # test_physicalAnalysis()
  # keypress()
  
  # test_basicPlot(need a plot)

  # test_freq_distrib()
  # keypress()
  
  test_plot_freq_distrib_q()
  # keypress()
  # 
  # test_types_freq()
  # keypress()
  # 
  # test_types_coverage()
  # keypress()
  
}
#  
test_ev_nlp_eda_lib.R()

  
  
    
  
    



