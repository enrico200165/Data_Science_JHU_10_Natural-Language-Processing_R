require(dplyr)
require(ggplot2)
require(gridExtra)
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
    ,TXT_NNLINES = splNums[1]
    ,TXT_NTOKENS = splNums[2]
    ,TXT_NCHAR = splNums[3]
    ,TXT_BYTES = splNums[4]
    ,max_line_len = splNums[5]
    ,stringsAsFactors=FALSE
  )
  names(wcDfRow) <- c(TXT_FNAME ,TXT_NNLINES ,TXT_NTOKENS 
  ,TXT_NCHAR ,TXT_BYTES  ,"max_line_len")
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

  
# -------------------------------------------------------------------
  physical_analysis_plots <- function() 
# -------------------------------------------------------------------
  {
    x_label <- "Type Of Text"
    legend_text <- "Language code"
    
    if (readIfEmpty(phys_df)) {
    } else {
      print("NO, I could NOT read it")
      phys_df <- physicalAnalysis(data_dir_corpus_subset)
      serializeIfNeeded(phys_df,FALSE)  
    }

    plot_phys_an_fsize <- basicPlot(phys_df
      ,"type","size",fillPar = "language"
      , title_par = "File Sizes, (from Linux ls command)"
      ,x_axis_lab_par = x_label
      ,y_axis_lab_par = "Size (rounded)"
      ,lengend_text)  
    # print(plot_phys_an_fsize);keypress()

    plot_phys_an_ntokens <- basicPlot(phys_df
      ,"type","n_token",fillPar = "language"
      , title_par = "Tokens \n(rough, from Linux wc command)"
      ,x_axis_lab_par = x_label
      ,y_axis_lab_par = "Count"
      ,legend_text)  
    #print(plot_phys_an_ntokens);keypress()

    plot_phys_an_nlines <- basicPlot(phys_df
      ,"type","n_newline",fillPar = "language"
      ,title_par = "Text Lines (from Linux wc)"
      ,x_axis_lab_par = x_label
      ,y_axis_lab_par = "Count"
      ,legend_text)  
    #print(plot_phys_an_nlines);keypress()

    plot_phys_an_max_line_len <- basicPlot(phys_df
      ,"type","max_line_len",fillPar = "language"
      ,title_par = "Max Line Lengths (from Linux wc command)"
      ,x_axis_lab_par = x_label
      ,y_axis_lab_par = "Length"
      ,legend_text)  
    #print(plot_phys_an_max_line_len);keypress()
    
    
    phys_an_plots <- list(      
       plot_phys_an_fsize 
      ,plot_phys_an_ntokens 
      ,plot_phys_an_nlines 
      ,plot_phys_an_max_line_len)
    serializeIfNeeded(phys_anal_plots,FALSE)
    
    phys_an_plots
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
  basicPlot <- function(dfPar,xPar,yPar,fillPar
   ,title_par, x_axis_lab_par, y_axis_lab_par,legend_title_par) 
# --------------------------------------------------------------------
{
  p <- ggplot(data=dfPar, aes_string(x = xPar, y = yPar)) 
  p <- p + geom_bar(stat="identity"
                      ,aes_string(fill = fillPar), position = "dodge")
  # p <- p + facet_grid(~language)
  # p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 1))
  
  # labels
  p <- p + ggtitle(title_par)
  p <- p + xlab(x_axis_lab_par)
  p <- p + ylab(y_axis_lab_par)
  p <- p + guides(fill=guide_legend(title=legend_title_par))

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
  freq_of_freq_cutted <- function(frq_d, nr_cuts, remove_outliers) 
#--------------------------------------------------------------------
  {

    if (missing(remove_outliers)) remove_outliers <- FALSE
    if (missing(nr_cuts)) nr_cuts <- 1000
    
    
    # move from frequencies to frequencies of frequencie
    if (remove_outliers) {
      qnt <- quantile(frq_d$frequency, probs=c(.25, .75), na.rm = FALSE)
      H <- 1.5 * IQR(qnt, na.rm = FALSE)
      l <- qnt[1] - H
      r <- qnt[2] + H
      frq_d <- frq_d[frq_d$frequency >= l & 
          frq_d$frequency <= r, ] 
    }
     
    frq_d$frequenze <- frq_d$frequency/sum(frq_d$frequency)
    max_freq <- max(frq_d$frequenze)
    avg_freq <- mean(frq_d$frequenze)
    sd_freq <- sd(frq_d$frequenze)
    
    left_lim <- 0 # min(frq_d$frequenze)
    right_lim <- max_freq
    cp <- seq(left_lim,right_lim
      ,by = (right_lim-left_lim)/nr_cuts)
    # don't work well, bugs and number too small
    # cut_labels  <- round(cp*100,2)
    # cut_labels <- cut_labels[3:length(cut_labels)]
    # cut_labels <- as.character(cut_labels)
    # cut_labels  <- c("~0+",as.character(cut_labels))
    # cut_labels <- paste(cut_labels,"%",sep = "")
    
    frq_d$freq_cut <- cut(frq_d$frequenze,breaks = cp
      #,labels = cut_labels
      )

    frq_d    
}
  
  
  



# --------------------------------------------------------------------
plot_freq_distrib_user_managed <- function(frq_d,faceted
  ,remove_outliars
  ,title_par, x_axis_lab_par, y_axis_lab_par
  ,legend_title_par
  ,no_ticks
  ,start_delta, end_delta
) 
# --------------------------------------------------------------------
{
  if(missing(remove_outliars)) remove_outliars <- TRUE
  if(missing(no_ticks)) no_ticks <- FALSE
  if(missing(start_delta)) start_delta <- 0
  if(missing(end_delta)) end_delta <- 0
  
    
  if (faceted)
    counted_df <- group_by(frq_d, group) %>% count(freq_cut)
  else 
    counted_df <- frq_d %>% count(freq_cut)

  if (remove_outliars) {
    qnt <- quantile(counted_df$n, probs=c(.25, .75), na.rm = FALSE)
    H <- 1.5 * IQR(qnt, na.rm = FALSE)
    l <- qnt[1] - H
    r <- qnt[2] + H
    counted_df <- counted_df[counted_df$n >= l & 
        counted_df$n <= r, ] 
    title_par <- paste(title_par,"(Outlayers Removed)")
  } else {
    title_par <- paste(title_par,"(Outlayers Included)")
  }

  if (faceted)
  p <- ggplot(counted_df[(1+start_delta):(nrow(counted_df)-end_delta),]
    ,aes(x = freq_cut, y = n,fill = group))   
  else
    p <- ggplot(counted_df[(1+start_delta):(nrow(counted_df)-end_delta),]
      ,aes(x = freq_cut, y = n))   
  p <- p + geom_bar(stat="identity")
  p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 0,
    vjust = 0))
  if (faceted) {
    p <- p + facet_grid(group ~ .)
  }
  
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
    # p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 1))
    
    p <- p + labs(x = NULL, y = "Frequency") 
    if (faceted) p <- p + facet_grid(group ~ .)
    p <- p + theme_minimal()
    
    p <- p + ggtitle(title_par)
    p <- p + xlab(x_axis_lab_par)
    p <- p + ylab(y_axis_lab_par)
    p <- p + guides(fill=guide_legend(title=legend_title_par))
    
    p
  }
  
# -------------------------------------------------------------------
freq_of_freq_an <- function()
# -------------------------------------------------------------------
{
  
  freq_d <- freq_distrib(dfm_full,"en",rem_stopw = F,proportions = F)
  freq_of_freq <- freq_of_freq_cutted(freq_d,250,F)

  
  freq_of_freq_an_plot_outlayers_excl <- plot_freq_distrib_user_managed(
    freq_of_freq
       ,faceted = T
       , remove_outliars = T
       ,"Frequency of Word Frequencies"
       ,"Frequency Ranges"
       ,"Frequency (of Frequency Range)"
       ,"Type of text"
       , no_ticks = T
       ,0,0
     )
   print(freq_of_freq_an_plot_outlayers_excl); keypress()
     
   freq_of_freq_an_plot_outlayers_incl <- plot_freq_distrib_user_managed(
    freq_of_freq
       ,faceted = T
       , remove_outliars = F
       ,"Frequency of Word Frequencies"
       ,"Frequency Ranges"
       ,"Frequency (of Frequency Range)"
       ,"Type of text"
       , no_ticks = T
       ,0,0
     )
   print(freq_of_freq_an_plot_outlayers_incl)

   
   freq_of_freq_an_plots <- list(
     freq_of_freq_an_plot_outlayers_excl
     ,freq_of_freq_an_plot_outlayers_incl
   )
  serializeIfNeeded(freq_of_freq_an_plots)
  freq_of_freq_an_plots
}


  
# ------------------------------------------------------------------------ 
  types_coverage <- function(qc, pct_to_cover, lng, remove_stopwords) 
# ------------------------------------------------------------------------ 
  {
    
    if (missing(remove_stopwords)) remove_stopwords <- FALSE
    if (missing(pct_to_cover)) pct_to_cover <- 0.95
    
    stopifnot( 0 < pct_to_cover && pct_to_cover < 1)
    
    # always remove punctuation
    tks  <- tokens(corpus_subset(qc,language == lng)
                   ,remove_punct = T)
    
    # normally should keep stopwords
    remove_it <- if(remove_stopwords) stopwords(lng) else NULL
    doc_fm <- dfm(tks, remove = remove_it)

    # get frequencies    
    frq <- textstat_frequency(doc_fm)
    # get proportions
    frq$props <- frq$frequency/sum(frq$frequency) ;
    stopifnot(round(sum(frq$props)-1,5) == 0)
    # cumulative
    frq$cumul <- cumsum(frq$props)
    idx <- which(frq$cumul >= pct_to_cover)[1]
    pct_types_for_coverage <- idx/nrow(frq)
    
    prt("nr. for coverage:",idx
        ,"pct features for 50% coverage:",pct_types_for_coverage
        ,"last feature:",frq$feature[idx])
    
    prt("vrification that we cover 50% summing simple probs",sum(frq$props[1:idx]))
    
    list(idx = idx
      ,pct_to_cover = pct_types_for_coverage
      ,proportion = frq$props
      ,freq_object = frq
      )
  }

  

  
  
# ====================================================================
#                     global initialization
# ====================================================================
  
  if (!readIfEmpty(qc_full)) {
    print(paste("reading corpus from dir:",read_dir))
    qc_full <- readQCorp(read_dir, FALSE)
  }
  serializeIfNeeded(qc_full, FALSE)

  if (!readIfEmpty(dfm_full)) {
    dfm_full <- dfm(qc_full, remove_punct = T)
    dfm_sel <- dfm_select(dfm_full,pattern = ".")
  }
  serializeIfNeeded(dfm_full, FALSE)
  
  
  
  
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
  test_physical_analysis_plots <- function() 
# -------------------------------------------------------------------
{

  ret <- physical_analysis_plots()
  grid.arrange(
      ret[[1]]  # plot_phys_an_fsize 
      ,ret[[2]] # plot_phys_an_ntokens 
      ,ret[[3]] # plot_phys_an_nlines 
      ,ret[[4]] # plot_phys_an_max_line_len
    )
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
  print(freq_d[1:5])
  freq_d
}


# --------------------------------------------------------------------
test_freq_of_freq_an <- function()
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


# ---------------------------------------------------------------------  
types_freq_an_wordcloud <- function(qc, lng, fct) 
# ---------------------------------------------------------------------  
{
  if((missing(fct))) fct <- FALSE 
  
  words_nr <-  50
  cols <- c('red', 'pink', 'green', 'purple', 'orange', 'blue')
  
  word_cloud_en <- textplot_wordcloud(dfm_subset(dfm_full
    ,language == lng)
    ,min_size = 2 ,max_size = 4, min_count = 10,
  max_words = words_nr, color = cols, font = NULL, adjust = 0,
  rotation = 0.1, random_order = FALSE, random_color = T,
  ordered_color = FALSE, labelcolor = "gray20", labelsize = 1.5,
  labeloffset = 0, fixed_aspect = TRUE,comparison = FALSE)
}

 
# ---------------------------------------------------------------------  
types_freq_an_wordclouds <- function(qc, fct) 
# ---------------------------------------------------------------------  
{
  if((missing(fct))) fct <- FALSE 

  types_freq_an_wordcloud(qc,"de",fct)
  keypress()

  types_freq_an_wordcloud(qc,"en",fct)
  keypress()

  types_freq_an_wordcloud(qc,"fi",fct)
  keypress()
  
  types_freq_an_wordcloud(qc,"ru",fct)
  keypress()
}

  
# ---------------------------------------------------------------------  
types_freq_an_q <- function(qc, fct) 
# ---------------------------------------------------------------------  
{
  if((missing(fct))) fct <- FALSE 

  # quanteda diagrams
  
  d <- types_distrib(qc,"de",1,rem_stopw = T, faceted = fct)
  types_freq_an_de_plot_q <- types_freq_plot_q(d, faceted = fct
                         ,"Most frequent words, ngrams"
                         ,"Words"
                         ,"Occurrences","")
  d <- types_distrib(qc,"de",3,rem_stopw = T, faceted = fct)
  types_freq_an_de_plot_3_q <- types_freq_plot_q(d, faceted = fct
                         ,"Most frequent words, ngrams"
                         ,"Words"
                         ,"Occurrences","")
  # print(types_freq_an_de_plot_3_q) ;keypress("fatto 3grams?")

  
  
   
  d <- types_distrib(qc,"en",1,rem_stopw = T, faceted = fct)
  types_freq_an_en_plot_q <- types_freq_plot_q(d, faceted = fct
                         ,"Most frequent words, ngrams"
                         ,"Words"
                         ,"Occurrences","")
  # print(types_freq_en_plot_q) ;keypress()
   d <- types_distrib(qc,"en",3,rem_stopw = T, faceted = fct)
  types_freq_an_en_plot_3_q <- types_freq_plot_q(d, faceted = fct
                         ,"Most frequent words, ngrams"
                         ,"Words"
                         ,"Occurrences","")
  #print(types_freq_an_en_plot_3_q) ;keypress("fatto 3grams?")

  
  
  d <- types_distrib(qc,"fi",1,rem_stopw = T, faceted = fct)
  types_freq_an_fi_plot_q <- types_freq_plot_q(d, faceted = fct
                         ,"Most frequent words, ngrams"
                         ,"Words"
                         ,"Occurrences","")
  # print(types_freq_en_plot_q) ;keypress()
   d <- types_distrib(qc,"fi",3,rem_stopw = T, faceted = fct)
  types_freq_an_fi_plot_3_q <- types_freq_plot_q(d, faceted = fct
                         ,"Most frequent words, ngrams"
                         ,"Words"
                         ,"Occurrences","")
  #print(types_freq_an_fi_plot_3_q) ;keypress("fatto 3grams?")

  
  
  d <- types_distrib(qc,"ru",1,rem_stopw = T, faceted = fct)
  types_freq_an_ru_plot_q <- types_freq_plot_q(d, faceted = fct
                         ,"Most frequent words, ngrams"
                         ,"Words"
                         ,"Occurrences","")
  # print(types_freq_en_plot_q) ;keypress()
   d <- types_distrib(qc,"ru",3,rem_stopw = T, faceted = fct)
  types_freq_an_ru_plot_3_q <- types_freq_plot_q(d, faceted = fct
                         ,"Most frequent words, ngrams"
                         ,"Words"
                         ,"Occurrences","")
  #print(types_freq_an_ru_plot_3_q) ;keypress("fatto 3grams?")



    grid.arrange(
      types_freq_an_en_plot_q
      ,types_freq_an_en_plot_3_q
      ,ncol=2
    )
    keypress()

    grid.arrange(
      types_freq_an_de_plot_q
      ,types_freq_an_fi_plot_q
      ,ncol=2
    )
    keypress()

  # list(types_freq_an_plot_q)
}

# ---------------------------------------------------------------------  
test_types_freq_an <- function(qc, fct) 
# ---------------------------------------------------------------------  
{
  if((missing(fct))) fct <- FALSE 
  
  types_freq_an_q(qc, fct)
  # types_freq_an_wordclouds(qc)
}  
  
  

# --------------------------------------------------------------------
test_types_coverage <- function(qc)
# --------------------------------------------------------------------

{
  ret <- types_coverage(qc_full, pct_to_cover = 0.9
    ,lng = "en", remove_stopwords = T) 

  
  mydf <- ret[["freq_object"]]
  print("")
  # ret$freq_object$props    $cuml
  p <- ggplot(data=mydf, aes(x=seq_along(props)/length(props), y = props))
  p <- p +geom_line()
  print(p)
  keypress()
  p <- ggplot(data=mydf, aes(x=seq_along(cumul)/length(cumul), y = cumul))
  p <- p +geom_line()
  print(p)
  
  print("")
  print("")
}   



# -------------------------------------------------------------------
  test_ev_nlp_eda_lib.R <- function()
# -------------------------------------------------------------------
{
 
  # read_dir = if (use_full_corpus) data_dir_corpus_full else data_dir_corpus_subset
  # readIfEmpty(qc_full) || readQCorp(read_dir, FALSE)
  
  # test_readIfEmpty_serializeIfNeeded(data_dir_corpus_subset)
  # keypress()
  
  # test_physical_analysis_plots()
  # keypress()
  
  # test_freq_distrib()
  # keypress()

  test_types_freq_an(qc_full)

    
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

  
  
    
  
    



