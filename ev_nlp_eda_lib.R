require(dplyr)
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
  physicalAnalysis <- function()
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
  freq_distrib <- function(doc_fm, lang, faceted, rem_stopw,
                           title_par, x_axis_lab_par, y_axis_lab_par
                           ,legend_title_par) 
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
    
    if (rem_stopw) {
      stopw <- stopwords(tolower(lang))
      doc_fm <- dfm(doc_fm, remove = stopw)
      
    }
    
    dfm_lang <- dfm_subset(doc_fm, language == tolower(lang))
    stopifnot(ndoc(dfm_lang) == 3)
    
    frq_grp <- textstat_frequency(dfm_lang, groups = TXT_TYP)
    
    require(Hmisc)
    
    #molt = 100*1000
    #frequenza <- frequenza*molt
    frequenza <- frq_grp$frequency/sum(frq_grp$frequency)
    #frequenza <- frequenza/molt
    print(sum(frequenza))
    if (!round(sum(frequenza),10) == 1) {
      print("paste(sum(frequenza)",paste(sum(frequenza)))
      stopifnot(sum(round(frequenza),10) == 1)
    }
    # frequenza <- log10(frequenza)
    max_freq <- max(frequenza)
    avg_freq <- mean(frequenza)
    sd_freq <- sd(frequenza)
    left_lim <- 0
    right_lim <- avg_freq+0.2*sd_freq
    cp <- seq(left_lim,right_lim
              ,by = right_lim/99)
    frequenza <- cut2(frequenza,cp)
    
    d <- data.frame(freq = frequenza, gruppo = frq_grp$group)
    
    p <- ggplot(d, aes(x = freq, fill = gruppo))
    p <- p + geom_histogram(stat = "count")
    # p <- p + geom_line()
    p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 1))
    if (faceted) {
      p <- p + facet_grid(gruppo ~ .)
    }
    
    p <- p + ggtitle(title_par)
    p <- p + xlab(x_axis_lab_par)
    p <- p + ylab(y_axis_lab_par)
    p <- p + guides(fill=guide_legend(title=legend_title_par))
    
    p <- p + scale_x_discrete(labels = NULL)
    p <- p + scale_y_continuous(labels = NULL)
    p
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
    phys_df <- physicalAnalysis()
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
  test_readIfEmpty_serializeIfNeeded <- function()
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
  d <- dfm_full
  # p <- freq_distrib(d,"en",T)
  p <- freq_distrib(d,"en",T,T
                    ,"Distribution of word *Frequencies*"
                    ,"Word Frequencies Ranges","Frequency (of word frequencies ranges)"
                    ,"Text Type")
  print(p)
}


# -------------------------------------------------------------------
  test_ev_nlp_eda_lib.R <- function()
# -------------------------------------------------------------------
{
  
  test_freq_distrib()
  
  stop()
  
  test_readIfEmpty_serializeIfNeeded()
  
  test_physicalAnalysis()
  
  test_basicPlot(physicalAnalysis())
}

# 
  test_ev_nlp_eda_lib.R()

