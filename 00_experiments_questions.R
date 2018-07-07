require(dplyr)
require(ggplot2)
require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)
require(stringr)

source("01_preprocess_lib.R")


# --------------------------------------------------------------------
#                 minimal texts for experimentation
# --------------------------------------------------------------------
text1 <- "hello, how are you"
text2 <- "good thanks, and you?"
text3 <- "See you! Bye Bye."


(t1 <- paste("t1",paste0("word",1:3),collapse = " ",sep = ""))
(t2 <- paste("t2",paste0("word",1:3),collapse = " ",sep = ""))
(t3 <- paste("t3",paste0("word",1:3),collapse = " ",sep = ""))

(freq_text1 <- paste(c(rep("a ",1),rep("b",2),rep("c",3),rep("d",4)),collapse = " "))
(freq_text2 <- paste(c( rep("z ",100),rep("z ",100)
 ,rep("a ",11),rep("b",12),rep("c",13),rep("d",14)
 ,rep("a ",1),rep("b",2),rep("c",3),rep("d",4)
 )
 ,collapse = " "))

(t4letter <- paste(unlist(sapply(1:4,function(i) rep(letters[i],i))), collapse = " "))

# --------------------------------------------------------------------
  sample_corpus <- function()
# --------------------------------------------------------------------
{
  # sample_de_blog <-    "sample de blog text"
  # sample_de_news <-    "sample de news text"
  # sample_de_twitter <- "sample de twitter text"
  # #
  # sample_en_blog <-    "sample en blog text"
  # sample_en_news <-    "sample en news text"
  # sample_en_twitter <- "sample en twitter text"
  # #
  # sample_fi_blog    <- "sample fi blog text"
  # sample_fi_news    <- "sample fi news text"
  # sample_fi_twitter <- "sample fi twitter text"
  # #
  # sample_ru_blog    <- "sample ru blog text"
  # sample_ru_news    <- "sample ru news text"
  # sample_ru_twitter <- "sample ru twitter text"
  # 
  # sample_texts <- rbind(
  #   sample_de_blog, sample_de_news, sample_de_twitter
  #   ,sample_en_blog, sample_en_news, sample_en_twitter
  #   ,sample_fi_blog, sample_fi_news, sample_fi_twitter
  #   ,sample_ru_blog, sample_ru_news, sample_ru_twitter
  # )

  i <- 1
  sample_texts <- character(length(LANGUAGES)*length(TYPES))  
  for (l in LANGUAGES) {
    for (ty in TYPES) {
      dummy <- paste(unlist(sapply(1:4,function(i) rep(letters[i],i))), collapse = " ")
      sample_texts[[i]] <- paste("Sample",l,ty, dummy)
      i <-i+1
    }
  }

  sample_corpus <- corpus(sample_texts)
  docvars(sample_corpus,TXT_LNG) <- rep(tolower(c(LNG_DE,LNG_EN, LNG_FI, LNG_RU)),each = 3)
  docvars(sample_corpus,TXT_TYP) <- rep(c(TYPE_BLOG,TYPE_NEWS, TYPE_TW),4)
  # (sample_corpus$documents)
  sample_corpus
}


# ------------.-------- end texts ------------------------------------


 textstat_frequency_basic <- function() 
{
   sc <- sample_corpus()
   dfm_freq <- dfm(sc)

   w <- "sample"
   
   # no grouping
   txt_freq <- textstat_frequency(dfm_freq)
   print(txt_freq[txt_freq$feature == w])
   
   # group by language
   txt_freq_lng <- textstat_frequency(
     dfm_freq,groups = TXT_LNG)
   print(txt_freq_lng[txt_freq_lng$feature == w])
   
   # group by type
   txt_freq_typ <- textstat_frequency(
     dfm_freq,groups = TXT_TYP)
   print(txt_freq_typ[txt_freq_typ$feature == w])
}
# textstat_frequency_basic()


ggplot2_bin_pars <- function() {

  (vals <- unlist(sapply(seq(0,10,by = 2),function(x) rep(x,x))))
  x <- data.frame(a = vals)
  
  p <- ggplot(x, aes(a)) 
  
  (p0 <- p + geom_histogram())
  
  (p1 <- p + geom_histogram(binwidth = 0.1))
  (p2 <- p + geom_histogram(binwidth = 1))

  (p3 <- p + geom_histogram(bins = 4))
  (p4 <- p + geom_histogram(bins = 8))
}
# ggplot2_bin_pars()


# 

# --------------------------------------------------------------------
#   https://tutorials.quanteda.io/statistical-analysis/frequency/
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# https://rdrr.io/cran/quanteda/man/textstat_frequency.html



# --------------------------------------------------------------------
  stopwords_basics_explore <- function(){
# --------------------------------------------------------------------
    require(stopwords)
    (head(stopwords("en")))
    (str(stopwords("en")))
    (length(stopwords("en")))
    T
  }
#  stopwords_basics_explore()


# --------------------------------------------------------------------
  dictionary_basics_explore <- function()
# --------------------------------------------------------------------
{
    require(stopwords)
  
    txt <- "Hello honey How are you Hello darling"

    dict_simple <- dictionary(list(
      greet = "hello"
     ,stop_word = stopwords("en")))
    tl <- tokens_lookup(tokens(txt), dictionary = dict_simple
      ,exclusive = T, nomatch = "ALL_OTHERS")
    (d <- data.frame(categories = factor(tl[[1]])))
    p <- ggplot(d, aes(x=categories)) 
    p <- p + geom_bar()
  (p)
        
  T
}



#--------------------------------------------------------------------
  types_distrib <- function(qc_par, lng, ngram, faceted, rem_stopw,
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
    
    
    qc <- corpus_subset(qc_par, language == char_tolower(lng))
    stopifnot(ndoc(qc) == 3)
    
    toks <- tokens(qc   
                   ,remove_numbers = T,remove_punct = T
                   ,remove_symbols = T, remove_twitter = FALSE , remove_url = T)
    toks <- tokens_tolower(toks)
    if (rem_stopw) { # manage stopwords in/exclusion
      stopw <- stopwords(tolower(lng))
      toks <- tokens_remove(toks,stopw)
    }

    # toks <- tokens(toks, ngrams = ngram)

    dfm_lang <- dfm(toks,ngrams = ngram)

    ntypes <- if (faceted) 5 else 25
    grouping = if (faceted) TXT_TYP else NULL
    # group by text type
    frq_grp <- textstat_frequency(dfm_lang, n = ntypes
                                  , groups = grouping
                                  )

    p <- ggplot(data = frq_grp, aes(x = reorder(feature, frequency)
                    , y = frequency))
    p <- p + geom_point()
    p <- p + coord_flip() 
    p <- p + labs(x = NULL, y = "Frequency") 
    if (faceted) p <- p + facet_grid(group ~ .)
    p <- p + theme_minimal()
    print(p)
    
    p
  }
  


# read_dir = if (use_full_corpus) data_dir_corpus_full else data_dir_corpus_subset

  if (!readIfEmpty(qc_full)) {
    print(paste("reading corpus from dir:",read_dir))
    qc_full <- readQCorp(read_dir, FALSE)
    serializeIfNeeded(qc_full, FALSE)
  }

# qc_full <- qc_full <- readQCorp(read_dir, FALSE)
# p <- freq_distrib(d,"en",T)
p <- types_distrib(qc_full,"en",2,faceted = F, rem_stopw = T
                  ,"Distribution of word *Frequencies*"
                  ,"Word Frequencies Ranges","Frequency (of word frequencies ranges)"
                  ,"Text Type")





