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


# 
sc <- sample_corpus()
# sc <- corpus_subset(data_corpus_inaugural, President == "Trump")
# sc <- dfm_full


#--------------------------------------------------------------------
  freq_distrib <- function(doc_fm, lang, faceted) 
#--------------------------------------------------------------------
{
  # https://tutorials.quanteda.io/statistical-analysis/frequency/
    # the dfm() function applies certain options by default, 
    # such as tolower() – a separate function for lower-casing texts 
    # – and removes punctuation. All of the options to tokens() can 
    # be passed to dfm(), however.
        
  if (missing(faceted))
    faceted <- FALSE
  
  stopw <- stopwords(tolower(lang))
  
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
  p <- p + ggtitle("Distribution of word *Frequencies*")
  p <- p + geom_histogram(stat = "count")
  # p <- p + geom_line()
  p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 1))
  if (faceted) {
    p <- p + facet_grid(gruppo ~ .)
  }
  p <- p + xlab("Word Frequencies Ranges")
  p <- p + scale_x_discrete(labels = NULL)
  p <- p + scale_y_continuous(labels = NULL)
  p <- p + ylab("Frequency (of word frequencies ranges)")
  p <- p + guides(fill=guide_legend(title="Text Type"))
  p
}

 # d <- dfm(sc)
# 
d <- dfm_full
# p <- freq_distrib(d,"en",T)
p <- freq_distrib(d,"en",F)
print(p)




# plot 20 most frequent words
# library("ggplot2")
# ggplot(freq[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
#   geom_point() + 
#   coord_flip() +
#   labs(x = NULL, y = "Frequency")
# 
# # plot relative frequencies by group
# dfm_weight_pres <- data_corpus_inaugural %>% 
#   corpus_subset(Year > 2000) %>% 
#   dfm(remove = stopwords("english"), remove_punct = TRUE) %>% 
#   dfm_weight(scheme = "prop")
# 
# 
# 
# 
# 
# qc <- sample_corpus()
# #(freq_text1);sc <- c(freq_text1,freq_text1,freq_text1)
# dfm_tmp <- dfm(qc)
# dfm_freq <- dfm_weight(dfm_tmp,scheme = "prop")
# txt_freq_typ <- textstat_frequency(
#   dfm_freq
#   #,groups = TXT_TYP
# )
# txt_freq_typ$frequency <- txt_freq_typ$frequency/ndoc(dfm_freq)
# sum(txt_freq_typ$frequency)
# 
# # histogram(txt_freq_typ$frequency/length(txt_freq_typ$frequency),breaks = cutpoints)
# 
# 
# 
# w <- "sample"
# 
# # no grouping
# txt_freq <- textstat_frequency(dfm_freq)
# txt_freq[txt_freq$feature == w]
# 
# # group by language
# txt_freq_lng <- textstat_frequency(
#   dfm_freq,groups = TXT_LNG)
# txt_freq_lng[txt_freq_lng$feature == w]
# 
# # group by type
# txt_freq_typ <- textstat_frequency(
#   dfm_freq,groups = TXT_TYP)
# txt_freq_typ[txt_freq_typ$feature == w]
# 
# 
# 
# (dfm_freq <- dfm(freq_text1))
# (dfm_weight(dfm_freq,scheme = "prop"))
# 
# p <- ggplot(data = txt_freq, aes(x = feature
#                                  ,y = frequency/sum(frequency)))
# p <-p + geom_bar(stat = "identity")
# print(p)
# 
