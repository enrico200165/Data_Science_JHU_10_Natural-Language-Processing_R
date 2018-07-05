require(dplyr)
require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)


source("01_globals.R")


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
  docvars(sample_corpus,TXT_LNG) <- rep(c(LNG_DE,LNG_EN, LNG_FI, LNG_RU),each = 3)
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


sc <- sample_corpus()
#(freq_text1);sc <- c(freq_text1,freq_text1,freq_text1)
(dfm_freq <- dfm(sc))
(dfm_freq <- dfm_weight(dfm_freq,scheme = "prop"))
txt_freq_typ <- textstat_frequency(
  dfm_freq
  #,groups = TXT_TYP
  )
txt_freq_typ$frequency <- txt_freq_typ$frequency/ndoc(dfm_freq)
sum(txt_freq_typ$frequency)

require(Hmisc)
nbins <- 10 
cutpoints <- seq(0,1,length.out = nbins+1)
str(cut2(txt_freq_typ$frequency,g = 20))


w <- "sample"

# no grouping
txt_freq <- textstat_frequency(dfm_freq)
txt_freq[txt_freq$feature == w]

# group by language
txt_freq_lng <- textstat_frequency(
  dfm_freq,groups = TXT_LNG)
txt_freq_lng[txt_freq_lng$feature == w]

# group by type
txt_freq_typ <- textstat_frequency(
  dfm_freq,groups = TXT_TYP)
txt_freq_typ[txt_freq_typ$feature == w]



(dfm_freq <- dfm(freq_text1))
(dfm_weight(dfm_freq,scheme = "prop"))

p <- ggplot(data = txt_freq, aes(x = feature
 ,y = frequency/sum(frequency)))
p <-p + geom_bar(stat = "identity")
print(p)


# --------------------------------------------------------------------
#   https://tutorials.quanteda.io/statistical-analysis/frequency/
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# https://rdrr.io/cran/quanteda/man/textstat_frequency.html
dfm1 <- dfm(c("a a b b c d", "a d d d", "a a a"))
textstat_frequency(dfm1)
textstat_frequency(dfm1, groups = c("one", "two", "one"))

obamadfm <- 
  corpus_subset(data_corpus_inaugural, President == "Obama") %>%
  dfm(remove_punct = TRUE, remove = stopwords("english"))
freq <- textstat_frequency(obamadfm)
head(freq, 10)


# plot 20 most frequent words
library("ggplot2")
ggplot(freq[1:20, ], aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() + 
  coord_flip() +
  labs(x = NULL, y = "Frequency")

# plot relative frequencies by group
dfm_weight_pres <- data_corpus_inaugural %>% 
  corpus_subset(Year > 2000) %>% 
  dfm(remove = stopwords("english"), remove_punct = TRUE) %>% 
  dfm_weight(scheme = "prop")

# calculate relative frequency by president
freq_weight <- textstat_frequency(dfm_weight_pres, n = 10,
                                  groups = "President")

# plot frequencies
ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")
