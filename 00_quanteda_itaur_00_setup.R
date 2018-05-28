#install.packages("quanteda")
require(quanteda)
# help(package = "quanteda")

#install.packages("readtext")
require(readtext)

#install.packages("devtools")

#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)

#install.packages("spacyr")
require(spacyr)

library(ggplot2)

summary(data_corpus_irishbudget2010)

ieDfm <- dfm(data_corpus_irishbudget2010, remove = c(stopwords("english"), "will"), 
             stem = TRUE)
str(ieDfm)
isS4(ieDfm)

topfeatures(ieDfm)
isS4(topfeatures(ieDfm))
str(topfeatures(ieDfm))

textplot_wordcloud(ieDfm, min.freq=25, random.order=FALSE)
