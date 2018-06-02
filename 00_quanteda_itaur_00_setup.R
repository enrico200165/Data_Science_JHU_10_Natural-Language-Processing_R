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
sloop::otype(data_corpus_irishbudget2010)
isS4(data_corpus_irishbudget2010)
# will show if it is an S6 class
class(data_corpus_irishbudget2010)
methods(class(data_corpus_irishbudget2010)[1])

ieDfm <- dfm(data_corpus_irishbudget2010
 ,remove = c(stopwords("english"), "will")
 ,stem = TRUE)
isS4(ieDfm)
# rubbish method indexing does not work as expected
sloop::s4_methods_class("dfm")
# [[]] allows to get a vector and work normally
sloop::s4_methods_class("dfm")[[1]][100:150]
class(ieDfm)
str(ieDfm)


topfeatures(dfm)
topfeatures(ieDfm)
isS4(topfeatures(ieDfm))
str(topfeatures(ieDfm))

textplot_wordcloud(ieDfm, min.freq=25, random.order=FALSE)
