require(quanteda, quietly = TRUE, warn.conflicts = FALSE)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#
devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)
library(ggplot2)

###########################################################
#             Descriptive Statistics
###########################################################
txt <- c(sent1 = "This is an example of the summary method for character objects.",
         sent2 = "The cat in the hat swung the bat.")
summary(txt)

# also works for corpus objects:
summary(corpus(data_char_ukimmig2010
#  ,notes = "Created as a demo.")
))

# to access the syllables of a text, we use syllables():
nsyllable(c("Superman.", "supercalifragilisticexpialidocious", "The cat in the hat."))

# can even compute the Scabble value of English words, using scrabble():
nscrabble(c("cat", "quixotry", "zoo"))

# analyze the lexical diversity of texts, using lexdiv() on a dfm:
myDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980))
lexstat <- textstat_lexdiv(myDfm, "R")
lexstat <- lexstat[order(lexstat[["R"]]), ]
with(lexstat, dotchart(R, labels = document))

# readability of texts, using readability() on a vector of texts or a corpus:
readab <- textstat_readability(corpus_subset(data_corpus_inaugural, Year > 1980), 
                               measure = "Flesch.Kincaid")
readab <- readab[order(readab[["Flesch.Kincaid"]]), ]
with(readab, dotchart(Flesch.Kincaid, labels = document))

# identify documents and terms that are similar to one another, using similarity()
presDfm <- corpus_subset(data_corpus_inaugural, Year > 1980) %>%
  dfm(remove = stopwords("english"), remove_punct = TRUE)
simili <- textstat_simil(presDfm, "1985-Reagan")

textstat_simil(presDfm, c("2009-Obama", "2013-Obama"), method = "cosine")
textstat_dist(presDfm, c("2009-Obama", "2013-Obama"), method = "canberra")
textstat_dist(presDfm, c("2009-Obama", "2013-Obama"), method = "ejaccard")

# compute some term similarities
lapply(as.list(textstat_simil(presDfm, c("senator", "chief", "terror")
    , margin = "features", method = "cosine")), 
   head, n = 10)

# this can be used for clustering documents
data(data_corpus_sotu, package = "quanteda.corpora")
presDfm <- dfm(corpus_subset(data_corpus_sotu, Date > "1990-01-01"), stem = TRUE,
               remove = c(stopwords("english"), "applause"), remove_punct = TRUE)
presDfm <- dfm_trim(presDfm, min_termfreq = 5, min_docfreq = 3)
# hierarchical clustering - get distances on normalized dfm
# presDistMat <- dist(as.matrix(dfm_weight(presDfm, "relFreq")))
# dfm_weight(x, scheme = "prop")
presDistMat <- dist(as.matrix(dfm_weight(presDfm, scheme = "prop")))
presDfm[1:5,1:5]
str(presDistMat)
## Warning: scheme = "relfreq" is deprecated; use dfm_weight(x, scheme =
## "prop") instead
# hiarchical clustering the distance object
presCluster <- hclust(presDistMat)
# label with document names
presCluster$labels <- docnames(presDfm)
# plot as a dendrogram
plot(presCluster)

# term clustering instead:

# word dendrogram with tf-idf weighting
wordDfm <- dfm_sort(dfm_tfidf(presDfm))
wordDfm <- t(wordDfm)[1:100,]  # because transposed
wordDistMat <- dist(wordDfm)
wordCluster <- hclust(wordDistMat)
plot(wordCluster, xlab="", main="tf-idf Frequency weighting")

# helper functions to extract information from quanteda objects:
myCorpus <- corpus_subset(data_corpus_inaugural, Year > 1980)
ndoc(myCorpus) # number of documents
ndoc(dfm(myCorpus))
ntoken(myCorpus) # how many tokens (total words)
str(ntoken(myCorpus))

print(ntoken("How many words in this sentence?"))

# arguments to tokenize can be passed 
ntoken("How many words in this sentence?", remove_punct = TRUE)

# how many types (unique words)
ntype(myCorpus)

ntype("Yada yada yada.  (TADA.)")
ntype("Yada yada yada.  (TADA.)", remove_punct = TRUE)
ntype(char_tolower("Yada yada yada.  (TADA.)"), remove_punct = TRUE)

# can count documents and features
ndoc(data_corpus_inaugural)

myDfm1 <- dfm(data_corpus_inaugural)
ndoc(myDfm1)

nfeat(myDfm1)

myDfm2 <- dfm(data_corpus_inaugural, remove = stopwords("english"), stem = TRUE)
nfeat(myDfm2)

myDfm3 <- dfm(data_corpus_inaugural, remove = stopwords("english"), remove_punct = TRUE, stem = TRUE)
nfeat(myDfm3)

# can extract feature labels and document names
head(featnames(myDfm1), 20)

head(docnames(myDfm1))

# and topfeatures
topfeatures(myDfm1)
topfeatures(myDfm2) # without stopwords

topfeatures(myDfm3) # without stopwords or punctuation

# End
