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

summary(data_corpus_inaugural)

# does not work as expected
summary(data_corpus_inaugural[1:5])

data_corpus_inaugural[1]

data_corpus_inaugural[2]

ndoc(data_corpus_inaugural)

docnames(data_corpus_inaugural)

nchar(data_corpus_inaugural[1:7])
str(nchar(data_corpus_inaugural[1:7]))

ntoken(data_corpus_inaugural[1:7])

tokens("Today is Thursday in Canberra. It is yesterday in London.")

vec <- c(one = "This is text one", 
         two = "This, however, is the second text")
tokens(vec)

tokens(char_tolower(vec), remove_punct = TRUE)

inaugTokens <- tokens(data_corpus_inaugural, remove_punct = TRUE)
tokens_tolower(inaugTokens[2])

inaugDfm <- dfm(inaugTokens)
trimmedInaugDfm <- dfm_trim(inaugDfm, min_doc = 5, min_count = 10)

require(magrittr)
inaugDfm2 <- dfm(inaugTokens) %>% 
  dfm_trim(min_doc = 5, min_count = 10) %>% 
  dfm_tfidf()

methods(dfm)

methods(class = "tokens")

summary(data_corpus_inaugural[52:57])

dv <- data.frame(Party = c("dem", "dem", "rep", "rep", "dem", "dem"))
recentCorpus <- corpus(data_corpus_inaugural[52:57], docvars = dv)
summary(recentCorpus)

partyDfm <- dfm(recentCorpus, groups = "Party", remove = (stopwords("english")))
textplot_wordcloud(partyDfm, comparison = TRUE)

