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


(smry <-summary(data_corpus_inaugural))
sloop::otype(data_corpus_inaugural)
class(data_corpus_inaugural)
methods(class(data_corpus_inaugural)[1])

# Summaries
head(smry, n=2)
sloop::otype(smry)
str(smry)
class(smry)


# indexing corpus
sloop::otype(data_corpus_inaugural[1])
sloop::otype(data_corpus_inaugural[[1]])
class(data_corpus_inaugural[1])
class(data_corpus_inaugural[[1]])
# catches a named char
substr(data_corpus_inaugural[1],1,16)
substr(data_corpus_inaugural[58],1,16)
substr(data_corpus_inaugural[[1]],1,16)
str(data_corpus_inaugural[1])
str(data_corpus_inaugural[[1]])
names(data_corpus_inaugural[1])
names(data_corpus_inaugural[58])
names(data_corpus_inaugural[[1]])
# tries to get column
# names(data_corpus_inaugural[[58]])



# Corpus texts


ndoc(data_corpus_inaugural)


docnames(data_corpus_inaugural)[c(1,58)]
str(docnames(data_corpus_inaugural))


nchar(data_corpus_inaugural[1:2])
str(nchar(data_corpus_inaugural[1:2]))

ntoken(data_corpus_inaugural[1:3])
str(ntoken(data_corpus_inaugural[1:3]))

t <- tokens("Today is Thursday in Canberra. It is yesterday in London.")
# indexing corpus
sloop::otype(t)
class(t)
str(t)
names(t)


vec <- c(one = "This is text one", 
         two = "This, however, is the second text")
tokens(vec)
tokens(char_tolower(vec), remove_punct = TRUE)

inaugTokens <- tokens(data_corpus_inaugural, remove_punct = TRUE)
tokens_tolower(inaugTokens[2])


# Once each text has been split into words, we can use the dfm 
# function to create a matrix of counts of the occurrences of 
# each word in each document
inaugDfm <- dfm(inaugTokens)
sloop::otype(inaugDfm)
class(inaugDfm)
str(inaugDfm)
# error methods(inaugDfm)



(trimmedInaugDfm <- dfm_trim(inaugDfm, min_doc = 5, min_termfreq = 10))
require(magrittr)
inaugDfm2 <- dfm(inaugTokens) %>% 
  dfm_trim(min_doc = 5, min_termfreq = 10) %>% 
  dfm_tfidf()


# add docvars to the corpus command
dv <- data.frame(Party = c("dem", "dem", "rep", "rep", "dem", "dem"))
recentCorpus <- corpus(data_corpus_inaugural[52:57], docvars = dv)
summary(recentCorpus)

partyDfm <- dfm(recentCorpus, groups = "Party", remove = (stopwords("english")))
textplot_wordcloud(partyDfm, comparison = TRUE)


# --------   Ev Added ----------

# Documents
docs <- data_corpus_inaugural$documents
sloop::otype(docs)
class(docs)
str(docs)

# Corpus metadata
str(data_corpus_inaugural$metadata)

# Corpus settings
str(data_corpus_inaugural$settings)

# Corpus tokens
str(data_corpus_inaugural$tokens)
NULL

cr <- corpus(c("ciao bambina", "salutam a soreta"))
dv <- data.frame(salut = c("saluto1", "saluto2"))
docvars(cr) <- dv
cr$documents