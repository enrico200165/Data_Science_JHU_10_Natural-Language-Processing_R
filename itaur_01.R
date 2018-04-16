require(quanteda)

summary(data_corpus_inaugural)

summary(data_corpus_inaugural[1:5])

data_corpus_inaugural[1]
names(data_corpus_inaugural[1])
names(data_corpus_inaugural[1:1])
nchar(data_corpus_inaugural[1])
nchar(data_corpus_inaugural[1:1])

# nota la differenza
head(data_corpus_inaugural[1:1,1:3])
head(data_corpus_inaugural[1:2,1:3])

# questo output e' cambiato
summary(data_corpus_inaugural[1:5])
# summary(data_corpus_inaugural)[1:5]

data_corpus_inaugural[1]

cat(data_corpus_inaugural[2])

ndoc(data_corpus_inaugural)

docnames(data_corpus_inaugural)

nchar(data_corpus_inaugural[1:7])

ntoken(data_corpus_inaugural[1:7])

# utile per contare punti
ntoken(data_corpus_inaugural[1:7], remove_punct = TRUE)

ntype(data_corpus_inaugural[1:7])

tokens("Today is Thursday in Canberra. It is yesterday in London.")
t <- tokens("Today is Thursday in Canberra. It is yesterday in London.")
types(t)
## [1] "Today"     "is"        "Thursday"  "in"        "Canberra"  "."         "It"       
## [8] "yesterday" "London"   

vec <- c(one = "This is text one", 
         two = "This, however, is the second text")
tokens(vec)
## [1] "This" "is"   "text" "one" 
## 
## two :
## [1] "This"    ","       "however" ","       "is"      "the"     "second" 
## [8] "text"
types(tokens(vec))
## [1] "This"    "is"      "text"    "one"     ","       "however" "the"     "second" 

# get a cleaned and tokenized version of our text.
tokens(char_tolower(vec), remove_punct = TRUE)

# the way that char_tolower() is named reflects the logic of quanteda’s function grammar.
# The first part (before the underscore _) names the class of object that is input 
# to the function and is returned by the function. 

inaugTokens <- tokens(data_corpus_inaugural, remove_punct = TRUE)
tokens_tolower(inaugTokens[2])
str(inaugTokens)
## $ 1789-Washington: chr [1:1430] "Fellow-Citizens" "of" "the" "Senate" ...
## $ 1793-Washington: chr [1:135] "Fellow" "citizens" "I" "am" ...
## $ 1797-Adams     : chr [1:2318] "When" "it" "was" "first" ...

# Once each text has been split into words, we can use the dfm function to create a matrix 
# of counts of the occurrences of each word in each document:
inaugDfm <- dfm(inaugTokens)
str(inaugDfm)
# Formal class 'dfm' [package "quanteda"] with 15 slots
# ..@ settings    :List of 1
# .. ..$ : NULL
# ..@ weightTf    :List of 3
# .. ..$ scheme: chr "count"
# .. ..$ base  : NULL
# .. ..$ K     : NULL
# ..@ weightDf    :List of 5
# .. ..$ scheme   : chr "unary"
# .. ..$ base     : NULL
# .. ..$ c        : NULL
# .. ..$ smoothing: NULL
# .. ..$ threshold: NULL
# ..@ smooth      : num 0
# ..@ ngrams      : int 1
# ..@ skip        : int 0
# ..@ concatenator: chr "_"
# ..@ version     : int [1:3] 1 1 1
# ..@ docvars     :'data.frame':	58 obs. of  3 variables:
#   .. ..$ Year     : num [1:58] 1789 1793 1797 1801 1805 ...
# .. ..$ President: chr [1:58] "Washington" "Washington" "Adams" "Jefferson" ...
# .. ..$ FirstName: chr [1:58] "George" "George" "John" "Thomas" ...
# ..@ i           : int [1:44341] 0 2 3 5 6 7 8 13 14 15 ...
# ..@ p           : int [1:9342] 0 19 77 135 144 202 210 224 267 272 ...
# ..@ Dim         : int [1:2] 58 9341
# ..@ Dimnames    :List of 2
# .. ..$ docs    : chr [1:58] "1789-Washington" "1793-Washington" "1797-Adams" "1801-Jefferson" ...
# .. ..$ features: chr [1:9341] "fellow-citizens" "of" "the" "senate" ...
# ..@ x           : num [1:44341] 1 3 2 1 1 5 1 11 1 1 ...
# ..@ factors     : list()
trimmedInaugDfm <- dfm_trim(inaugDfm, min_doc = 5, min_count = 10)
print(trimmedInaugDfm)
# Document-feature matrix of: 58 documents, 1,502 features (68.6% sparse).

weightedTrimmedDfm <- dfm_tfidf(trimmedInaugDfm)
print(weightedTrimmedDfm)
# Document-feature matrix of: 58 documents, 1,502 features (68.6% sparse).

require(magrittr)
## Loading required package: magrittr
inaugDfm2 <- dfm(inaugTokens) %>% 
  dfm_trim(min_doc = 5, min_count = 10) %>% 
  dfm_tfidf()

# To see what objects for which any particular method (function) is defined, 
# you can use the methods() function:
methods(dfm)

# Likewise, you can also figure out what methods are defined for any given class 
# ä of object, using the same function:

methods(class = "tokens")

#If we are interested in analysing the texts with respect to some other variables, 
# we can create a corpus object to associate the texts with this metadata. 
# For example, consider the last six inaugural addresses:
summary(data_corpus_inaugural[52:57])

# We can use the docvars option to the corpus command to record the party with 
# which each text is associated:

dv <- data.frame(Party = c("dem", "dem", "rep", "rep", "dem", "dem"))
recentCorpus <- corpus(data_corpus_inaugural[52:57], docvars = dv)
summary(recentCorpus)

partyDfm <- dfm(recentCorpus, groups = "Party", remove = (stopwords("english")))
print(partyDfm)
# Document-feature matrix of: 2 documents, 2,204 features (37.7% sparse).

textplot_wordcloud(partyDfm, comparison = TRUE)

