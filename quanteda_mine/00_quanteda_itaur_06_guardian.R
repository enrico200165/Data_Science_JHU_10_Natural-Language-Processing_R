require(quanteda, quietly = TRUE, warn.conflicts = FALSE)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#
# devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)
library(ggplot2)

# --- external files ---

source("01_globals.R")
prj_dir()



###########################################################
#   Guardian
###########################################################

# Techniques useful in analyzing news articles:
# - Constructing subject specific tokens object using kwic
# - Identifying and concatenating multi-word expressions using textstat_collocations()

# Characteristics of news articles/transcripts
# It is common to construct a corpus of news articles (or transcripts) from databases 
# with keyword queries, but such a corpus still contains a lot of noises, because 
# a news article is a set sentences and paragraphs on various subjects. 
# To focus our analysis on particular subjects, we can construct a tokens by kwic().
# 1 Construct a tokens as usual from a corpus
# 2 Run kwic() on the tokens to create a kwic
# 3 Convert kiwc into tokens using as.tokens
# 4 Apply whatever analysis we want on tokens

require(quanteda)


setwd(file.path(itaur_dir(),"6_advanced"))

load('guardianSample.RData')
toks <- tokens(guardianSample, remove_punct = TRUE)
toks <- tokens_remove(toks, stopwords('english'))

europe <- kwic(toks, c('Europe*', 'European Union', 'EU'))
print(class(europe));head(europe)
britain <- kwic(toks, c('Brit*', 'United Kingdom', 'UK'))
head(britain)

# original below does not work
# toks_europe <- as.tokens(europe)
# toks_britain <- as.tokens(britain)
toks_europe <- tokens(europe$post)
toks_britain <- tokens(britain$post)


# below my personal fix
# relative frequency analysis
dfm_europe <- dfm(toks_europe)
dfm_britain <- dfm(toks_britain) 
kwds <- textstat_keyness(rbind(dfm_europe, dfm_britain), target = seq_along(toks_europe))
print(kwds[1:3,])


# Targetted dictionary analysis
dict <- quanteda::dictionary(list(
  people = c('people', 'citizen*') 
 ,migrant = c('immigra*', 'migra*')
 ,economy = c('econom*', 'financ*', 'business*')
,crime = c('crim*', 'polic*', 'terror*'))
)

dfm_dict_europe <- dfm(toks_europe, dictionary = dict)
dfm_dict_britain <- dfm(toks_britain, dictionary = dict)

freq_dict_europe <- colSums(dfm_dict_europe) / sum(ntoken(toks_europe))
freq_dict_britain <- colSums(dfm_dict_britain) / sum(ntoken(toks_britain))


plot(freq_dict_europe, ylab = 'Frequency', xlab = 'Category', xaxt = 'n', 
     ylim = c(0, 0.01), xlim = c(0.5, 4.5), main = "Frequency of keywords")
axis(1, at = 1:4, labels = names(freq_dict_europe))
grid()
points(freq_dict_britain, col = 'red', pch = 2)
legend('topright', legend = c('Europe', 'Britain'), col = c('black', 'red'), pch = 1:2)


# Identifying and concatenating multi-word expressions
require(quanteda)
load('guardianSample.RData')
toks <- tokens(guardianSample, remove_punct = FALSE)
toks <- tokens_remove(toks, stopwords('english'), padding = TRUE)

# ev reduce it otherwise textstat_collocations never ends
tok2 <- toks[1:10]
toks <- as.tokens(lapply(tok2, function(t) { t[1:100]}))

col <- textstat_collocations(toks, method = 'lambda', max_size = 5,
                             features = "^[A-Z][A-Za-z0-9]+$", valuetype = "regex", case_insensitive = FALSE)
head(col)


toks_multi <- tokens_compound(toks, col)
head(toks[[1]], 50)
head(toks_multi[[1]], 50)


# fine
# ---------------------------------------------------------
prj_dir()


# ---------------------------------------------------------
prj_dir()
