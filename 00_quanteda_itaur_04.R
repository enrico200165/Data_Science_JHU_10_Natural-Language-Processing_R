require(quanteda, quietly = TRUE, warn.conflicts = FALSE)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)
library(ggplot2)

txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!",
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
tokens(txt)
# for each text a list of tokens (and many attributes)
str(tokens(txt))


# for really fast and simple tokenization are "fastestword" and "fasterword", 
# if performance is a key issue. These are less intelligent than the boundary detection used in 
# the default "word" method, which is based on stringi/ICU boundary detection.

# quanteda functions *_tolower include options designed to preserve acronyms.
# based on stringi::stri_trans_tolower(), and is therefore nicely Unicode compliant.
test1 <- c(text1 = "England and France are members of NATO and UNESCO",
           text2 = "NASA sent a rocket into space.")
char_tolower
char_tolower(test1, keep_acronyms = TRUE)

test2 <- tokens(test1, remove_punct = TRUE)
tokens_tolower(test2)

# here we apply to tokens
tokens_tolower(test2, keep_acronyms = TRUE)


data(data_char_encodedtexts, package = "readtext")
print(str(data_char_encodedtexts))
print(names(data_char_encodedtexts))

# Russian
cat(iconv(data_char_encodedtexts[8], "windows-1251", "UTF-8"))
## "8-битные" oncodings являются частью прошлого. 0€. Дефис-ели. Тильда ~ тире - и ± § 50.
cat(tolower(iconv(data_char_encodedtexts[8], "windows-1251", "UTF-8")))
## "8-битные" oncodings являются частью прошлого. 0€. дефис-ели. тильда ~ тире - и ± § 50.
head(char_tolower(stopwords("russian")), 20)
##  [1] "и"   "в"   "во"  "не"  "что" "он"  "на"  "я"   "с"   "со"  "как"
## [12] "а"   "то"  "все" "она" "так" "его" "но"  "да"  "ты"
head(char_toupper(stopwords("russian")), 20)
##  [1] "И"   "В"   "ВО"  "НЕ"  "ЧТО" "ОН"  "НА"  "Я"   "С"   "СО"  "КАК"
## [12] "А"   "ТО"  "ВСЕ" "ОНА" "ТАК" "ЕГО" "НО"  "ДА"  "ТЫ"

# Arabic
cat(iconv(data_char_encodedtexts[6], "ISO-8859-6", "UTF-8"))
## ترميزات 8 بت هي موديل قديم. 0 . الواصلة أكل. تيلدا ~ م اندفاعة - و  50 .
cat(tolower(iconv(data_char_encodedtexts[6], "ISO-8859-6", "UTF-8")))
## ترميزات 8 بت هي موديل قديم. 0 . الواصلة أكل. تيلدا ~ م اندفاعة - و  50 .
head(char_toupper(stopwords("arabic")), 20)
## Warning: 'stopwords(language = "ar")' is deprecated.
## Use 'stopwords(language = "ar", source = "misc")' instead.
## See help("Deprecated")
##  [1] "فى"    "في"    "كل"    "لم"    "لن"    "له"    "من"    "هو"   
##  [9] "هي"    "قوة"   "كما"   "لها"   "منذ"   "وقد"   "ولا"   "نفسه" 
## [17] "لقاء"  "مقابل" "هناك"  "وقال"


# dfm, the Swiss army knife, converts to lower case by default, 
# but this can be turned off using the tolower = FALSE argument.

# ---------------------------------------------------------
# 3. Removing and selecting features
# ---------------------------------------------------------

# when creating a dfm:
# with English stopwords and stemming
dfmsInaug2 <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980),
  remove = stopwords("english"), stem = TRUE)
print(dfmsInaug2[1:3,1:5])

# after creating a dfm:
myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.",
               "Does the United_States or Sweden have more progressive taxation?"),
             tolower = FALSE, verbose = TRUE)

# selects features (types?) whose name ends in s or has .y
dfm_select(myDfm, pattern = c("s$", ".y"), selection = "keep", valuetype = "regex")
dfm_select(myDfm, pattern = c("s$"), selection = "keep", valuetype = "regex")
# now selection = "remove"
dfm_select(myDfm, pattern = c("s$", ".y"), selection = "remove", valuetype = "regex")
# keeps the stopwords?
dfm_select(myDfm, pattern = stopwords("english"), selection = "keep", valuetype = "fixed")

dfm_select(myDfm, pattern = stopwords("english"), selection = "remove", valuetype = "fixed")

testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with
             the newspaper from a boy named Seamus, in his mouth."
testCorpus <- corpus(testText)

featnames(dfm(testCorpus, remove = stopwords("english")))

featnames(dfm(testCorpus, ngrams = 1:2, remove = stopwords("english")))



## removing stopwords before constructing ngrams
tokensAll <- tokens(tolower(testText), remove_punct = TRUE)
tokensNoStopwords <- tokens_remove(tokensAll, stopwords("english"))
tokensNgramsNoStopwords <- tokens_ngrams(tokensNoStopwords, 2)
featnames(dfm(tokensNgramsNoStopwords, verbose = FALSE))

# keep only certain words
dfm(testCorpus, select = "*s", verbose = FALSE)  # keep only words ending in "s"

dfm(testCorpus, select = "s$", valuetype = "regex", verbose = FALSE)

testTweets <- c("My homie @justinbieber #justinbieber shopping in #LA yesterday #beliebers",
                "2all the ha8ers including my bro #justinbieber #emabiggestfansjustinbieber",
                "Justin Bieber #justinbieber #belieber #fetusjustin #EMABiggestFansJustinBieber")

# keep only hashtags
dfm(testTweets, select = "#*", remove_twitter = FALSE)
# ^ $ meaning seems to be relative to features rather than to phrases
dfm(testTweets, select = "^#.*$", valuetype = "regex", remove_twitter = FALSE)





d <- dfm(testCorpus, remove = stopwords("english"))

print(d[1,1:5])

# ability to create a new dfm with the same feature set as the old. 
# This is very useful, for instance, if we train a model on one dfm, and 
# need to predict on counts from another, but need the feature set to be equivalent.

# selecting on a dfm
# textVec1 <- c("This is text one.", "This, the second text.", "Here: the third text.")
# textVec2 <- c("Here are new words.", "New words in this text.")
# EV my semplification, note that the first provides the features
# the second the frequencies
textVec1 <- c("z aaa bbb ddd")
textVec2 <- c("ccc z z z")
featnames(dfm1 <- dfm(textVec1))
featnames(dfm2 <- dfm(textVec2))
dfm_select(dfm2, dfm1)

# textVec1 <- paste0(letters,collapse = " ")
textVec1 <- "a b c d e f g h i"
textVec2 <- "a a a a e e"
(dfm1 <- dfm(textVec1))
(dfm2 <- dfm(textVec2))
(dfm3 <- dfm_select(dfm2, dfm1))


# ---------------------------------------------------------
# 4. Applying equivalency classes: dictionaries, thesaruses
# ---------------------------------------------------------

# import the Laver-Garry dictionary from http://bit.ly/1FH2nvf
# what does a dictionary do? used for lookup

myDict <- dictionary(list(frutta = c("mela", "pera", "arancio"),
                          insalata = c("verze", "lattuga")))
myDfm <- dfm(c("mela pera arancio verze lattuga pizza riso torta"))
print(myDfm)
dfm_lookup(myDfm, myDict, valuetype = "glob")
             
setwd("C:\\Users\\e_viali\\Documents\\dev\\ITAUR\\4_preparing_texts")
myDict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
                          opposition = c("Opposition", "reject", "notincorpus"),
                          taxglob = "tax*",
                          taxregex = "tax.+$",
                          country = c("United_States", "Sweden")))
myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.",
               "Does the United_States or Sweden have more progressive taxation?"),
             remove = stopwords("english"), verbose = FALSE)

dfm_lookup(myDfm, myDict, valuetype = "glob")
dfm_lookup(myDfm, myDict, valuetype = "glob", case_insensitive = FALSE)
dfm_lookup(myDfm, myDict, valuetype = "regex", case_insensitive = TRUE)

lgdict <- dictionary(file = "LaverGarry.cat")
budgdfm <- dfm(data_corpus_irishbudget2010, dictionary = lgdict, verbose = TRUE)
dfm_lookup(myDfm, myDict, valuetype = "glob")
dfm_lookup(myDfm, myDict, valuetype = "fixed")
dfm_lookup(myDfm, myDict, valuetype = "fixed", case_insensitive = FALSE)


# possible to pass through a dictionary at the time of dfm() creation.
# dfm with dictionaries
mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1900)
mydict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
                          opposition = c("Opposition", "reject", "notincorpus"),
                          taxing = "taxing",
                          taxation = "taxation",
                          taxregex = "tax*",
                          country = "united states"))

# limita le features alle entries del dizionario
dictDfmDic <- dfm(mycorpus, dictionary = mydict)
# nota
dictDfm <- dfm(mycorpus)
print(paste(nfeat(dictDfmDic),"vs", nfeat(dictDfm)))



# “thesaurus” feature, which collapses words in a dictionary but is not exclusive.
# BUT IS NOT EXCLUSIVE
mytexts <- c("British English tokenises differently, with more colour.",
             "American English tokenizes color as one word.")
mydict <- dictionary(list(color = "colo*r", tokenize = "tokeni?e*"))
dfm(mytexts, thesaurus = mydict)
dfm(mytexts, dictionary  = mydict)

# ---------------------------------------------------------
# 5 Stemming
# ---------------------------------------------------------
# relies on the SnowballC package’s implementation of the Porter stemmer
SnowballC::getStemLanguages()
quanteda_options("language_stemmer")

# note that you have
# tokens_wordstem(x, language = quanteda_options("language_stemmer"))
# char_wordstem(x, language = quanteda_options("language_stemmer"))
# dfm_wordstem(x, language = quanteda_options("language_stemmer"))

# It’s not perfect:
char_wordstem(c("win", "winning", "wins", "won", "winner"))

it_idx <- which(SnowballC::getStemLanguages() == "italian")
char_wordstem(c("andare", "mangiare","leggere","brutto")
              , language = SnowballC::getStemLanguages()[it_idx])

# stemmed objects must be tokenized, but can be of many different quanteda classes:
txt <- "From 10k packages, quanteda is an text analysis package, for analysing texts."
char_wordstem(txt)
## Error in char_wordstem.character(txt): whitespace detected: you can only stem tokenized texts
(toks <- tokens_wordstem(tokens(txt, remove_punct = TRUE)))

# Some text operations can be conducted directly on the dfm:
dfm1 <- dfm(data_corpus_inaugural[1:2], verbose = FALSE)
head(featnames(dfm1))
head(featnames(dfm_wordstem(dfm1)))
# same as 
head(dfm(data_corpus_inaugural[1:2], stem = TRUE, verbose = FALSE))


# ---------------------------------------------------------
# dfm() and its many options
# ---------------------------------------------------------
# Operates on character (vectors), corpus, or tokens objects,
# dfm(x, tolower = TRUE, stem = FALSE, select = NULL, remove = NULL,
#     thesaurus = NULL, dictionary = NULL, 
#     valuetype = c("glob", "regex", "fixed"), 
#     groups = NULL, 
#     verbose = quanteda_options("verbose"), ...)
