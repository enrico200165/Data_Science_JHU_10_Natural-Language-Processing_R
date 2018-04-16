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

# quanteda can construct a corpus object from several input sources:
  
# from a character vector object
str(data_char_ukimmig2010)
# Named chr [1:9] "IMMIGRATION: AN UNPARALLELED CRISIS WHICH ONLY THE BNP CAN SOLVE. \n\n- At current immigration and birth rates,"| __truncated__ ...
# - attr(*, "names")= chr [1:9] "BNP" "Coalition" "Conservative" "Greens" ...
myCorpus <- corpus(data_char_ukimmig2010
                   #, notes = "My first corpus"
                   )
# a VCorpus object from the tm package
data(crude, package = "tm")
myTmCorpus <- corpus(crude)
summary(myTmCorpus, 5)

# Texts read by the readtext package

# readtext accepts filemasks, so that you can specify a pattern 
# to load multiple texts,  and these texts can even be of multiple types. 
# readtext is smart enough to process them correctly, returning 
# a data.frame with a primary field "text" containing a character vector of the texts, 
# and additional columns of the data.frame as found in the document variables 
# from the source files.

# As encoding can also be a challenging issue for those reading in texts, 
# we include functions for diagnosing encodings on a file-by-file basis, and 
# allow you to specify vectorized input encodings to read in file types with 
# individually set (and different) encodings. 
# (All ecnoding functions are handled by the stringi package.)

require(readtext)
setwd("C:\\Users\\enrico\\GDRIVE\\CAPSTONE\\Quanteda\\ITAUR\\3_file_import")
myCorpus <- corpus(readtext("inaugural/*.txt"))
myCorpus <- corpus(readtext("sotu/*.txt"))

mytf <- readtext("inaugural/*.txt", docvarsfrom = "filenames", dvsep = "-", 
                 docvarnames = c("Year", "President"))
mytf
# readtext object consisting of 57 documents and 2 docvars.
# # data.frame [57 x 4]
# doc_id              text                 Year President 
# <chr>               <chr>               <int> <chr>     
#   1 1789-Washington.txt "\"Fellow-Cit\"..."  1789 Washington
# 2 1793-Washington.txt "\"Fellow cit\"..."  1793 Washington
# 3 1797-Adams.txt      "\"When it wa\"..."  1797 Adams     
# 4 1801-Jefferson.txt  "\"Friends an\"..."  1801 Jefferson 
# 5 1805-Jefferson.txt  "\"Proceeding\"..."  1805 Jefferson 
# 6 1809-Madison.txt    "\"Unwilling \"..."  1809 Madison   
# # ... with 51 more rows
data_corpus_inaugural <- corpus(mytf)
summary(data_corpus_inaugural, 5)

#  add document variables to the corpus, as long as the data 
# frame containing them is of the same length as the texts:
SOTUdocvars <- read.csv("SOTU_metadata.csv", stringsAsFactors = FALSE)
SOTUdocvars$Date <- as.Date(SOTUdocvars$Date, "%B %d, %Y")
SOTUdocvars$delivery <- as.factor(SOTUdocvars$delivery)
SOTUdocvars$type <- as.factor(SOTUdocvars$type)
SOTUdocvars$party <- as.factor(SOTUdocvars$party)
SOTUdocvars$nwords <- NULL

sotuCorpus <- corpus(readtext("sotu/*.txt", encoding = "UTF-8-BOM"))
head(docvars(sotuCorpus),3)
str(head(docvars(sotuCorpus),3))
row.names(head(docvars(sotuCorpus),3))
(docvars(sotuCorpus))[1:3, ]

docvars(sotuCorpus) <- SOTUdocvars
(docvars(sotuCorpus))[1:3, ]

#  texts are stored alongside the document variables in a structured file, 
# such as a json, csv or excel file. The textfile command can read in the texts 
# and document variables simultaneously from these files when the name of 
# the field containing the texts is specified.
tf1 <- readtext("inaugTexts.csv", text_field = "inaugSpeech")
myCorpus <- corpus(tf1)
tf2 <- readtext("text_example.csv", text_field = "Title")
myCorpus <- corpus(tf2)
head(docvars(tf2))


# -------------------------------------------------------------------
# Working with corpus objects
# -------------------------------------------------------------------
recentCorpus <- corpus_subset(data_corpus_inaugural, Year > 1980)
oldCorpus <- corpus_subset(data_corpus_inaugural, Year < 1880)

require(magrittr)
## Loading required package: magrittr
demCorpus <- corpus_subset(sotuCorpus, party == 'Democratic')
demFeatures <- dfm(demCorpus, remove = stopwords('english')) %>%
  dfm_trim(min_doc = 3, min_count = 5) %>% 
  dfm_weight(scheme='prop')
topfeatures(demFeatures)

#  quanteda corpus objects can be combined using the + operator:
data_corpus_inaugural2 <- demCorpus + repCorpus
dfm(data_corpus_inaugural2, remove = stopwords('english'), verbose = FALSE) %>%
  dfm_trim(min_doc = 3, min_count = 5) %>% 
  dfm_tfidf() %>% 
  topfeatures

# It should also be possible to load a zip file containing texts directly 
# from a url. However, whether this operation succeeds or not can depend 
# on access permission settings on your particular system (i.e. fails 
# on Windows):

immigfiles <- readtext("https://github.com/kbenoit/ME114/raw/master/day8/UKimmigTexts.zip")
mycorpus <- corpus(immigfiles)
summary(mycorpus)