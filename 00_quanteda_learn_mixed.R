require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)

data_dir <- system.file("extdata/", package = "readtext")
list.files(data_dir)
evdata_dir <- file.path("../../..", "capstone_data",fsep = .Platform$file.sep)
#list.files(evdata_dir)



# If your text data is stored in a pre-formatted file where one column 
# contains the text and additional columns might store document-level 
# variables (e.g. year, author, or language)
inaug_data <- read.csv(paste0(data_dir, "/csv/inaugCorpus.csv"))

# Alternatively, you can use the readtext package to import character 
# (comma- or tab-separated) values.
inaug_data <- readtext(paste0(data_dir, "/tsv/dailsample.tsv"), text_field = "speech")


# individual text files usually do not contain document-level variables. 
# However, you can create document variables using the readtext package.
udhr_data <- readtext(paste0(data_dir, "/txt/UDHR/*"))

# You can generate document-level variables based on the file names 
# using the docvarnames and docvarsfrom argument. 
# dvsep = "_" specifies the value separator in the filenames.
# encoding = "ISO-8859-1" determines character encodings of the texts.

eu_data <- readtext(paste0(data_dir, "/txt/EU_manifestos/*.txt"),
  docvarsfrom = "filenames"
  ,docvarnames = c("unit", "context", "year", "language", "party")
  ,dvsep = "_",encoding = "ISO-8859-1")
str(eu_data)



cap_data <- readtext(paste0(evdata_dir,"/data_in/corpus/en*.txt"),
                    docvarsfrom = "filenames"
                    ,docvarnames = c("lng", "title")
                    ,dvsep = "_", encoding = "UTF-8" 
                    #,encoding = "ISO-8859-1"
                    )
print(object.size(cap_data), units="Mb")

# You can also read JSON files (.json) downloaded from the Twititer stream API.
twitter_data <- readtext(file.path(paste0(data_dir,"json/twitter.json")))

#Convert Character Vector between Encodings
# iconv(x, from = "", to = "", sub = NA, mark = TRUE, toRaw = FALSE)
#  character encoding not supported by R.
setdiff("ghgh", iconvlist())



immig_corp <- corpus(data_char_ukimmig2010, 
                     docvars = data.frame(party = names(data_char_ukimmig2010)))
summary(immig_corp)
corp_sum_df <- as.data.frame(summary(immig_corp))
names(corp_sum_df) <- tolower(names(corp_sum_df))
library(ggplot2)
qplot(x = text, y=tokens, data=corp_sum_df)


# construct a corpus

data_dir <- system.file("extdata/", package = "readtext")


inaug_data <- readtext(paste0(data_dir, "/csv/inaugCorpus.csv"), text_field = "texts")
names(inaug_data)

# Construct a corpus from inaug_data.
inaug_corp <- corpus(inaug_data)
summary(inaug_corp, 5)

cap_corp <- corpus(cap_data)
cap_corp_sum_df <- summary(cap_corp, 5)


# can edit the docnames
docid <- paste(inaug_data$Year, 
               inaug_data$FirstName, 
               inaug_data$President, sep = " ")
docnames(inaug_corp) <- docid
summary(inaug_corp, 5)

docnames(cap_corp) <- cap_corp$text
cap_corp_sum_df <- summary(cap_corp)

# SUBSET CORPUS
# select documents in a corpus based on document-level variables.
corp <- data_corpus_inaugural
print(ndoc(corp))
head(docvars(corp))
recent_corp <- corpus_subset(corp, Year >= 1990)
print(ndoc(corp))
dem_corp <- corpus_subset(corp, President %in% c('Obama', 'Clinton', 'Carter'))
ndoc(dem_corp)






https://tutorials.quanteda.io/basic-operations/corpus/corpus/

  
  
  
  
  
  
  testcorp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
                       d3 = "b b c e", d4 = "e e f a b"),
                     docvars = data.frame(grp = c(1, 1, 2, 3)))

testcorp[1]
##        d1 
## "a b c d" 
class(testcorp[4])
# [1] "character"
# nota che solo l'ultimo elemento ha un nome
testcorp[4]
# d4
# "e e f a b"
# qui sotto non funziona
names(testcorp[4])[1] <- "xy"


(myDfm <- dfm(data_corpus_inaugural[1:5]))
# keep only words occurring >=10 times and in >=2 documents
dfm_trim(myDfm, min_count = 10, min_docfreq = 2)

