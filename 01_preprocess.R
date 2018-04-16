# Sources
# https://rpubs.com/lmullen/nlp-chapter
# http://www.mjdenny.com/Text_Processing_In_R.html

# install.packages("stringr", dependencies = TRUE)
library(stringr)


# --- DB ---
# https://www.r-bloggers.com/using-sqlite-in-r/
# install.packages("RSQLite", dependencies = TRUE)
# install.packages("sqldf")
library(DBI); 
library(RSQLite); 
library(sqldf)

library(readtext)
library(quanteda)


source("01_preprocess_lib.R")

readInQCorp(data_dir_corpus, FALSE)
print("")