#data on
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

# install.packages("stringr", dependencies = TRUE)
library(stringr)


# --- DB ---
# https://www.r-bloggers.com/using-sqlite-in-r/
# install.packages("RSQLite", dependencies = TRUE)
# install.packages("sqldf")
require(DBI);
require(RSQLite);
require(sqldf)

require(readtext)
require(quanteda)


source("01_preprocess_lib.R")

readInQCorp(data_dir_corpus, FALSE)
print("")