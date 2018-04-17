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
#   Advanced
###########################################################

#
old_dir <- getwd()
setwd("C:\\Users\\enrico\\GDRIVE\\CAPSTONE\\Quanteda\\ITAUR\\6_advanced")
load("tweetSample.RData")
str(tweetSample)

