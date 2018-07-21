require(dplyr)
require(ggplot2)
require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)
require(stringr)

source("01_globals.R")


silent <- F

x <- gc()
print(str(x))
keypress()
summary(x)


