require(dplyr)
require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)


source("01_globals.R")

dt <- data.table(
  x =    1:3
  ,y = 10*1:3)

dt[ , sum(.SD)]
# [1] 66

dt[ , .(sum(.SD))]
#    V1
# 1: 66

dt[ ,lapply(.SD,sum)]
#    x  y   z
# 1: 6 60 600