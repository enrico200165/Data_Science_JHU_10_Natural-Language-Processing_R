# require(dplyr)
# require(ggplot2)
# require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
# require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
# require(quanteda.corpora)
#install.packages("spacyr")
require(pryr)
require(stringr)
require(data.table)

# source("01_globals.R")




# Using a data.frame
untracemem(df)
df <- data.frame(x=1)
tracemem(df)
# df$x[1] <- 99
dt <- data.table(df)
rm(df)
dt$x[1] <- 99

df <- NULL ; untracemem(df); untracemem(dt2)
df <- data.frame(x=1); tracemem(df)
setDT(df);
class(df)
rm(df)
df$x[1] <- 99
df$x[1] <- 88
df$x[1] <- 77



