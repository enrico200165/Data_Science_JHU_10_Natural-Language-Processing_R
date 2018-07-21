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


# --------------------------------------------------------------------
vars_mem <- function()
# --------------------------------------------------------------------
# named by var ID array of sizes of existing variables
{
  vars_mem_ret <- sapply(ls(all.names = TRUE) ,pryr::object_size)
  vars_mem_ret <- sort(vars_mem , decreasing = T)
  # sapply(vars_mem,XiB)
}