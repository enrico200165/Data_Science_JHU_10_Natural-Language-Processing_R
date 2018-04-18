require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)



# ---------------------------------------------------------
# find ngram by name
# ---------------------------------------------------------
# probably slow
quantdfm[   , which(colnames(quantdfm) == "counsel")]
