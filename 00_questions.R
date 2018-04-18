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
#   Useful, Scott
# ---------------------------------------------------------
# convert(x, to = c("lda", "tm", "stm", "austin", "topicmodels", "lsa",
#                   "matrix", "data.frame"), docvars = NULL)


# ---------------------------------------------------------
# find ngram by name
# ---------------------------------------------------------
# probably slow
quantdfm[   , which(colnames(quantdfm) == "counsel")]

# ---------------------------------------------------------
# Quanteda
# ---------------------------------------------------------
class(toks)
# [1] "tokens"
str(toks)
# List of 5965
# $ text166543: chr [1:1139] "" "man" "" "shot" ...
# $ text166544: chr [1:924] "Mitt" "Romney" "warned" "" ...
# $ text166545: chr [1:1632] "" "rape" "survivor" "support" ...
# $ text166546: chr [1:1163] "" "man" "" "shot" ...
# $ text166547: chr [1:1613] "" "" "sun" "went" ...

# ---------------------------------------------------------
#  looking for a value in a column
# ---------------------------------------------------------
kwds <- textstat_keyness(rbind(dfm_europe, dfm_britain), target = seq_along(toks_europe)) #
class(kwds)
# [1] "keyness"    "textstat"   "data.frame"
kwds[which(kwds[,1] == "uk"), ]

