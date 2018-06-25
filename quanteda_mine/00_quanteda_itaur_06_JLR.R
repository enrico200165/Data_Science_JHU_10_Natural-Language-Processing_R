require(quanteda, quietly = TRUE, warn.conflicts = FALSE)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#
# devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)
library(ggplot2)

# --- external files ---

source("01_globals.R")
prj_dir()



###########################################################
#   JLR
###########################################################

# Techniques useful in analyzing news articles:
# - Constructing subject specific tokens object using kwic
# - Identifying and concatenating multi-word expressions using textstat_collocations()

# Characteristics of news articles/transcripts
# It is common to construct a corpus of news articles (or transcripts) from databases 
# with keyword queries, but such a corpus still contains a lot of noises, because 
# a news article is a set sentences and paragraphs on various subjects. 
# To focus our analysis on particular subjects, we can construct a tokens by kwic().
# 1 Construct a tokens as usual from a corpus
# 2 Run kwic() on the tokens to create a kwic
# 3 Convert kiwc into tokens using as.tokens
# 4 Apply whatever analysis we want on tokens



require(quanteda)


# ---------------------------------------------------------
prj_dir()
