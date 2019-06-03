
# ####################################################################
#                       MODULE MISSION
# globals for predicton model - nothing more
# 
# ####################################################################

# --------------------------------------------------------------------
#                  CONSTANSTS
# --------------------------------------------------------------------

TYPE1_COLNAME <- "primo"   # column with type 1 of an ngram
TYPE2_COLNAME <- "secondo" # column with type 2 of an ngram
TYPE3_COLNAME <- "terzo"   # column with type 3 of an ngram
TYPES_COLNAMES <- c(TYPE1_COLNAME, TYPE2_COLNAME , TYPE3_COLNAME)

# --- bare DTF with ngrams probabilities
if (!exists("dtf_1gram_sep")) dtf_1gram_sep <- NULL
if (!exists("dtf_2gram_sep")) dtf_2gram_sep <- NULL
if (!exists("dtf_3gram_sep")) dtf_3gram_sep <- NULL