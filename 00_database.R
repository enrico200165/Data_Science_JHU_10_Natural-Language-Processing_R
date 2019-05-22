# Objective: 
# Store dataframes (with frequencies) to sqlite DB
# probably created for eventual usage on shyny
# Status: currently abandoned
#

require(dplyr)
require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(data.table)

source("01_globals.R")


dbname <-  "ngrams.sqlite"
conn <- NULL

# --------------------------------------------------------------------
ngram_dtf_to_DB <- function()          
# --------------------------------------------------------------------
{
conn <- DBI::dbConnect(RSQLite::SQLite(),dbname)
prt(file.exists(dbname))


copy_to(conn, dtf_sep_1gram, "1grams_freq",
  temporary = FALSE, 
  indexes = list("primo")
)

copy_to(conn, dtf_sep_2gram, "2grams_freq",
  temporary = FALSE, 
  indexes = list(
    c("primo", "secondo"),
    "primo", "secondo")
)

copy_to(conn, dtf_sep_3gram, "3grams_freq",
  temporary = FALSE, 
  indexes = list(
    c("primo", "secondo","terzo"),
    c("primo", "secondo"),
    "primo", "secondo" , "terzo")
)  
DBI::dbDisconnect(conn)
conn <- NULL
}
