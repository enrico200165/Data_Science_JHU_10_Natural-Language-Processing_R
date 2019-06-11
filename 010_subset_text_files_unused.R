

require(DBI); 
require(RSQLite); 
require(sqldf)


# ---------------------------------------------------------
buildWordsDBFromFile <- function(words_file, db_fname)
  # ---------------------------------------------------------
# reads text file and put words in an sqlite table
{
  words_table <- "words_en"
  words_col = "words"
  
  con = dbConnect(drv=RSQLite::SQLite(), dbname = db_fname)
  
  tables = dbListTables(con)
  if (length(tables) == 0) {
    print("creating DB")
    dbExecute(conn = con,"CREATE TABLE :tab ( :col TEXT);",
              params = list(tab = words_table, col = words_col));
  } else {
    print(paste("table",words_table, "already exists"))
  }
  
  results <- dbGetQuery(con, paste("SELECT count(*) FROM "
                                   ,words_table,";"))
  res = as.numeric(results)
  # dbClearResult(con)
  if (is.na(res) | res <= 0) {
    print("filling DB from file")
    dbWriteTable(conn = con, name =  words_table
                 ,value = words_file, row.names = FALSE, header = FALSE
                 ,overwrite = T, sep="#")
    dbDisconnect(con)
    unlink(db_fname)
  } else {
    print(paste("DB",db_fname,"already loaded"))
  }
  
  dbDisconnect(con)
  unlink(db_fname)
}

# ---------------------------------------------------------
readTxtFileToStringVectors <- function(fname, nrLinesToRead) {
  # ---------------------------------------------------------
  
  enc <- getOption("encoding")
  con <- file(fname, "rt", encoding = enc)
  # enc <- iconvlist()[309] # utf8
  
  linesRead <- character(0)
  tryCatch({
    linesRead <- readLines(con, nrLinesToRead, skipNul = T)
  }
  ,error = function(x) { print("error"); print(x)}
  ,warning = function(x) { print("warning"); print(x)}
  ,finally = { close.connection(con); gc(); return(linesRead) }
  )
  
  linesRead
}


