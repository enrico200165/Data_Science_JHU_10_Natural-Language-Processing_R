require(data.table)
require(dplyr)
require(nycflights13)
source("01_globals.R")


colnames_in_variables <- function() {

  dt <- data.table(a = 1:3 , b = 11:13)
  newcols_names <- c("newcol1" , "newcol2")
  dt[, newcols_names[1:2] := list(a + 5, b + 5)]
  print(dt)
  dt[, newcols_names := list(a + 5, b + 5)]
  print(dt)
}


keys_and_indexes <- function() {
  
  keys <- c("a","b","c")

  d <- data.table(a = 1, b = 2, c = 3)
  
  # set key ok
  setkeyv(d,keys); print(key(d))
  
  # indexes seem to fail when key is present
  setindexv(d,keys); print(indices(d))

  # without key indexes work  
  d <- data.table(a = 1, b = 2, c = 3)
  setindexv(d,keys); print(indices(d))

}

