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

# vignette at: 
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-secondary-indices-and-auto-indexing.html

flights <- flights[ ,-c(4 ,5 ,7 ,8 ,11 ,12 ,18 ,19)]
flights

