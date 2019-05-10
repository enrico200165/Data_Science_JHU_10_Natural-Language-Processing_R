require(data.table)
require(dplyr)
source("01_globals.R")


dt4tests <- function() {
  data.table(a = 1:3 , b = 11:13, c = 21:23)
}

# verify column names in variables
colnames_in_variables <- function() {

  mydt <- dt4tests()
  
  newcols_names <- c("newcol1" , "newcol2")
  mydt[, newcols_names[1:2] := list(21:23, 31:33)]
  
  if (all(mydt[,c("newcol1")] == t(21:23))) {
    print("ok")
  } else {
    print(mydt)
  }
}
colnames_in_variables()


# Doc. setkey reorders (i.e. sorts) the rows of a data.table by the columns provided
# it's good practice to use column names rather than numbers. 
# This is whysetkey and setkeyv only accept column names. 
# If you use column numbers then bugs (possibly silent) can more easily creep into 
# your code 

check_keys_and_indexes_compatibilty <- function() {

  mydt <- dt4tests()
  
  # set key ok
  key_cols <- c("a","b","c")
  setkeyv(mydt,key_cols); print(mykey(dt))
  # indexes seem to fail when key is present
  setindexv(mydt,key_cols); print(indices(mydt))

  # without key indexes work  
  mydt <- dt4tests()
  setindexv(mydt,key_cols); print(indices(d))

}
check_keys_and_indexes_compatibilty()
