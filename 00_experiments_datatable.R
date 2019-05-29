require(data.table)
require(dplyr)
source("01_globals.R")



# -------------------------------------------------------------
#          Corso Data Camp
# -------------------------------------------------------------
#☺ Note da http://xiangxing98.github.io/R_Learning/data.table-data_analysis_with_data.table.html
# Create my_first_data_table
my_first_data_table <- data.table(x = c("a", "b", "c", "d", "e"), 
                                  y = c(1, 2, 3, 4, 5))  

# Create a data.table using recycling
DT <- data.table(a = c(1L, 2L), b = LETTERS[1:4])
# Print the third row to the console
print(DT[3])
# Print the second and third row to the console without using commas
print(DT[2:3])

# Print the second to last row of DT using .N
print(DT[.N-1])

# Print the column names of DT
print(names(DT))

# Print the number or rows and columns of DT
print(dim(DT))

# Print a new data.table containing rows 2, 2, and 3 of DT
DT2 = DT[c(2,2,3)]
print(DT2)

# get last row in DT
# DT[.N] and DT[nrow(DT)] will both return 
DT[.N]
DT[nrow(DT)]



# --- video 2 ---

# .()
# alias to list

DT = data.table(A = 1:3, C = 21:23) # EV creato da me
DT[, .(Total = sum(A), Mean = mean(C))]


DT = data.table(A = 1:5, B = letters[1:5], C = 6:10)
DT[, .(B, C = sum(C))]


# j flexibility
# can throw anything into j
# not necessary to return a value
# not necessary to wrap in .() when not interested in its return (NULL)
DT[, plot(A, C)]

# Curly Braces to evaluate a series of expressions (separated by new lines or semicolons) and return only the last expression:
DT[, {print(A); hist(C); NULL}]



# DT[ , .(B) ] vs. DT[ , .B]
dt <- data.table(x = 1:5, y = 10*1:5)
str(dt[,x])
str(dt[,.(x)])                    


# Create a subset containing the columns B and C for rows 1 and 3 of DT. 
#  Simply print out this subset to the console
DT[c(1,3), .(B, C)]
# From DT, create a data.table, ans with two columns: B and val, 
# where val is the product of A and C
ans = DT[ , .(B , val = A * C)]
# Fill in the blanks such that ans2 equals target below here
target <- data.table(B = c("a", "b", "c", "d", "e", 
                           "a", "b", "c", "d", "e"), 
                     val = as.integer(c(6:10, 1:5)))
ans2 <- DT[, .(B, val = c(C, A))]


# non-existing column
# ex. type D <- 5 in the console. 
# What do you think is the output of DT[, .(D)] and DT[, D]?
D <- 5
# the following on some R versions should give:
# Error: Item 1 of j is 5 which is outside the column number range [1,ncol=3]
# not in my R version, let's see
DT[, D]
# Error in `[.data.table`(DT, , D) : 
#   j (the 2nd argument inside [...]) is a single symbol but column 
# name 'D' is not found. Perhaps you intended DT[, ..D]. 
# This difference to data.frame is deliberate and explained in FAQ 1.1.
class(DT[, .(D)])
# [1] "data.table" "data.frame"
class(DT[, D]) # in my version gives same error as DT[, D]
DT[, ..D]
# Error in `[.data.table`(DT, , ..D) : 
#   Item 1 of j is 5 which is outside the column number 
# range [1,ncol=3]
D <- 2
DT[, ..D]
# B
# 1: a
# 2: b
# 3: c
# 4: d
# 5: e


# -------------------------------------------------------------
#         Fine corso data-camp
# -------------------------------------------------------------


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
