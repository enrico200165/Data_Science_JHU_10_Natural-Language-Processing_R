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
# DT[, D] will give:
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


# --------------------------------------
# Section 3 - Doing j by group
#--------------------------------------

# in J "very important in dt that groups"
# are returned in the order that they first appear" [DT creator]

DT1 <- data.table(A = c("c","b","a"), B =c(1, 2, 3, 4, 5, 6))
DT1

# can use functions in J/by?
# yes.
# aggregating on subset of rows
DT1[, .(MySum = sum(B),
        MyMean = mean(B)),
    by = .(A)]
# Groups in 1st Column    
#    A MySum MyMean
# 1: c     5    2.5
# 2: b     7    3.5
# 3: a     9    4.5


# Function calls in by
DT2 <- data.table(A = 1:5, B = 10:14)
#    A  B
# 1: 1 10
# 2: 2 11
# 3: 3 12
# 4: 4 13
# 5: 5 14
DT2[, .(Mysum = sum(B)), by = .(Grp = A%%2)]
# even and odd number group
#    Grp Mysum
# 1:   1    36
# 2:   0    24


# shortcut in DT
# with just 1 item in j or y 
# .() not necessary
# can put even just a function
DT2[, sum(B), by = A%%2]
#    A V1
# 1: 1 36
# 2: 0 24


# subsetting rows when also using j and by
# obviously possible, behaves as expected
# Grouping only on a subset, 2:4 rows do group sum
DT2
# A  B
# 1: 1 10
# 2: 2 11
# 3: 3 12
# 4: 4 13
# 5: 5 14
DT2[2:4, sum(B), by = A%%2]
#    A V1
# 1: 0 24
# 2: 1 12


# For each Species, print the mean Sepal.Length
DT <- data.table(iris)
DT[, mean(Sepal.Length), by = Species]
# Species    V1
# 1:     setosa 5.006
# 2: versicolor 5.936
# 3:  virginica 6.588


# Print mean Sepal.Length, grouping by first letter of Species
DT[, mean(Sepal.Length), by = substr(Species,1,1)]
# substr    V1
# 1:      s 5.006
# 2:      v 6.262


# .N can be used in 
# i: typically used for the last row or an offset from it
# j: number of rows in this group.very powerful when you use it in combination with by.
DT <- data.table(iris)
DT[, .(Count = .N), .(Area = 10 * round(Sepal.Length * Sepal.Width / 10))]
# Area Count
# 1:   20   117
# 2:   10    29
# 3:   30     4
# J: .(Count = .N) by N
# K: 
#    .() K seems a list
#    multipli di 10:
#    Area = 10 * round(Sepal.Length * Sepal.Width / 10)


# select n first or last rows of a column while grouping
# ex. 
# DT[, .(C = tail(C,2)) , by = A]

# number of rows in group
# .N 
# used in j too



# UTILE assieme a sort, per le N miliori frequenze ...
# can you return multiple numbers in J
# Create the data.table DT
DT <- data.table(A = rep(letters[2:1], each = 4L), 
                 B = rep(1:4, each = 2L), 
                 C = sample(8))
# Create a new data.table DT2 with 3 columns, A, B and C, 
# where C is the cumulative sum of the C column of DT
DT2 <- DT[, .(C = cumsum(C)), by = .(A, B)]
DT2
# A B  C
# 1: b 1  6
# 2: b 1 10
# 3: b 2  8
# 4: b 2 13
# 5: a 3  3
# 6: a 3 10
# 7: a 4  1
# 8: a 4  3
# Select from DT2 the last two values from C while you group by A
DT2[, .(C = tail(C, 2)), by = .(A)]
# A  C
# 1: b  8
# 2: b 13
# 3: a  1
# 4: a  3


# subsetting and aggregating ex. hflights
# install.packages("hflights")
library(hflights)
library(data.table)
DT <- as.data.table(hflights)
DT[Month==10,mean(na.omit(AirTime)), by=UniqueCarrier]


#Returns all those rows where the Carrier is AA American Airlines
DT[UniqueCarrier == "AA"]


# Return the penultimate row of DT
DT[.N-1]


# j part ... can be used 
# All kinds of functions can be used, 
# which is a strong point of the data.table package.
DT[, mean(na.omit(ArrDelay))]
# [1] 7.094334


# When selecting several columns in the 'j' part, you need to use 
# the ‘.()’ notation. 
# just an alias to ‘list()’. 
# It returns a data table, 
# not using ‘.()’ only returns a vector, as shown above.


# not using ‘.()’ in j return 
#only returns a vector
DT[, .(mean(na.omit(DepDelay)), mean(na.omit(ArrDelay)))]
# V1       V2
# 1: 9.444951 7.094334


# feature which requires the ‘.()’ notation 
# renaming columns inside the DT[…] command.
DT[, .(Avg_ArrDelay = mean(na.omit(ArrDelay)))]
# Avg_ArrDelay
# 7.094334
DT[, .(Avg_DepDelay = mean(na.omit(DepDelay)),
       avg_ArrDelay = mean(na.omit(ArrDelay)))]
# Avg_DepDelay Avg_ArrDelay
# 9.444951     7.094334


# Combining ‘i’ and ‘j’ example gives:
DT[UniqueCarrier=="AA", 
   .(
     Avg_DepDelay = mean(na.omit(DepDelay)),
     Avg_ArrDelay = mean(na.omit(ArrDelay)),
     plot(DepTime,DepDelay,ylim=c(-15,200)),
     abline(h=0))
   ]


# hflights example average delay before departure, 
# grouped by where the plane is coming from.
DT[,mean(na.omit(DepDelay)),by=Origin]
# Origin  V1
# IAH     8.436951
# HOU    12.837873


# ======================================================
#                 PART 2
# ======================================================


# chaining
DT[][]
# ex. con spazio esagerato
DT = data.table(A = 1:5, B = 11:15, C = 21:25)
DT[, .(C = cumsum(C)), by = .(A, B)]  [, .(C = tail(C, 2)), by = A]


# chaining example
DT <- data.table(A = rep(letters[2:1], each = 4L), 
                 B = rep(1:4, each = 2L), 
                 C = sample(8)) 
DT
# A B C
# 1: b 1 3
# 2: b 1 1
# 3: b 2 5
# 4: b 2 7
# 5: a 3 2
# 6: a 3 4
# 7: a 4 8
# 8: a 4 6
# Combine the two steps in a one-liner
# DT2 <- DT[, .(C = cumsum(C)), by = .(A, B)]
# DT2[, .(C = tail(C, 2)), by = A]
DT[, .(C = cumsum(C)), by = .(A, B)][, .(C = tail(C, 2)), by = A]


# unique values from a group of rows
# https://stackoverflow.com/questions/44945646/for-r-data-table-how-to-use-uniquen-in-order-count-unique-distinct-values-in
# value of A irrilevant, count distinct B,C
DT <- data.table(A = 1:8,
                 B = rep(letters[1:2], each = 4L),
                 C = rep(1:4, each = 2L)) 
# unique returns a data.table with duplicated rows removed, by columns 
# specified in by argument. 
# When no by then duplicated rows by all columns are removed.
DT[, .(distinct_groups = uniqueN(A)), by = .(B, C)]

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
