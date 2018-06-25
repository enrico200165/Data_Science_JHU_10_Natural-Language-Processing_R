require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
# require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)
library(ggplot2)

s1 <- 'my example text'
length(s1)
## [1] 1
nchar(s1)
## [1] 15

s2 <- c('This is', 'my example text.', 'So imaginative.')
length(s2)
## [1] 3
nchar(s2)
## [1]  7 16 15
sum(nchar(s2))
## [1] 38

# We can use this to answer some simple questions about 
# the inaugural addresses.

# Which were the longest and shortest speeches?
  
inaugTexts <- texts(data_corpus_inaugural)
which.max(nchar(inaugTexts))
## 1841-Harrison 
##            14
which.min(nchar(inaugTexts))
## 1793-Washington 
##               2

# not possible to index into a “string” – 
# where a string is defined as a sequence of text characters – in R:
s1 <- 'This file contains many fascinating example sentences.'
s1[6:9]
## [1] NA NA NA NA


# To extract a substring, instead we use the substr function.
s1 <- 'This file contains many fascinating example sentences.'
substr(s1, 6,9)
## [1] "file"

# note that returns a LIST, split marker removed
# returning list is a good thing, the elements are the splitted parts
s1 <- 'split this string'
strsplit(s1, 'this')
## [[1]]
## [1] "split "  " string"

parts <- strsplit(names(inaugTexts), '-')
parts[1:3]
# [[1]]
# [1] "1789"       "Washington"
# 
# [[2]]
# [1] "1793"       "Washington"
# 
# [[3]]
# [1] "1797"  "Adams"
years <- sapply(parts, function(x) x[1])
pres <-  sapply(parts, function(x) x[2])

# paste() The way in which the elements are combined depends on the values 
# of the sep and collapse arguments:

paste('one', 'two', 'three')
## [1] "one two three"
paste('one', 'two', 'three', sep = '_')
## [1] "one_two_three"
paste(years, pres, sep = '-')[1:3]

# Character vectors can be compared using the == and %in% operators:
c1 <- c('apples', 'oranges', 'pears')
'pears' %in% c1
## [1] TRUE
c2 <- c('bananas', 'pears')
c2 %in% c1
## [1] FALSE  TRUE

# quanteda has a special wrapper for changing case, called char_tolower(), 
# which is better than the built-in tolower() ... calls the stringi package’s (see more below) function 
# stri_trans_tolower(), which is more sensitive to multi-byte encodings and 
# the definition of case transformations for non-European languages (and even 
# some “harder” European ones, such as Hungarian, which has characters not 
# used in any other language).
require(quanteda)
tolower(c("This", "is", "Kεφαλαία Γράμματα"))
## [1] "this"              "is"                "kεφαλαία γράμματα"

#  R even has a function to convert from glob expressions to regular expressions: 
# glob2rx().

grep('orangef', 'these are oranges')
## integer(0)
grep('pear', 'these are oranges')
## integer(0)
grep('orange', c('apples', 'oranges', 'pears'))
## [1] 2
grep('pears', c('apples', 'oranges', 'pears'))
## [1] 3

gsub('oranges', 'apples', 'these are oranges')
## [1] "these are apples"

#  stringi package is a large suite of character (“string”) handling functions 
# that are superior in almost every way to the equivalent base R functions. 
# One reason that they are better lies in how they handle Unicode text, 
# which includes character categories and covers all known languages

# If you are serious about low-level text processing in R, you will want 
# to spend time learning stringi.
# A somewhat simpler-to-use package than stringi is the stringr package. 
# It wraps many of stringi’s low level functions in more convenient wrappers,
# although with fewer options.
require(stringr)
pattern <- "a.b"
strings <- c("abb", "a.b")
str_detect(strings, pattern)
## [1] TRUE TRUE

# Regular expression variations
# NB non sta "rimouovendo", matcha solo lowercase
str_extract_all("The Cat in the Hat", "[a-z]+")
## [[1]]
## [1] "he"  "at"  "in"  "the" "at"
str_extract_all("The Cat in the Hat", regex("[a-z]+", TRUE))
## [[1]]
## [1] "The" "Cat" "in"  "the" "Hat"

str_extract_all("a\nb\nc", "^.")
## [[1]]
## [1] "a"
str_extract_all("a\nb\nc", regex("^.", multiline = TRUE))
## [[1]]
## [1] "a" "b" "c"

# secondo modo estende il matching di . 
str_extract_all("a\nb\nc", "a.")
## [[1]]
## character(0)
str_extract_all("a\nb\nc", regex("a.", dotall = TRUE))
## [[1]]
## [1] "a\n"

# Besides extracting strings, we can also replace them:
  
fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")
## [1] "-ne apple"     "tw- pears"     "thr-e bananas"
str_replace_all(fruits, "[aeiou]", "-")
## [1] "-n- -ppl-"     "tw- p--rs"     "thr-- b-n-n-s"

str_replace(fruits, "([aeiou])", "")
## [1] "ne apple"     "tw pears"     "thre bananas"
str_replace(fruits, "([aeiou])", "\\1\\1")
## [1] "oone apple"     "twoo pears"     "threee bananas"
# interessante
str_replace(fruits, "[aeiou]", c("1", "2", "3"))
## [1] "1ne apple"     "tw2 pears"     "thr3e bananas"
str_replace(fruits, c("a", "e", "i"), "-")
## [1] "one -pple"     "two p-ars"     "three bananas"

# Functions also exist for word detection
fruit <- c("apple", "banana", "pear", "pinapple")
str_detect(fruit, "e")
## [1]  TRUE FALSE  TRUE  TRUE
fruit[str_detect(fruit, "e")]
## [1] "apple"    "pear"     "pinapple"
str_detect(fruit, "^a")
## [1]  TRUE FALSE FALSE FALSE
str_detect(fruit, "a$")
## [1] FALSE  TRUE FALSE FALSE
str_detect(fruit, "b")
## [1] FALSE  TRUE FALSE FALSE
str_detect(fruit, "[aeiou]")
## [1] TRUE TRUE TRUE TRUE

# Also vectorised over pattern
str_detect("aecfg", letters)
##  [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
## [12] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [23] FALSE FALSE FALSE FALSE
sum(str_detect("aecfg", letters))

# segment words by their boundary definitions, which is part of the Unicode 
# definition. quanteda relies heavily on this for tokenization, 
# which is the segmentation of texts into sub-units (normally, terms).
# Word boundaries
words <- c("These are   some words.")
str_count(words, boundary("word"))
## [1] 4
str_split(words, " ")[[1]]
## [1] "These"  "are"    ""       ""       "some"   "words."
str_split(words, boundary("word"))[[1]]
## [1] "These" "are"   "some"  "words"

# stringr can also be used to remove leading and trailing whitespace. 
# “Whitespace” has an extensive definition, but can be thought of in its 
# most basic form as spaces (" "), tab characters (“”), and newline 
# characters (“”). str_trim() will remove these:
str_trim("  String with trailing and leading white space\t")
## [1] "String with trailing and leading white space"
str_trim("\n\nString with trailing and leading white space\n\n")
## [1] "String with trailing and leading white space"