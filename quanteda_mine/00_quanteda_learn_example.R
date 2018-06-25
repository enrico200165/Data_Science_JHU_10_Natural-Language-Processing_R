require(quanteda)
# help(package = "quanteda")
#install.packages("readtext")
require(readtext)
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)

# from http://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html


# Define some utilities before starting NLP:
  
  # Generic function for parallelizing any task (when possible)
  parallelizeTask <- function(task, ...) {
    # Calculate the number of cores
    ncores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    #print("Starting task")
    r <- task(...)
    #print("Task done")
    stopCluster(cl)
    r
  }

# Returns a vector of profanity words
getProfanityWords <- function(corpus) {
  profanityFileName <- "profanity.txt"
  if (!file.exists(profanityFileName)) {
    profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
    download.file(profanity.url, destfile = profanityFileName, method = "curl")
  }
  
  if (sum(ls() == "profanity") < 1) {
    profanity <- read.csv(profanityFileName, header = FALSE, stringsAsFactors = FALSE)
    profanity <- profanity$V1
    profanity <- profanity[1:length(profanity)-1]
  }
  
  profanity
}

# ---------------------------------------------------------
# N-gram creation
# ---------------------------------------------------------
# Separate the text into sentences before creating the ngrams as to avoid ngrams like: finish sentence. start sentence. Also add a symbol for start and end of sentence which may or may not be useful later. Since quanteda removes ‘<’, ‘>’ and ‘/’ during tokenization we’ll use ‘#s# and’#e#‘to mark the start and end of a sentence respectively, the’#’ symbol is safe as long as removeTwitter=TRUE.

makeSentences <- function(input) {
  output <- tokenize(input, what = "sentence", removeNumbers = TRUE,
                     removePunct = TRUE, removeSeparators = TRUE,
                     removeTwitter = TRUE, removeHyphens = TRUE)
  output <- removeFeatures(output, getProfanityWords())
  unlist(lapply(output, function(a) paste('#s#', toLower(a), '#e#')))
}

# Now a function for creating ngrams:
  makeTokens <- function(input, n = 1L) {
    tokenize(input, what = "word", removeNumbers = TRUE,
             removePunct = TRUE, removeSeparators = TRUE,
             removeTwitter = FALSE, removeHyphens = TRUE,
             ngrams = n, simplify = TRUE)
  }

# Use the functions and create the Document Frequency Matrix:
  
sentences <- parallelizeTask(makeSentences, qcorpus)
ngram1 <- parallelizeTask(makeTokens, sentences, 1)
ngram2 <- parallelizeTask(makeTokens, sentences, 2)
ngram3 <- parallelizeTask(makeTokens, sentences, 3)
ngram4 <- parallelizeTask(makeTokens, sentences, 4)

dfm1 <- parallelizeTask(dfm, ngram1, ignoredFeatures=getProfanityWords())
dfm2 <- parallelizeTask(dfm, ngram2, ignoredFeatures=getProfanityWords())
dfm3 <- parallelizeTask(dfm, ngram3, ignoredFeatures=getProfanityWords())
dfm4 <- parallelizeTask(dfm, ngram4, ignoredFeatures=getProfanityWords())

# ---------------------------------------------------------
# DataTables and Smoothing
# ---------------------------------------------------------
# We can create a DataTable (which is faster and more efficient than DataFrames) with 2 colums: the ngram and its count.

dt4 <- data.table(ngram = features(dfm4), count = colSums(dfm4), key = "ngram")
# Store the total number of ngrams (features in quanteda terminology) for later use
nfeats <- nfeature(dfm4)

#Let’s use it to search an ngram, where regex is an ngram of the form ‘a_nice_four_gram’

# add ^ to make sure we sra at the beginning of an ngram
hits <- DT[ngram %like% paste("^", regex, "_", sep = ""), ngram]

#We can have a recursive function that searches a 4-gram and if not found then search for a 3-gram, 2-gram, etc. I leave that as an exercise to the reader. Then let’s prepare different smoothing techniques:
  
  if (length(hits) > 0) {
    print("Hit!")
    baseCount <-dts[[n0]][.(regex)]$count
    for (hit in hits) {
      DT[.(hit), ':=' (
        mle = count/baseCount, 
        lap = (count+1)/(baseCount+nfeats[[n0]]),
        gt = (count+1) * (countDFS[[n0]][count+1]/countDFS[[n0]][count]),
        sbo = count + 0.4*baseCount + stupidBO(ngram, n0-1)
      )]
    }
    DT[hits][order(-sbo)]
  }
# dts is a list of the different datatables created before where dts[[4]] would refer to the datatable fro 4-grams.
# baseCount is our Nc-1 count
# mle is sel-explanatory
# lap stands for laplacian aka add one smoothing
# gt is Good Turing
# sbo is stupid backoff
# Good Turing
# For GT I pre-calculated the frequency of frequencies (countDFS) by creating a list of size max(DTcount) and pre-filling it with 1s:
  
  createFreqs <- function(index) {
    DT <- dts[[index]]
    l <- rep(1, times = DT[, max(count)])
    for (n in unique(DT$count)) {
      l[n] <- DT[count == n, length(count)]
    }
    l[length(l)+1] <-1
    l
  }

countDF4 <- parallelizeTask(createFreqs, 4)

# Stupid Backoff
# Remember SBO is based on the idea of 0.4^0xNc + 0.4^1xNc-1 + 0.4^2xNc-2 + 0.4^3xNc-3, we already calculate the first 2 terms in the datatable so this function creates the last 2 terms.

stupidBO <- function(text, n) {
  if (n == 0) {
    return(0)
  } 
  l <- rep(1, times = n)
  exp <- n
  for (i in n:1) {
    regex <- getNgram(text, i, sep = "_")
    l[i] <- .4^exp * dts[[i]][.(regex)]$count
    exp <- exp + 1
  }
  return(sum(l))
}