
# ---------------------------------------------------------
last_n_tokens <- function(x, n) {
# ---------------------------------------------------------
  
  x <- trimws(x, which = "both")
  if (n <= 0)
    return(character(0))
  
  tokens <- strsplit(x, "\\s+")[[1]]
  l <- length(tokens)
  
  nr <- min(n,l)
  
  tokens[(l-nr+1):l]
}


print(paste(last_n_tokens(" ciao   ", -1)))

for (i in -1:10) 
  print(paste(i,last_n_tokens(" ciao salamone come   stai   ", i)))