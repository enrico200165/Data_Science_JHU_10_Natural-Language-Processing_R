
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

# --------------------------------------------------------
tstmp_fname <- function() 
# --------------------------------------------------------
{
  # used in file names, avoid funny charss
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

# --------------------------------------------------------
tstmp <- function() 
# --------------------------------------------------------
{
  # used in file names, avoid funny charss
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}


