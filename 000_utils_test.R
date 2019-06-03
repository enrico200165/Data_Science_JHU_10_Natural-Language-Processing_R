
source("007_utils.R")

# --------------------------------------------------------------------
test_clean_rds <- function() {
  clean_rds()
}
#test_clean_rds()

# --------------------------------------------------------------------
test_XiB <- function() {
  
  silent <<- F
  prt(XiB(1*2^(10+3)))
  prt(XiB(1*2^(20+3)))
  prt(XiB(2.1*2^(30+3)))
  
}

# --------------------------------------------------------------------
test_rie <- function() {
  
  
  mydf <- data.frame(10:1)
  serializeIfNeeded(mydf,TRUE)
  
  # check if it serializes correctly
  if (rie(mydf, function() data.frame(10:1))) {
    print("ERROR I did not read it")
  }
  
  mydf <<- NULL
  if (!rie(mydf, function() data.frame(10:1))) {
    print("ERROR I did not read it")
  }
  str(mydf)
}
test_rie()

# --------------------------------------------------------------------
test_prt <- function() {
  # check if it serializes correctly
  silent <<-F
  prt("pippo", "pluto")
  Sys.sleep(1)
  prt("")
  Sys.sleep(2)
  prt()
  Sys.sleep(5)
  prt()
}
# test_prt()

# --------------------------------------------------------------------
test_getSerializeFName <- function() {
  name <- "pippo"
  print(paste(name,"->",getSerializFName(name)))
  
  name <- "pluto"
  print(paste(name,"->",getSerializFName(name,paste0("forcefile_",name))))
  
  name <- ""
  print(paste(name,"->",getSerializFName(name,paste0("forcefile_",name))))
  
  name <- NULL
  print(paste(name,"->",getSerializFName(name,"forcefile_xxx")))
  
  
  # below here should stop/abort
  
  # name <- ""
  # prt(name,"->",getSerializFName(name))
  
  
  # name <- ""
  # prt(name,"->",getSerializFName(name,""))
  
  # prt(name,"->",getSerializFName(NULL))
  
}

# --------------------------------------------------------------------
test_getSerializFName <- function() {
  
  name <- "enrico"
  sname <- getSerializFName("enrico")
  print(paste("serialization name for",name,"is",sname))
  
  name <- "enrico"
  sname <- getSerializFName("enrico","forced_worked")
  print(paste("serialization name for",name,"is",sname))
  
}
# test_getSerializFName()


# -----------------------------------------------------------------------
test_utils <- function()
{
  
  silent <<- F
  fulldata <<- F
  
  
  test_XiB()  
  testRemoveAllVarExcept()
  test_rie()
  test_prt()
  
  test_getSerializeFName()
  test_coverage_of_freq_list()
}

# 
test_utils()


