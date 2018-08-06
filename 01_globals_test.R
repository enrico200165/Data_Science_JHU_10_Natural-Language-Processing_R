

source("01_globals.R")

# illegal position, ease frequent access

silent <- F
fulldata <- F


# ====================================================================
#                               Tests
# ====================================================================

# --------------------------------------------------------------------
  test_prt <- function() 
{
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


# --------------------------------------------------------------------
testRemoveAllVarExcept <- function() {
  a <- 1;  b <-2; c <- 3; d <- 4; e <- 99; f <- 5
  removed <- removeAllVarExcept(c("d","previous_names"))
  print(paste("vars removed: ",paste(removed,collapse = " ")))
}


# --------------------------------------------------------------------
testAddToCoreDF <- function() {
  
  d <- data.frame(language = c(rep("EN",3),rep("RU",3)
    ,rep("FI",3),rep("DE",3))
    , country = c(rep("USA",3),rep("ru",3)
                  ,rep("fi",3),rep("ddr",3))
    ,type = rep(c("blog","news","twitter"),4)
    ,value = rep(100,12)
    )

  setCoreDF(d,"DE","blog","value",90)
  setCoreDF(d,"EN","blog","value",91)
  setCoreDF(d,"FI","blog","value",92)
  setCoreDF(d,"RU","blog","value",93,T)

  setCoreDF(d,"DE","news","value",80)
  setCoreDF(d,"EN","news","value",81)
  setCoreDF(d,"FI","news","value",82)
  setCoreDF(d,"RU","news","value",83,T)

  t <- "twitter"
  i <- 50
  setCoreDF(d,"DE",t,"value",i)
  setCoreDF(d,"EN",t,"value",i+1)
  setCoreDF(d,"FI",t,"value",i+2)
  setCoreDF(d,"RU",t,"value",(i+3),T)

  setCoreDF(d,"DE","news","value",99999,T)
  
  print(d)
}


# --------------------------------------------------------------------
  testReadIfEmpty <- function() 
{
  
  mydf <- NULL
  if (readIfEmpty(mydf)) {
    # print("ok, I could read it")
  } else {
    print("NO, I could NOT read it")
    # mydf <- data.frame(a = 1:3,b = 11:13)
  }
  
  serializeIfNeeded(mydf,FALSE)
  mydf <<- NULL
  readIfEmpty(mydf)
  print(mydf)
}



# --------------------------------------------------------------------
  test_rie <- function() 
{
  # check if it serializes correctly
  mydf <- data.frame(x = 1:3)
  rie(mydf, function() data.frame(x = 1:3))

  mydf <<- NULL
  rie(mydf, function() data.frame(x = 1:3))
  print(mydf)
}


    
# --------------------------------------------------------------------
  test_getSerializeFName <- function()
  {
    name <- "pippo"
    prt(name,"->",getSerializFName(name))

    name <- "pluto"
    prt(name,"->",getSerializFName(name,paste0("forcefile_",name)))

    name <- ""
    prt(name,"->",getSerializFName(name,paste0("forcefile_",name)))
    
    name <- NULL
    prt(name,"->",getSerializFName(name,"forcefile_xxx"))

            
    # below here should stop/abort
    
    # name <- ""
    # prt(name,"->",getSerializFName(name))
    
    
    # name <- ""
    # prt(name,"->",getSerializFName(name,""))
    
    # prt(name,"->",getSerializFName(NULL))
    
  }

  
# --------------------------------------------------------------------
  test_XiB <- function() {

  silent <<- F
  prt(XiB(1*2^(10+3)))
  prt(XiB(1*2^(20+3)))
  prt(XiB(2.1*2^(30+3)))

}

  
# --------------------------------------------------------------------
test_coverage_of_freq_list <- function() 
{
  qtiles_vec <- c(0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.97,0.98,0.99)

  # x <- rep(1,1000)
  x <- 100:1
  ret <- coverage_of_freq_list(x, qtiles_vec)
  print(rbind(qtiles_vec ,ret[[1]],ret[[2]],ret[[3]]))
  
  y <- x[1:10] # unable to satisfy  
  ret <- coverage_of_freq_list(y, qtiles_vec)
  print(rbind(qtiles_vec ,ret[[1]],ret[[2]],ret[[3]]))

}
    
# -----------------------------------------------------------------------
  test_Globals.R <- function() 
# -----------------------------------------------------------------------
  
{
  
  silent <<- F
  fulldata <<- F


  
  # testRemoveAllVarExcept()
  # testReadIfEmpty()
  # test_rie()
  # test_prt()

  # testAddToCoreDF()
  # test_getSerializeFName()
  test_coverage_of_freq_list()
}

# 
test_Globals.R()
