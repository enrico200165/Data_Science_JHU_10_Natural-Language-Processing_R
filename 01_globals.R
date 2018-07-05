
# --------------------------------------------------------------------
#                   CONSTANTS
# --------------------------------------------------------------------
TXT_CTR = "country"
TXT_LNG = "language"
TXT_TYP = "type"

TXT_FNAME = "file"
TXT_SIZE  = "size"
TXT_SIZE_U  = "size_units"
TXT_BYTES = "n_byte"
TXT_NCHAR = "n_char"
TXT_NTOKENS = "n_token"
TXT_NNLINES = "n_newline"

# languages
LNG_DE = "DE"
LNG_EN = "EN"
LNG_FI = "FI"
LNG_RU = "RU"
LANGUAGES = c(LNG_DE, LNG_EN, LNG_FI, LNG_RU)

# text types
TYPE_BLOG = "blog"
TYPE_NEWS = "news"
TYPE_TW = "twitter"
TYPES = c(TYPE_BLOG, TYPE_NEWS, TYPE_TW)


SERIAL_PREFIX <- "SERIALIZATION_"



# --------------------------------------------------------------------
#                   Global Variables
# --------------------------------------------------------------------
# they are evil, but here they are not and save acrobacies

use_full_corpus <- T

# qc: quanteda corpus
qc_full <- if (exists("qc_full")) qc_full else NULL

dfm_full <- if (exists("dfm_full")) dfm_full else NULL


# --------------------------------------------------------------------
#                   TO ORGANIZE
# --------------------------------------------------------------------



# orig_dir <- dirname(sys.frame(1)$ofile)
orig_dir <- getwd()
prj_dir <- function() setwd(orig_dir)

db_fname <- "words_en.sqlite"



# --------------------------------------------------------------------
#                         data directories
# --------------------------------------------------------------------


dev_data_dir <- function() {
  if (Sys.info()["nodename"] == "LTPGJSDPX1") {
    "C:\\Users\\e_viali\\Documents\\dev\\ITAUR"
  } else if (Sys.info()["nodename"] == "DESKTOP-B40LLN4" ) {
    #"C:\\Users\\enrico\\Documents\\dev\\dev_data"
    file.path("C:","Users","enrico","Documents","dev","dev_data")
  } else if (Sys.info()["nodename"] == "THPAD-W530") {
    "V:\\data\\pers_dev\\data_dev"
  } else if (Sys.info()["nodename"] == "enrico-ThinkPad-W530") {  
    file.path("/","media","enrico","usbdata","data","pers_dev","data_dev")
  } else {
    NA
  }
}


# superdir= ".."
data_dir <- dev_data_dir();
dir.exists(data_dir)
data_dir_cap <- file.path(data_dir,"capstone_data")
dir.exists(data_dir_cap)

data_dir_corpus_full <-   file.path(data_dir_cap,"data_in","corpus_full")
data_dir_corpus_subset <- file.path(data_dir_cap,"data_in","corpus_subset")


itaur_dir <- function() {
  if (Sys.info()["nodename"] == "LTPGJSDPX1") {
    "C:\\Users\\e_viali\\Documents\\dev\\ITAUR"
  } else {
    if (Sys.info()["nodename"] == "asus") {
      "asusdir"
    } else if (Sys.info()["nodename"] == "THPAD-W530") {
      "C:\\Users\\enrico\\GDrv_enrico.viali\\CAPSTONE\\Quanteda\\ITAUR"
    } else {
      NA
    }
  }
}


# --------------------------------------------------------------------
  inc <- function(e1) eval.parent(substitute(e1 <- e1+1))
# --------------------------------------------------------------------


#---------------------------------------------------------------------
  setCoreDF <- function(std_df, lng, ttype, name_col, value, set_it) 
#---------------------------------------------------------------------
# assumes a data frame with language and text type as ID columns
# adds value to a given column
{
  stopifnot(c(TXT_LNG,TXT_TYP) %in% names(std_df))
  if (missing(set_it)) set_it <- FALSE
  
  varName <- deparse(substitute(std_df))
  
  row_to_update <- which(
    std_df[[TXT_LNG]] == lng & std_df[[TXT_TYP]] == ttype)
  col_to_update <- which(name_col == names(std_df))
  
  if ( (length(row_to_update) <= 0) 
      || (length(col_to_update) <= 0)
      ){
    print("warn: col or row not found")
    return(FALSE)
  }

  if(set_it) {
    std_df[row_to_update,col_to_update] <- value
  } else { # add to it
    std_df[row_to_update,col_to_update] <- value + std_df[row_to_update,col_to_update]
  }
  
  assign(varName,std_df,parent.frame(n = 1)) # pass it outside
  TRUE
}


#---------------------------------------------------------------------
  serializeIfNeeded <- function(dfPar, forceIt, rdsFName) 
#---------------------------------------------------------------------
{
  
  if (missing(forceIt))
    forceIt <- FALSE
  varName <- deparse(substitute(dfPar))
  if (missing(rdsFName)) {
    rdsFName <- paste0(varName,".rds")
  }
  
  if (!grepl(SERIAL_PREFIX, rdsFName))
    rdsFName <- paste0(SERIAL_PREFIX,rdsFName)
  
  if (!file.exists(rdsFName) || forceIt) {
    # if(exists(varName)) {
    if(TRUE) {
      # 
      print(paste("serializing var:",varName,"to file:",rdsFName,"..."))
      saveRDS(dfPar, file = rdsFName)
      print(paste("FINISHED serializing",varName))
    } else {
      # print(paste("not serializing because not exists var: ",varName))
    }
  }
}


#---------------------------------------------------------------------
  readIfEmpty <- function(df, rdsFName, forceIt) 
#---------------------------------------------------------------------
# probably should be rewritten (to use a better subfunction I will
# probably write soon)
# return:
# TRUE if it was or has bee filled
{
  if (missing(forceIt))
    forceIt <- FALSE
 
  ret <- FALSE
   
  varName <- ""
  varName <- deparse(substitute(df))
  if (missing(rdsFName)) {
    rdsFName <- paste0(varName,".rds")
  }
  
  if (!grepl(SERIAL_PREFIX, rdsFName))
    rdsFName <- paste0(SERIAL_PREFIX,rdsFName)
  
  if (!exists(varName)
      || is.null(df)
      || (length(df) <= 0 && nrow(df) <= 0)) {
    # print(paste(varName,"is empty", rdsFName))
    if (file.exists(rdsFName)) {
      print(paste("reading serialization for:",varName," file:", rdsFName))
      df <- readRDS(rdsFName)
      #assign(varName,df,.GlobalEnv) # pass it outside
      assign(varName,df,parent.frame(n = 1)) # pass it outside
      ret <- TRUE
    } else {
      print(paste(varName,"cannot read, no file: ", rdsFName))
      ret <- FALSE
    }
  } else {
    print(paste(varName,"alread filled, size:",pryr::object_size(df)))
    ret <- TRUE
  }
  # print(paste("exit",varName, ))
  
  if (ret) {
    serializeIfNeeded(df ,forceIt,rdsFName)
  }
  
  ret
}
# pippo <- 1:10
# serializeIfNeeded(pippo)
# pippo <- NULL
# readIfEmpty(pippo)



# --------------------------------------------------------------------
  removeAllVarExcept <- function (survivors,e)
# --------------------------------------------------------------------
{
  if (missing(e))
    #e <- env_parent()
    e <- parent.frame()
  
  names_initial <- rlang::env_names(e)

  victims <- setdiff(rlang::env_names(e),survivors)
  rlang::env_unbind(e, victims)

  names_removed <- setdiff(names_initial, rlang::env_names(e))
}


# --------------------------------------------------------------------
  GiB <- function (x, digits)
# --------------------------------------------------------------------
{
  if (missing(digits))
    digits <- 2
  round(x/(2^30),digits)
}


# --------------------------------------------------------------------
  MiB <- function (x, digits)
# --------------------------------------------------------------------
{
  if (missing(digits))
    digits <- 2
  round(x/(2^20),digits)
}

# --------------------------------------------------------------------
  clean_rds <- function(patt)
# --------------------------------------------------------------------
{
  patt = if(missing(patt)) ".*ubset.*" else patt
  patt <- paste0(patt,".*.rds")
  print(list.files(".",patt))
  file.remove(list.files(".",patt))
}


# ------------------------------------------
#    Tests
# ------------------------------------------

testRemoveAllVarExcept <- function() {
  a <- 1;  b <-2; c <- 3; d <- 4; e <- 99; f <- 5
  removed <- removeAllVarExcept(c("d","previous_names"))
  print(paste("vars removed: ",paste(removed,collapse = " ")))
}


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


  test_Globals.R <- function() 
{
  testRemoveAllVarExcept()
  testReadIfEmpty()
  testAddToCoreDF()
}

#  test_Globals.R()
  