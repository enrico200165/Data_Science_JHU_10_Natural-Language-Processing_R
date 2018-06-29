


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
TXT_NTOKENS = "n_tokens"
TXT_NNLINES = "n_newlines"




# --------------------------------------------------------------------
#                   TO ORGANIZE
# --------------------------------------------------------------------



# orig_dir <- dirname(sys.frame(1)$ofile)
orig_dir <- getwd()
prj_dir <- function() setwd(orig_dir)

db_fname <- "words_en.sqlite"

SERIAL_PREFIX <- "SERIALIZATION_"

inc <- function(e1) eval.parent(substitute(e1 <- e1+1))


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
data_dir_corpus_in <- file.path(data_dir_cap,"data_in","corpus")
data_dir_corpus_work <- file.path(data_dir_cap,"data_work","corpus")
dir.exists(data_dir_corpus_work)


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


#---------------------------------------------------------------------
  readIfEmpty <- function(df, nomeFile) 
#---------------------------------------------------------------------
{
  varName <- "1235413fsdfsdfsdfdsfsdfdfs"
  varName <- deparse(substitute(df))
  if (missing(nomeFile)) {
    nomeFile <- paste0(varName,".rds")
  }
  

  if (!exists(varName)
      || is.null(df)
      || (is.data.frame(df) && nrow(df) <= 0)) {
    if (file.exists(nomeFile)) {
      # print(paste(varName,"is empty, trying to read", nomeFile))
      df <- readRDS(nomeFile)
      #assign(varName,df,.GlobalEnv) # pass it outside
      assign(varName,df,parent.frame(n = 1)) # pass it outside
      return(TRUE)
    } else {
      # no file to read from
    }
    # otherwise would be always read
  } else {
    # print(paste(varName,"is not empty"))
  }
  # print(paste("exit",varName, "has rows", nrow(df)))
  FALSE
}



#---------------------------------------------------------------------
  serializeIfNeeded <- function(dfPar, forceIt, rdsFName) 
#---------------------------------------------------------------------
{
  varName <- deparse(substitute(dfPar))
  if (missing(rdsFName)) {
    rdsFName <- paste0(varName,".rds")
  }
  
  if (!file.exists(rdsFName) || forceIt) {
    # if(exists(varName)) {
    if(TRUE) {
      # print(paste("serializig to .rds var:",varName,"file:",rdsFName))
      saveRDS(dfPar, file = rdsFName)
    } else {
      # print(paste("not serializing because not exists var: ",varName))
    }
  }
   
}


# --- Corpuses ---
# qc: quanteda corpus

if (!exists("qc_news")) qc_news <- NA
if (!exists("qc_blogs")) qc_blogs <- NA
if (!exists("qc_twitts")) qc_twitts <- NA


# ------------------------------------------
#    Tests
# ------------------------------------------
# print(getDataDir(data_dir))

testIt <- function() {
  
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

#
#
# testIt()
# print(mydf)
