require(dplyr)
require(beepr)
require(rlang)


#  dummy assignments 4 ease frequent access

silent       <- if (exists("silent"))       silent       else FALSE
fulldata     <- if (exists("fulldata"))     fulldata     else FALSE
keypressWait <- if (exists("keypressWait")) keypressWait else FALSE

strict <- function() FALSE


  
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


SERIAL_PREFIX <- "" # "SERIALIZATION_"


# ---------- mapping terms, for internal purposes --------------------

# .. prefix: private fields naming convention

# empty private data.frame
..map_values = data.frame(
  acronym = character(0)
  ,full = character(0)
  , stringsAsFactors = F
)
# add rows to empty data frame
..map_values <- bind_rows(..map_values,
  c(acronym = LNG_DE, full = "German")
 ,c(acronym = LNG_EN,full = "English")
 ,c(acronym = LNG_FI,full = "Finnish")
 ,c(acronym = LNG_RU,full = "Russian")
 )
# function that returns the word for the acronym
map_acro <- function(x) ..map_values[
  ..map_values$acronym == char_toupper(x),2]



# --------------------------------------------------------------------
#            Define Global Variables
# --------------------------------------------------------------------
# yes, globals  are evil 99% of the cases, these are the few that 
# fall in the 1%, for my current goals.
#
# mostly to get a NULL (easily mangeable) instead of a non-existing 
# variable (more laborious to manage)

# --------------------------------------------------------------------
use_full_corpus <- function(vPar = NULL, reinit_funct = NULL 
  ,...)
# --------------------------------------------------------------------
#' @description change usage of data, full or partial, and reinit
#' @param 
#' @usage 
#' @return 
{
  if (missing(vPar) || is.null(vPar)) 
    return(fulldata) 

  previous <- fulldata
  fulldata <<- vPar # update global var 
  if (previous != fulldata) {
    qc_full <<- NULL
    dfm_full <<- NULL
    invisible(gc())
    prt("fulldata:", previous,"->",fulldata)
    if (!is.null(reinit_funct)) {
      prt("############ Reinitializing from <",previous,"> ################")
      args <- list(...)
      do.call(reinit_funct,args)
      prt("############ Reinitialized to <",fulldata,"> #################")
    }
  }
  invisible(fulldata)
}


# ---------------------------------------------------------
  data_type_prefix <- function() 
# ---------------------------------------------------------
if (all(exists("fulldata"), !is.null(fulldata),fulldata)) "full" else "subset"


# qc: quanteda corpus
qc_full <- if (exists("qc_full")) qc_full else NULL
qc_subs <- if (exists("qc_subs")) qc_full else NULL
qc_auto <- function() {
    if (use_full_corpus()) qc_full else qc_subs
}

dfm_full <- if (exists("dfm_full")) dfm_full else NULL
dfm_subs <- if (exists("dfm_subs")) dfm_subs else NULL
dfm_auto <- function() {
    if (use_full_corpus()) dfm_full else dfm_subs
}


# --------------------------------------------------------------------
#                         data directories
# --------------------------------------------------------------------


dev_data_dir <- function() {
  
  host <- Sys.info()["nodename"]
  
  if ( host == "VT-VIALIEN") {
    file.path("C:","Users","vialien","Documents","00_ev_data","08_dev","dev_data")
  } else if ( host == "LTPGJSDPX1") {
    "C:\\Users\\e_viali\\Documents\\dev\\ITAUR"
  } else if (host == "DESKTOP-B40LLN4" ) {
    file.path("C:","Users","enrico","Documents","dev","dev_data")
  } else if (host == "THPAD-W530") {
    "V:\\data\\pers_dev\\data_dev"
  } else if (host == "enrico-ThinkPad-W530") {  
    file.path("/","media","enrico","usbdata","data","pers_dev","data_dev")
  } else {
      stop(paste("dummy data_dir host non trovato per host",host))
  }
} 


# superdir= ".."
data_dir <- dev_data_dir();
dir.exists(data_dir)
data_dir_cap <- file.path(data_dir,"capstone_data")
dir.exists(data_dir_cap)


data_dir_corpus_full <-   file.path(data_dir_cap,"data_in","corpus_full")
data_dir_corpus_subset <- file.path(data_dir_cap,"data_in","corpus_subset")

data_dir_corpus_in <- function() {
  full <- use_full_corpus() 
  
  if (is.null(full)) {
    m <- "you must initialize the variable fulldata, it is NULL"
    prt_error(m) # in case I use it as brakpoint
    stop("you must initialize the variable fulldata, it is NULL")
  }

  if (full) 
      data_dir_corpus_full 
  else
    data_dir_corpus_subset
}


#-------------------------------------------------------------------------
  read_dir <- function() 
# ------------------------------------------------------------------------
{
  val <- use_full_corpus()
  if (val) 
    data_dir_corpus_full 
  else 
    data_dir_corpus_subset
}


# -------------------------------------------------------------------------
  itaur_dir <- function() 
# -------------------------------------------------------------------------
{
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
  setCoreDF <- function(std_df, lng, ttype, name_col, value, set_it) 
#---------------------------------------------------------------------
# service function to easily update df based on  language column
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





###########################################################
# HERE TEMPORARILY OPY TEST FUNCTIONS FOR EASY DEBUG
###########################################################

