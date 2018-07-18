require(dplyr)
require(beepr)

# illegal position, ease frequent access

silent <- NULL
fulldata <- NULL

# --------------------------------------------------------------------
use_full_corpus <- function(vPar, reinit_funct = NULL 
  ,...)
# --------------------------------------------------------------------
{

  if (missing(vPar) || is.null(vPar)) 
    return(fulldata) 

  previous <- fulldata
  fulldata <<- vPar
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

..map_values = data.frame(
  acronym = character(0)
  ,full = character(0)
  , stringsAsFactors = F
)
..map_values <- bind_rows(..map_values,
  c(acronym = LNG_DE, full = "German")
 ,c(acronym = LNG_EN,full = "English")
 ,c(acronym = LNG_FI,full = "Finnish")
 ,c(acronym = LNG_RU,full = "Russian")
  
 )
map_acro <- function(x) ..map_values[
  ..map_values$acronym == char_toupper(x),2]
#map_acro(LNG_EN )



# --------------------------------------------------------------------
#            Define Global Variables
# --------------------------------------------------------------------
# yes, globals  are evil 99% of the cases, these are the few that 
# fall in the 1%, for my current goals.
#
# mostly to get a NULL (easily mangeable) instead of a non-existing 
# variable (more laborious to manage)



data_type_prefix <- function() if (fulldata) "full" else "subs"



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
data_dir_corpus_in <- function() {
    if (use_full_corpus) data_dir_corpus_full else data_dir_corpus_subset
}


read_dir <- function() {
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



# --------------------------------------------------------------------
  set_parallelism <- function(ncores, onLinux)
# --------------------------------------------------------------------
# stub to be developed later when implementing predictions
{
  require(parallel)
  require(doParallel)
  
  ncores <- 6 # detectCores() - 1
  # Initiate cluster
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
}


# --------------------------------------------------------------------
  inc <- function(e1) eval.parent(substitute(e1 <- e1+1))
# --------------------------------------------------------------------
# ot terribly necessary :-)


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


#---------------------------------------------------------------------
getSerializFName <- function(var_id, force_name) 
#---------------------------------------------------------------------
{
  if (missing(force_name) || is.null(force_name) || nchar(force_name) <= 0) {
    stopifnot(!is.null(var_id) && nchar(var_id) > 0)
    rds_fname <- var_id
  } else  {
    rds_fname <- force_name
  } 
  
  if (nchar(SERIAL_PREFIX) > 0 && !grepl(SERIAL_PREFIX, rds_fname))
    rds_fname <- paste0(SERIAL_PREFIX,rds_fname)
  
  rds_fname <- paste0(data_type_prefix(),"_",rds_fname,".rds") 
}


#---------------------------------------------------------------------
  serializeIfNeeded <- function(dfPar, forceIt, rdsFName) 
#---------------------------------------------------------------------
{
  if (missing(forceIt)) forceIt <- FALSE
  
  varName <- deparse(substitute(dfPar))
  if (missing(rdsFName) || is.null(rdsFName) || nchar(rdsFName) <= 0)
    rdsFName <- getSerializFName(varName)
  
  if (grepl("full_full",rdsFName)) {
    printf("asino")
  }
  
  if (!file.exists(rdsFName) || forceIt) {
    # if(exists(varName)) {
    if(TRUE) {
      # 
      prt(paste("serializing var:",varName,"to file:",rdsFName,"..."))
      saveRDS(dfPar, file = rdsFName)
      prt(paste("FINISHED serializing",varName))
    } else {
      # print(paste("not serializing because not exists var: ",varName))
    }
  } else {
    dummy <- 3 # just to debug
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
  if (missing(forceIt)) forceIt <- FALSE
 
  ret <- FALSE
   
  varName <- deparse(substitute(df))
  rdsFName <- getSerializFName(varName,rdsFName)
  
  if (!exists(varName)
      || is.null(df)
      || (length(df) <= 0 && nrow(df) <= 0)) {
    prt(paste(varName,"is empty", rdsFName))
    if (file.exists(rdsFName)) {
      # print(paste("reading serialization for:",varName," file:", rdsFName))
      df <- readRDS(rdsFName)
      #assign(varName,df,.GlobalEnv) # pass it outside
      assign(varName,df,parent.frame(n = 1)) # pass it outside
      ret <- TRUE
    } else {
      print(paste(varName,"cannot read, no file: ", rdsFName))
      ret <- FALSE
    }
  } else {
    prt(paste(varName,"alread filled, size:"
      ,GiB(pryr::object_size(df))),"fulldata:",fulldata)
    ret <- TRUE
  }
  # print(paste("exit",varName, ))
  
  if (ret) {
    serializeIfNeeded(df,forceIt,rdsFName)
  }
  
  ret
}
# pippo <- 1:10
# serializeIfNeeded(pippo)
# pippo <- NULL
# readIfEmpty(pippo)


#---------------------------------------------------------------------
  rie <- function(df ,calc_function ,...) 
#---------------------------------------------------------------------
# trying to develop a more automated version of readIfENpty
# TRUE if it was or has bee filled
{
  
  args <- list(...)
  
  ret <- FALSE
   
  varName <- deparse(substitute(df))
  rdsFName <- getSerializFName(varName)
  
  if (!exists(varName)
      || is.null(df)
      || (length(df) <= 0 && nrow(df) <= 0)) {
    # print(paste(varName,"is empty", rdsFName))
    if (file.exists(rdsFName)) {
      # print(paste("reading serialization for:",varName," file:", rdsFName))
      df <- readRDS(rdsFName)
      #assign(varName,df,.GlobalEnv) # pass it outside
    } else {
      # must calculate it
      prt(varName,"no" ,rdsFName,"file, calling function to calculate it: "
        ,deparse(substitute(calc_function)) )
      df <- do.call(calc_function, args) 
      prt("completed call to",deparse(substitute(calc_function)))
    }
    assign(varName,df,parent.frame(n = 1)) # pass it outside
    ret <- TRUE
  } else {
    prt(varName,"alread filled, size GiB:"
      ,GiB(pryr::object_size(df)), "size: ",pryr::object_size(df))
    ret <- TRUE
  }
  # print(paste("exit",varName, ))
  
  if (ret) {
    serializeIfNeeded(df,,rdsFName)
  }
  
  invisible(ret)
}


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
  kill_var <- function (variablesPar, serializ)
# --------------------------------------------------------------------
{
  if (missing(serializ)) serializ <- TRUE

  e <- parent.frame()
  varName <- deparse(substitute(variablesPar))
  if ( exists(varName,e)) {
    rlang::env_unbind(e, varName)
    stopifnot(!exists(varName,e))
  } else {
    if (exists(varName,.GlobalEnv))
      rlang::env_unbind(.GlobalEnv, varName)
      stopifnot(!exists(varName,.GlobalEnv))
  }
  
  fname <- getSerializFName(varName)
  if (serializ && file.exists(fname)) {
    ret <- file.remove(fname)
  }
}

# kill_var(freq_of_freq_plots)
# --------------------------------------------------------------------
  GiB <- function (x, digits)
# --------------------------------------------------------------------
{
  if (missing(digits))
    digits <- 2
  round(x/(2^30),digits)
}


# --------------------------------------------------------------------
keypress <- function (message, sound_nr = 1)
# --------------------------------------------------------------------
{
  beep(sound = sound_nr, expr = NULL)
  Sys.sleep(1)
  if (!silent) {
  if (missing(message)) message <- "Press [enter] to continue"
    invisible(readline(prompt=message))
  # prt(message)
  }
  invisible(T)
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


prt_last_call_time <- Sys.time()
# --------------------------------------------------------------------
  prt <- function(...) 
# --------------------------------------------------------------------
{
  if (silent) {
    invisible("")
  } else {
  time_diff <- Sys.time() - prt_last_call_time
  pars <- list(...)
  print(paste(time_diff,paste(pars,collapse = " ")))
  prt_last_call_time <<- Sys.time()
  }
}
  test_prt <- function() 
{
  # check if it serializes correctly
  prt("pippo", "pluto")
  Sys.sleep(1)
  prt("")
  Sys.sleep(2)
  prt()
  Sys.sleep(5)
  prt()
}
# test_prt()



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



  
  test_rie <- function() 
{
  # check if it serializes correctly
  mydf <- data.frame(x = 1:3)
  rie(mydf, function() data.frame(x = 1:3))

  mydf <<- NULL
  rie(mydf, function() data.frame(x = 1:3))
  print(mydf)
}


    

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

# -----------------------------------------------------------------------
  test_Globals.R <- function() 
# -----------------------------------------------------------------------
  
{
  # testRemoveAllVarExcept()
  # testReadIfEmpty()
  test_rie()
  # testAddToCoreDF()
  # test_getSerializeFName()
  
}

#   test_Globals.R()
