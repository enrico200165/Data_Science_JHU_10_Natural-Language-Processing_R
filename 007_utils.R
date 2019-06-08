require(ggplot2)
require(quanteda)
# help(package = "quanteda")

#install.packages("readtext")
require(readtext)

require(spacyr)

source("006_globals.R")


# ---------------------------------------------------------
gen_strong_var <- function(init_val = NULL) 
# ---------------------------------------------------------
#' @description PROBABLY TOTALLY USELESS
#' @param initial value to set it to
#' @usage myVar <- gen_strong_var(); myVar(99); print(myVar)
#' @return the closure value
{
strict2_ <- init_val
function(val = NULL) {
  if (is.null(val)) strict2_ else { strict2_ <<- val; strict2_}
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
# Not terribly necessary :-)


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
  
  rds_fname <- gsub("\\[", "_", rds_fname)
  rds_fname <- gsub("\\]", "_", rds_fname)
  
  rds_fname
}


#---------------------------------------------------------------------
serializeIfNeeded <- function(dfPar, forceIt = FALSE, rdsFName) 
#---------------------------------------------------------------------
{
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


# #---------------------------------------------------------------------
#   readIfEmpty <- function(df, rdsFName, forceSerialization = FALSE) 
# #---------------------------------------------------------------------
# # probably should be rewritten (to use a better subfunction I will
# # probably write soon)
# # return:
# # TRUE if it was or has bee filled
# {
#   ret <- FALSE
#    
#   varName <- deparse(substitute(df))
#   rdsFName <- getSerializFName(varName,rdsFName)
#   
#   if (!exists(varName) || is.null(df) || (length(df) <= 0 && nrow(df) <= 0)) {
#     prt(paste(varName,"is empty", rdsFName))
#     if (file.exists(rdsFName)) {
#       # print(paste("reading serialization for:",varName," file:", rdsFName))
#       df <- readRDS(rdsFName)
#       #assign(varName,df,.GlobalEnv) # pass it outside
#       assign(varName,df,parent.frame(n = 1)) # pass it outside
#       ret <- TRUE
#     } else {
#       print(paste(varName,"cannot read, no file: ", rdsFName))
#       ret <- FALSE
#     }
#   } else {
#     prt(paste(varName,"alread filled, size GB (rounded down):"
#       ,XiB(pryr::object_size(df))),"fulldata:",fulldata)
#     ret <- TRUE
#   }
#   # print(paste("exit",varName, ))
#   
#   if (ret) {
#     serializeIfNeeded(df, forceSerialization, rdsFName)
#   }
#   
#   ret
# }


#---------------------------------------------------------------------
rie <- function(df, force_calc, force_fname, calc_function,...) 
#---------------------------------------------------------------------
#' @description trying to develop a more automated version of readIfENpty
#' @param variable to read or calculate
#' @param force_calc calculate even if there is a file
#' @param force_name filename to use
#' @param calc_function to produce variable value if missing
#' @param arguments for calc_function
#' @return TRUE if it was or has bee filled
{
  args <- list(...)
  ret <- FALSE
  
  varName <- deparse(substitute(df))
  
  # rdsFName <- if (is.null(force_fname)) getSerializFName(varName) else getSerializFName(force_fname)
  rdsFName <- getSerializFName(varName, force_fname)
  if (any(!exists(varName), is.null(df), (length(df) <= 0 && nrow(df) <= 0))) {
    # print(paste(varName,"is empty", rdsFName))
    if (all(file.exists(rdsFName), !force_calc) ) {
      prt("reading serialization for:",varName," file:", rdsFName)
      df <- readRDS(rdsFName)
      #assign(varName,df,.GlobalEnv) # pass it outside
    } else {
      # must calculate it
      prt(varName,"no" ,rdsFName,"or forced file, calling function to calculate it: "
          ,deparse(substitute(calc_function)) )
      df <- do.call(calc_function, args) 
      prt("completed call to",deparse(substitute(calc_function)))
    }
    assign(varName, df, parent.frame(n = 1)) # pass it outside
    ret <- TRUE
  } else {
    prt(varName,"alread filled, size GiB:"
        ,GiB(pryr::object_size(df)), "size: " 
        ,XiB(pryr::object_size(df)))
    ret <- TRUE
  }
  # print(paste("exit",varName, ))
  
  # if the value exists, serialize it if needed
  if (ret) {
    serializeIfNeeded(df,,rdsFName)
  }
  
  invisible(ret)
}



#---------------------------------------------------------------------
rie_str <- function(vname, force_calc, force_fname, calc_function,...) 
  #---------------------------------------------------------------------
#' @description identical to rie but var name is not deparsed
{
  args <- list(...)
  ret <- FALSE
  
  varName <- vname
  
  # rdsFName <- if (is.null(force_fname)) getSerializFName(varName) else getSerializFName(force_fname)
  rdsFName <- getSerializFName(varName, force_fname)
  if (any(!exists(varName, where = global_env()), is.null(df), (length(df) <= 0 && nrow(df) <= 0))) {
    # print(paste(varName,"is empty", rdsFName))
    if (all(file.exists(rdsFName), !force_calc) ) {
      prt("reading serialization for:",varName," file:", rdsFName)
      df <- readRDS(rdsFName)
      #assign(varName,df,.GlobalEnv) # pass it outside
    } else {
      # must calculate it
      prt(varName,"no" ,rdsFName,"or forced file, calling function to calculate it: "
          ,deparse(substitute(calc_function)) )
      df <- do.call(calc_function, args) 
      prt("completed call to",deparse(substitute(calc_function)))
    }
    assign(varName, df, parent.frame(n = 1)) # pass it outside
    #assign(varName, df, global_env()) # pass it outside
    ret <- TRUE
  } else {
    prt(varName,"alread filled, size GiB:"
        ,GiB(pryr::object_size(df)), "size: " 
        ,XiB(pryr::object_size(df)))
    ret <- TRUE
  }
  # print(paste("exit",varName, ))
  
  # if the value exists, serialize it if needed
  if (ret) {
    serializeIfNeeded(df,,rdsFName)
  }
  
  invisible(ret)
}


# --------------------------------------------------------------------
zap_files_serializations <- function(patternPar) 
  # --------------------------------------------------------------------
{
  
  if (missing(patternPar)) patternPar <- "*txt*.rds"
  
  fnames_to_delete <- list.files(".", patternPar)
  file.remove(fnames_to_delete)
  length(fnames_to_delete)
}
# zap_files_serializations()



# --------------------------------------------------------------------
# https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved by me
# --------------------------------------------------------------------
.ls.objects <- function (pos = 1, pattern, order.by ,no_funct = T
                         ,decreasing=FALSE, head=FALSE, n=5) 
  # --------------------------------------------------------------------
{
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  
  names <- ls(pos = pos, pattern = pattern)
  
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(pryr::object_size(x), units = "auto") })
  obj.size <- napply(names, pryr::object_size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  
  if (no_funct)
    out <- out[ out$Type != "function", ]
  
  out$ID <- rownames(out)
  
  out
}

# shorthand
lsos <- function(..., n=100) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}



# --------------------------------------------------------------------
removeAllVarExcept <- function(survivors = character(0), e)
  # --------------------------------------------------------------------
#' @description removes variables from environment e
#' @param variable names NOT to be remved
#' @param environment
#' @return names of removed variables
# -------------------------------------------------------------------
{
  if (missing(e)) e <- parent.frame()
  
  names_initial <- rlang::env_names(e)
  
  victims <- setdiff(rlang::env_names(e),survivors)
  rlang::env_unbind(e, victims)
  
  names_removed <- setdiff(names_initial, rlang::env_names(e))
}


# --------------------------------------------------------------------
kill_var <- function (variablesPar, serializ = F)
  # --------------------------------------------------------------------
{
  varName <- deparse(substitute(variablesPar))
  
  var_size <- if (exists(varName)) pryr::object_size(variablesPar) else 0
  
  # rm(list = varName, pos = ".GlobalEnv")
  if (exists(varName)) {
    rm(list = varName, pos =  .GlobalEnv)
    if (var_size >= 1024*1024) { gc() }
  } else {
    prt_warn("killing variable:" ,varName , "does NOT exist")
  }
  
  
  if (serializ) {
    fname <- getSerializFName(varName)
    if (serializ && file.exists(fname)) {
      ret <- file.remove(fname)
    }
  }
}


# --------------------------------------------------------------------
mem_health <- function(survivors = character(0) ,dry_run = F 
                       ,save_size = -1 ,verbose = F)
  # --------------------------------------------------------------------
# named by var ID array of sizes of existing variables
{
  
  if (dry_run) verbose <- T
  
  gc_first <- gc()
  
  all_vars_df <- lsos()
  
  # remove local, should be unnecessary
  local_vars <- ls()
  delete_vars_df <- all_vars_df[!(all_vars_df$ID %in% local_vars), ]
  
  # variables to save
  delete_vars_df <- all_vars_df[!(all_vars_df$ID %in% survivors), ]
  # don't remove small variables
  delete_vars_df <- all_vars_df[delete_vars_df$size > save_size, ]
  
  
  wrong_vars <- setdiff(survivors,all_vars_df$ID)
  if (length(wrong_vars) > 0) {
    prt("ERRORS: saving non existing vars",paste(wrong_vars))
  }
  
  mem_delete_vars <- sum(delete_vars_df$Size)
  
  if (!is.null(verbose) && verbose) {
    prt("deleting", length(delete_vars_df$ID),"vars:",paste(delete_vars_df$ID))
    prt(" sparing", length(survivors),"vars:", paste(survivors))
  }
  
  prt("Delete vars tot. mem:" ,XiB(mem_delete_vars))
  if (!dry_run) {
    do.call(rm, as.list(delete_vars_df$ID))
  }
  
  (gc() - gc_first)
}
# silent <- F
# x <- mem_health(c("fulldata"), dry_run = F)



# --------------------------------------------------------------------
MiB <- function (x, digits = 2) round(x/(2^20),digits)
# --------------------------------------------------------------------
# --------------------------------------------------------------------
GiB <- function (x, digits = 2)   round(x/(2^30),digits)
# --------------------------------------------------------------------
# --------------------------------------------------------------------
XiB <- function (x, digits = 2, separ = "_")
  # --------------------------------------------------------------------
# for printing
{
  unit <- "b"
  
  if (x < 2^(10) )   return(paste(x,"b"))
  if (x < 2^(10*2) ) return(paste(round(x/2^10    ,digits),"KiB",sep = separ))
  if (x < 2^(10*3) ) return(paste(round(x/2^(10*2),digits),"MiB",sep = separ))
  
  return(paste(round(x/2^(10*3),digits),"GiB",sep = separ))
}


# --------------------------------------------------------------------
keypress <- function (message = "Press [enter] to continue"
                      , sound_nr = 1)
  # --------------------------------------------------------------------
{
  beep(sound = sound_nr, expr = NULL)
  
  if (keypressWait) {
    invisible(readline(prompt=message))
  } else {
    prt(message)
    Sys.sleep(5)
  }
  
  invisible(T)
}




# --------------------------------------------------------------------
clean_rds <- function(regex_to_remove = NULL, ser_prefix = data_type_prefix())
# --------------------------------------------------------------------
{
  if (is.null(regex_to_remove)) {
    prt_warn("asked to remove nothing")
    return(F)
  }
  patt <- paste0(ser_prefix,".*?",regex_to_remove,".*?","\\.rds")
   
  prt("pattern", patt, " removing: ", paste(list.files(".",patt),collapse = " "))
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
    time_diff <- as.numeric(difftime(Sys.time(), prt_last_call_time))
    time_diff <- paste(round(time_diff, 2), "secs -")
    time_diff <- paste(format(Sys.time(), "%X"), time_diff)
    
    pars <- list(...)
    print(paste(time_diff, paste(pars, collapse = " ")))
    prt_last_call_time <<- Sys.time()
  }
}

# --------------------------------------------------------------------
prt_warn <- function(...) do.call(prt,prepend(list(...),"# WARNING:"))
prt_error <- function(...) do.call(prt,prepend(list(...),"### ERROR:"))


# --------------------------------------------------------------------
coverage_of_freq_list <- function(frq_vect, qtiles_vec, size = 0)
# --------------------------------------------------------------------
{
  
  stopifnot(!is.null(frq_vect) && exists("frq_vect"))
  stopifnot(!is.null(qtiles_vec) && exists("qtiles_vec"))
  
  # naive attempt to avoid underflow
  molt <- 100 * 1000
  
  # get proportions
  props <- frq_vect*molt/sum(frq_vect);
  stopifnot(!strict() || round(sum(props)-(1*molt),5) == 0)
  # cumulative
  cumul <- cumsum(props)
  
  
  idxs <- numeric(length =length(qtiles_vec))
  pcts <- numeric(length =length(qtiles_vec))
  frqs <- numeric(length =length(qtiles_vec))
  sizs <- numeric(length =length(qtiles_vec))
  
  for (i in seq_along(qtiles_vec)) {
    
    qtile <- qtiles_vec[i]
    idx_set <- which(cumul >= qtile*molt)
    if (length(idx_set) <= 0) {
      prt_warn("unable to find quantile")
      prt_warn("freq vector:", frq_vect)
      stop()
    }
    idxs[i] <- idx_set[1]
    pcts[i] <- idxs[i]/length(frq_vect)
    frqs[i] <- frq_vect[idx_set[1]]
    sizs[i] <- XiB(pcts[i]*size)
    
    #prt(qtile,"somma:",round(sum(props[1:idxs[i]])/molt ,2))
    #print("")
  }
  
  list(
    qtls = qtiles_vec
    ,idxs = idxs
    ,pcts = pcts
    ,frqs = frqs
    ,sizs = sizs)
}


###########################################################
#                      TEMP TEST
###########################################################
test_rie <- function() {
  
  
  myvec <- LETTERS
  if (!rie(myvec[5], F, , function() myvec[5])) {
    print("ERROR I did not read it")
  }
  str(myvec)
  
  
  mydf <- data.frame(10:1)
  serializeIfNeeded(mydf,TRUE)
  
  # check if it serializes correctly
  if (rie(mydf, T, ,function() data.frame(10:1))) {
    print("ERROR I did not read it")
  }
  
  mydf <<- NULL
  if (!rie(mydf, F,"forcingit", function() data.frame(10:1))) {
    print("ERROR I did not read it")
  }
  str(mydf)
  
}
# 
# test_rie()
