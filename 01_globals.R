

db_fname <- "words_en.sqlite"

SERIAL_PREFIX <- "SERIALIZATION_"

inc <- function(e1) eval.parent(substitute(e1 <- e1+1))

# --- data directories ---
superdir= ".."
data_dir_start <- file.path(superdir,superdir,superdir,superdir,superdir,"data_dev")
getDataDir <- function(ddir) {
  while (nchar(ddir) > 4 & !dir.exists(ddir) & grepl(paste0(superdir,"[\\/]"),ddir)) {
    ddir <- substring(ddir,nchar(superdir)+2, nchar(ddir))
    # print(ddir)
  }
  if(dir.exists(ddir)) {
    return(ddir)
  } else {
    return(NA)
  }
}
data_dir <- getDataDir(data_dir_start)
data_dir_cap <- file.path(data_dir,"capstone_data")
data_dir_corpus <- file.path(data_dir_cap,"data_in/corpus/")


# --- Corpuses ---
# qc: quanteda corpus

if (!exists("qc_news")) qc_news <- NA
if (!exists("qc_blogs")) qc_blogs <- NA
if (!exists("qc_twitts")) qc_twitts <- NA


# ------------------------------------------
#    Tests
# ------------------------------------------
# print(getDataDir(data_dir))