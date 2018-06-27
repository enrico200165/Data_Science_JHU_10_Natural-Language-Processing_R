
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
  } else if (Sys.info()["nodename"] == "asus") {
    "asusdir"
  } else if (Sys.info()["nodename"] == "THPAD-W530") {
    "V:\\data\\pers_dev\\data_dev"
  } else {
    NA
  }
}


# superdir= ".."
data_dir <- dev_data_dir();
dir.exists(data_dir)
data_dir_cap <- file.path(data_dir,"capstone_data")
dir.exists(data_dir_cap)
data_dir_corpus_in <- file.path(data_dir_cap,"data_in/corpus/")
data_dir_corpus_work <- file.path(data_dir_cap,"data_work/corpus/")
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


# --- Corpuses ---
# qc: quanteda corpus

if (!exists("qc_news")) qc_news <- NA
if (!exists("qc_blogs")) qc_blogs <- NA
if (!exists("qc_twitts")) qc_twitts <- NA


# ------------------------------------------
#    Tests
# ------------------------------------------
# print(getDataDir(data_dir))