# Title     : TODO
# Objective : TODO
# Created by: enrico
# Created on: 02/04/2018
require(ggplot2)
require(quanteda)
# help(package = "quanteda")

#install.packages("readtext")
require(readtext)

#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
#install.packages("spacyr")
require(spacyr)

source("01_globals.R")



# -------- TEST -----

# list.files(data_dir_corpus)
# cap_corp <- paste0(data_dir_corpus,"/en*.txt")

#data_dir <- system.file("extdata/", package = "readtext")
# list.files(data_dir)

#evdata_dir <- file.path("../../..", "capstone_data",fsep = .Platform$file.sep)
#
# list.files(evdata_dir)



print("OK")
