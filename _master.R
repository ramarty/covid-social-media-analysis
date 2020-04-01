# COVID Webscraping and Social Media Analysis

# Packages ---------------------------------------------------------------------
library(gtrendsR)
library(dplyr)
library(parallel)
library(pbmcapply)
library(ggplot2)

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") dropbox_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Side Work/COVID Social Media Analysis"
if(Sys.info()[["user"]] == "robmarty") dropbox_file_path <- "~/Dropbox/World Bank/Side Work/COVID Social Media Analysis"

