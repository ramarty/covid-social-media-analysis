# COVID Webscraping and Social Media Analysis

# Packages ---------------------------------------------------------------------
library(gtrendsR)
library(dplyr)
library(parallel)
library(pbmcapply)
library(ggplot2)
library(jsonlite)
library(stringr)
library(raster)
library(stringi)
library(lubridate)
library(purrr)

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") dropbox_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Side Work/COVID Social Media Analysis"
if(Sys.info()[["user"]] == "robmarty") dropbox_file_path <- "~/Dropbox/World Bank/Side Work/COVID Social Media Analysis"

if(Sys.info()[["user"]] == "WB521633") covid_twitter_github <- "C:/Users/wb521633/Documents/Github/COVID-19-TweetIDs"
if(Sys.info()[["user"]] == "robmarty") covid_twitter_github <- "~/Documents/Github/COVID-19-TweetIDs"
