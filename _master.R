# COVID Webscraping and Social Media Analysis

# Packages ---------------------------------------------------------------------
library(gtrendsR)
library(tidyverse)
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
library(tidytext)
library(quanteda)
library(qdap)
library(SentimentAnalysis)
library(sentimentr)
library(tm)
library(tokenizers)
library(wordcloud)
library(ggwordcloud)
library(ggpubr)
library(hrbrthemes)
library(scales)

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") dropbox_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Side Work/COVID Social Media Analysis"
if(Sys.info()[["user"]] == "robmarty") dropbox_file_path <- "~/Dropbox/World Bank/Side Work/COVID Social Media Analysis"

if(Sys.info()[["user"]] == "WB521633") covid_twitter_github <- "C:/Users/wb521633/Documents/Github/COVID-19-TweetIDs"
if(Sys.info()[["user"]] == "robmarty") covid_twitter_github <- "~/Documents/Github/COVID-19-TweetIDs"

if (Sys.getenv("USERNAME") == "maximiliano") {
    dropbox_file_path       <- file.path("D:/Dropbox/COVID Social Media Analysis")
    github_file_path        <- file.path("D:/Documents/RA Jobs/DIME/github/covid-social-media-analysis")
    covid_twitter_github    <- file.path("D:/Documents/RA Jobs/DIME/github/COVID-19-TweetIDs")
}

brazil_twitter_figures_path <- file.path(dropbox_file_path, "Data", "twitter", "Outputs", "figures")
