#------------------------------------------------------------------------------#
# PROJECT:          COVID Webscraping and Social Media Analysis
# COUNTRY:                      BRAZIL 
#------------------------------------------------------------------------------#
# 
# Outline:
#   1.  Settings
#       1.1. Load packages
#       1.2. File paths
#------------------------------------------------------------------------------#
# 1. SETTINGS ----
#------------------------------------------------------------------------------#
run_codes <- FALSE
#------------------------------------------------------------------------------#
#   1.1. Packages ----
#------------------------------------------------------------------------------#
if (!require("pacman")) install.packages("pacman")

pacman::p_load(gtrendsR, tidyverse, parallel, pbmcapply, ggplot2, scales,
               widyr, ggraph, igraph, jsonlite, stringr, raster, stringi, 
               lubridate, purrr, lexiconPT, tidytext, quanteda, qdap, 
               SentimentAnalysis, sentimentr, patchwork, tm, tokenizers, 
               wordcloud, ggwordcloud, ggpubr, hrbrthemes)

#------------------------------------------------------------------------------#
#   1.2. File paths ----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#   1.2.1. Main paths
#------------------------------------------------------------------------------#

#   User: Rob Marty 
#------------------------------------------------------------------------------#
#   World Bank computer
if (Sys.getenv("USERNAME") == "WB521633") {
    dropbox_file_path       <- "C:/Users/wb521633/Dropbox/World Bank/Side Work/COVID Social Media Analysis"
    covid_twitter_github    <- "C:/Users/wb521633/Documents/Github/COVID-19-TweetIDs"
}
#   Personal computer
if (Sys.getenv("USERNAME") == "robmarty") {
    dropbox_file_path       <- "~/Dropbox/World Bank/Side Work/COVID Social Media Analysis"
    covid_twitter_github    <- "~/Documents/Github/COVID-19-TweetIDs"
}

#   User: Rony Rodrigo Maximiliano Rodriguez Ramirez
#------------------------------------------------------------------------------#
if (Sys.getenv("USERNAME") == "maximiliano") {
    dropbox_file_path       <- file.path("D:/Dropbox/COVID Social Media Analysis")
    github_file_path        <- file.path("D:/Documents/RA Jobs/DIME/github/covid-task-force/covid-social-media-analysis")
    covid_twitter_github    <- file.path("D:/Documents/RA Jobs/DIME/github/covid-task-force/COVID-19-TweetIDs")
}
#------------------------------------------------------------------------------#
#   1.2.2. Other paths
#------------------------------------------------------------------------------#
brazil_twitter_figures_path     <- file.path(dropbox_file_path, "Data", "twitter", "Outputs", "figures")
brazil_gmobility_figures_path   <- file.path(dropbox_file_path, "Data", "google_mobility", "Outputs", "figures")

#------------------------------------------------------------------------------#
# 2. Cleaning ----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 3. Analysis ----
#------------------------------------------------------------------------------#