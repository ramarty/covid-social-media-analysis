# COVID Webscraping and Social Media Analysis

# Packages ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(gtrendsR, tidyverse, parallel, pbmcapply, ggplot2, scales,
               jsonlite, stringr, raster, stringi, lubridate, purrr, lexiconPT,
               tidytext, quanteda, qdap, SentimentAnalysis, sentimentr,
               tm, tokenizers, wordcloud, ggwordcloud, ggpubr, hrbrthemes)

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") dropbox_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Side Work/COVID Social Media Analysis"
if(Sys.info()[["user"]] == "robmarty") dropbox_file_path <- "~/Dropbox/World Bank/Side Work/COVID Social Media Analysis"

if(Sys.info()[["user"]] == "WB521633") covid_twitter_github <- "C:/Users/wb521633/Documents/Github/COVID-19-TweetIDs"
if(Sys.info()[["user"]] == "robmarty") covid_twitter_github <- "~/Documents/Github/COVID-19-TweetIDs"

if (Sys.getenv("USERNAME") == "maximiliano") {
    dropbox_file_path       <- file.path("D:/Dropbox/COVID Social Media Analysis")
    github_file_path        <- file.path("D:/Documents/RA Jobs/DIME/github/covid-task-force/covid-social-media-analysis")
    covid_twitter_github    <- file.path("D:/Documents/RA Jobs/DIME/github/covid-task-force/COVID-19-TweetIDs")
}

brazil_twitter_figures_path <- file.path(dropbox_file_path, "Data", "twitter", "Outputs", "figures")
