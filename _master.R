# COVID Webscraping and Social Media Analysis

# Packages ---------------------------------------------------------------------
library(gtrendsR)
library(parallel)
library(pbmcapply)
library(ggplot2)
library(jsonlite)
library(stringr)
library(raster)
library(ISOcodes)
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
library(dplyr)
library(sf)
library(geofacet)
library(readstata13)
library(strucchange)
library(forcats)
library(ISOcodes)
library(ggwordcloud)
library(hrbrthemes)
library(lexiconPT)
library(textdata)
library(tidyr)
library(rgeos)
library(dplyr)

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") dropbox_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Side Work/COVID Social Media Analysis"
if(Sys.info()[["user"]] == "robmarty") dropbox_file_path <- "~/Dropbox/World Bank/Side Work/COVID Social Media Analysis"

if(Sys.info()[["user"]] == "wb537287") dropbox_file_path <- "/Users/wb537287/Dropbox/COVID Social Media Analysis"
if(Sys.info()[["user"]] == "manuelramos") dropbox_file_path <- "~/Dropbox/COVID Social Media Analysis"

if(Sys.info()[["user"]] == "WB521633") covid_twitter_github <- "C:/Users/wb521633/Documents/Github/COVID-19-TweetIDs"
if(Sys.info()[["user"]] == "robmarty") covid_twitter_github <- "~/Documents/Github/COVID-19-TweetIDs"

brazil_twitter_figures_path <- file.path(dropbox_file_path, "Data", "twitter", "Outputs", "figures")

google_figures_path <- file.path(dropbox_file_path, "Data", "google_trends", "outputs", "figures")

