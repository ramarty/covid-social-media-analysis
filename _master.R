# COVID Webscraping and Social Media Analysis

# Packages ---------------------------------------------------------------------
## Install/Load Package Dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(gtrendsR, countrycode, parallel, pbmcapply, ggplot2, jsonlite,
               stringr, raster, scales, rmapshaper, sparkline, magick, magrittr,
               htmltools, data.table, plotly, ISOcodes, stringi, lubridate,
               purrr, tidytext, quanteda, qdap, SentimentAnalysis, sentimentr,
               tm, tokenizers, wordcloud, ggwordcloud, ggpubr, dplyr, sf,
               geofacet, readstata13, strucchange, forcats, ISOcodes, hrbrthemes,
               lexiconPT, textdata, tidyr, rgeos, tidylog, TTR, sparkline,
               shinydashboard, RColorBrewer, shinythemes, DT, rmarkdown, shiny,
               wesanderson, shinyWidgets, zoo, bcrypt, shinyjs, ngram, rtweet,
               stringdist, stringr, rgdal, rgeos, geosphere, htmlwidgets,
               tidyverse, sf, raster, leaflet, leaflet.extras, plotly,
               geosphere, data.table, formattable, tidyr, viridis, data.table,
               WDI, scales, rnaturalearth, sp)

# remotes::install_github("wilkelab/ggtext")
# library(ggtext)

source("https://raw.githubusercontent.com/ramarty/r_google_translate/main/r_google_translate.R")

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") dropbox_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Side Work/COVID Social Media Analysis"
if(Sys.info()[["user"]] == "robmarty") dropbox_file_path <- "~/Dropbox/World Bank/Side Work/COVID Social Media Analysis"

if(Sys.info()[["user"]] == "wb537287") dropbox_file_path <- "/Users/wb537287/Dropbox/COVID Social Media Analysis"
if(Sys.info()[["user"]] == "manuelramos") dropbox_file_path <- "~/Dropbox/COVID Social Media Analysis"

if(Sys.info()[["user"]] == "WB521633") github_file_path <- "C:/Users/wb521633/Documents/Github/covid-social-media-analysis"
if(Sys.info()[["user"]] == "robmarty") github_file_path <- "~/Documents/Github/covid-social-media-analysis"

if(Sys.info()[["user"]] == "WB521633") covid_twitter_github <- "C:/Users/wb521633/Documents/Github/COVID-19-TweetIDs"
if(Sys.info()[["user"]] == "robmarty") covid_twitter_github <- "~/Documents/Github/COVID-19-TweetIDs"

brazil_twitter_figures_path <- file.path(dropbox_file_path, "Data", "twitter", "Outputs", "figures")

google_figures_path <- file.path(dropbox_file_path, "Data", "google_trends", "outputs", "figures")

if(Sys.info()[["user"]] == "robmarty"){
  paper_tables <- file.path("~/Dropbox/Apps/Overleaf/COVID-19 and Google Trends Paper/tables")
  paper_figures <- file.path("~/Dropbox/Apps/Overleaf/COVID-19 and Google Trends Paper/figures")
}


