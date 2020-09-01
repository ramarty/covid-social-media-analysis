# COVID Webscraping and Social Media Analysis

# Packages ---------------------------------------------------------------------
library(gtrendsR)
library(parallel)
library(pbmcapply)
library(ggplot2)
library(jsonlite)
library(stringr)
library(raster)
library(scales)
library(sparkline)
library(data.table)
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
library(tidylog)
library(TTR)
library(sparkline)
library(shinydashboard)
library(RColorBrewer)
library(shinythemes)
library(DT)
library(dplyr)
library(rmarkdown)
library(lubridate)
library(shiny)
library(wesanderson)
library(ggplot2)
library(tidyr)
library(shinyWidgets)
library(zoo)
library(bcrypt)
library(shinyjs)
library(ngram)
library(rtweet)
library(stringdist)
library(stringr)
library(rgdal)
library(rgeos)
library(geosphere)
library(htmlwidgets)
library(tidyverse)
library(sf)
library(tidyverse)
library(raster)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(data.table)
library(formattable)
library(tidyr)
library(viridis)
library(data.table)
library(raster)
library(htmltools)
library(scales)
library(lubridate)
library(geosphere)
library(hrbrthemes)
# remotes::install_github("wilkelab/ggtext")
library(ggtext)

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




