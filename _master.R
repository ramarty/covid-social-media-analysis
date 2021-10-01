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
               WDI, scales, rnaturalearth, sp, utf8, ggtext, stargazer, lfe,
               ggrepel, Rfast, tikzDevice)

## User defined functions
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

if(Sys.info()[["user"]] == "robmarty"){
  paper_tables <- file.path("~/Dropbox/Apps/Overleaf/COVID-19 and Google Trends Paper/tables")
  paper_figures <- file.path("~/Dropbox/Apps/Overleaf/COVID-19 and Google Trends Paper/figures")
}

data_dir <- file.path(dropbox_file_path, "Data")
who_covid_dir <- file.path(data_dir, "who_covid")
gtrends_dir <- file.path(data_dir, "google_trends")
vaccine_dir <- file.path(data_dir, "usa_vaccine")
oxpol_dir <- file.path(data_dir, "oxford_covid_policy_tracker")

# Google API Key ---------------------------------------------------------------
# Only needed for Google translation
api_key <- read_csv(file.path("~", "Dropbox", "World Bank", "Webscraping", "Files for Server", 
                              "api_keys.csv")) %>%
  filter(Account %in% "robertandrew311@gmail.com",
         Service %in% "Google Directions API") %>%
  pull(Key)

# Keywords ---------------------------------------------------------------------
# Keywords to use to evaluate COVID containement policies
KEYWORDS_CONTAIN_USE <- c("social distance",
                          "stay at home",
                          
                          "unemployment",
                          "unemployment insurance",
                          "unemployment benefits",
                          "unemployment office",
                          "file for unemployment",
                          "debt",
                          
                          "boredom",
                          "anxiety",
                          "anxiety attack",
                          "anxiety symptoms",
                          "overwhelmed", 
                          "panic",
                          "hysteria",
                          "suicide",
                          "insomnia",
                          "social isolation",
                          "lonely",
                          "loneliness",
                          "divorce",
                          #"condom",
                          "emergency pill",
                          "pregnancy test", 
                          #"abortion",
                          #"plan child",
                          #"plan other children",
                          "tinder",
                          #"relationship",
                          "break up", 
                          "wedding", 
                          "dating app") 

KEYWORDS_SYMTPOMS <- c("loss of smell",
                       "loss of taste",
                       "i can't smell",
                       "i can't taste",
                       "ageusia",
                       "anosmia",
                       "pneumonia",
                       "cough",
                       "fever",
                       "shortness of breath",
                       "how to treat coronavirus",
                       "covid symptoms",
                       "coronavirus",
                       "covid-19")

# Common Functions -------------------------------------------------------------
lm_post_confint_tidy <- function(lm){
  # Extract coefficients and confidence interval from regression coefficients
  
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint$tvalue <- summary(lm)$coefficients[,3] %>% as.vector()
  lm_confint$pvalue <- summary(lm)$coefficients[,4] %>% as.vector()
  
  return(lm_confint)
}



