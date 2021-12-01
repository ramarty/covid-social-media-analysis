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
               ggrepel, Rfast, tikzDevice, ISOcodes, ggthemes)

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
                          "condom",
                          "emergency pill",
                          "pregnancy test", 
                          "abortion",
                          "plan child",
                          "plan other children",
                          "tinder",
                          "relationship",
                          "break up", 
                          "wedding", 
                          "dating app") 

KEYWORDS_SYMTPOMS <- c("loss of smell",
                       "loss of taste",
                       "I can't smell",
                       "I can't taste",
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

# "vaccine cause autism"  
#  "Bill Gates BioTerrorist" 
# "Antivaccines"  
# "Fuck vaccines"  
# "Vaccine damage" 
# "Vaccine harm"  
#  "covid 19 vaccine priority list"  
# "vaccine approved" 
# "is vaccine approved" 
#  "vaccine sex"   
# "adverse effect"  
# "vaccine disease"   
VACCINE_KEYWORDS <- c("does covid vaccine change dna",             "covid vaccine change dna",             
                      "safety of covid vaccine",                    "covid vaccine safety",                      
                      "covid vaccine side effects",                 "covid vaccine dangerous",                   
                      "long term effects of covid vaccine",         "negative side effects of covid vaccine",    
                      "is the covid vaccine the mark of the beast", "is covid vaccine the mark of the beast",    
                      "covid vaccine",                              "covid vaccine cause infertility",           
                      "can covid vaccine cause infertility",        "does covid vaccine cause infertility",      
                      "covid vaccine infertility",                  "covid vaccine mark of the best",            
                      "covid vaccine austism",                     
                      "covid vaccine cause autism",                 "covid microchip",                           
                      "covid vaccine microchip",                    "ivermectin",                                
                      "COVID vaccine mercury",                                     
                      "Covid vaccine is poison",                   
                      "Medical freedom",                           
                      "Vaccine failure",                           
                      "Vaccine fraud",                             
                      "Vaccine injuries",                           "Vaccines are poison",                       
                      "Vaccines kill",                              "covid vaccine unsafe",                      
                      "covid vaccine ineffective",                  "blood clots",                               
                      "covid vaccine blood clots",                  "where covid vaccine",                       
                      "covid vaccine appointment",                  "vaccine appointment",                       
                      "pharmacy",                                   "vaccine near me",                           
                      "get covid vaccine near me",                  "where get covid vaccine near me",           
                      "where can I get the covid vaccine",          "where to get vaccine covid near me",        
                      "covid vaccine center",                       "where to get covid vaccine",                
                      "covid vaccine appointment near me",          "can i get the vaccine",                     
                      "can I get the covid vaccine",                "covid vaccine priority",                    
                      "covid vaccine priority list",                        
                      "sick from covid vaccine",                    "covid vaccine sick",                        
                      "does the covid vaccine make you sick",       "do you get sick after covid vaccine",       
                      "covid vaccine second dose",                  "covid vaccine second dose sick",            
                      
                      "covid vaccine approved",                     "is covid vaccine approved",                 
                      "vaccine approval",                           "vaccine reaction",                          
                      "vaccine conspiracy",                         "vaccine allergy",                           
                      "vaccine toxins",                            
                      "vaccine mercury",                            "vaccine aluminum",                          
                      
                      "vaccine dna", "vaccine",
                      "needle phobia",
                      "fear of needles",
                      "trypanophobia")

#keywords_df <- read_csv(file.path(dropbox_file_path, "Data", "google_trends", 
#                                  "keywords", "RawData", "covid_keywords_english.csv"))

#keywords_df$keyword_en[keywords_df$category %>% str_detect("vac")] %>% unique()

KEYWORDS_TIMESERIES_ALL <- c(KEYWORDS_CONTAIN_USE, KEYWORDS_SYMTPOMS, VACCINE_KEYWORDS) %>% unique()

KEYWORDS_TIMESERIES_ALL_lw <- tolower(KEYWORDS_TIMESERIES_ALL)
KEYWORDS_SYMTPOMS_lw <- tolower(KEYWORDS_SYMTPOMS)

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



