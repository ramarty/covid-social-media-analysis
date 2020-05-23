#------------------------------------------------------------------------------#
# PROJECT:                  Google Trends for Covid19 
# COUNTRY:                      Brazil
#------------------------------------------------------------------------------#
# 
# Outline:
#   1.  Settings
#       1.1. Load packages
#   2.  File paths
#       2.1. Dropbox and GitHub paths
#       2.2. Folder paths
#   3.  Codes
#       3.1. Graphs
#
#------------------------------------------------------------------------------#
# 1. SETTINGS----
#------------------------------------------------------------------------------#
clean_state_pop_data <- FALSE
merge_
#------------------------------------------------------------------------------#
#   1.1. Load packages
#------------------------------------------------------------------------------#
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, gtrendsR, janitor, lubridate, parallel,jsonlite,
               cowplot, stringr, stringi, purrr, tidytext, 
               quanteda, tm, ggpubr, sf, readstata13)


#------------------------------------------------------------------------------#
# 2. FILE PAHTS----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#   2.1. Dropbox and GitHub paths
#------------------------------------------------------------------------------#

# User: Rob Marty
#------------------------------------------------------------------------------#
if(Sys.info()[["user"]] == "WB521633") dropbox_file_path <- "C:/Users/wb521633/Dropbox/World Bank/Side Work/COVID Social Media Analysis"
if(Sys.info()[["user"]] == "robmarty") dropbox_file_path <- "~/Dropbox/World Bank/Side Work/COVID Social Media Analysis"

# User: Manuel Ramos
#------------------------------------------------------------------------------#
if (Sys.getenv("USERNAME") == "wb537287") {
    dropbox_file_path <- "/Users/wb537287/Dropbox/COVID Social Media Analysis"
    github_file_path  <- file.path("C:/Users/wb537287/Documents/GitHub/covid-social-media-analysis/DataWork/google_trends")
}

if (Sys.getenv("USERNAME") == "manuelramos") {
    dropbox_file_path <- "~/Dropbox/COVID Social Media Analysis"
    github_file_path  <- file.path("~/Documents/GitHub/covid-social-media-analysis/DataWork/google_trends")
}

#------------------------------------------------------------------------------#
#   2.2. Folder paths
#------------------------------------------------------------------------------#
data_raw  <- file.path(dropbox_file_path,  "google_trends/RawData")
data_final <- file.path(dropbox_file_path,  "google_trends/FinalData")

#------------------------------------------------------------------------------#
#   2.3. Function to source Rmarkdown Files (.Rmd)
#------------------------------------------------------------------------------#

ksource <- function(x, ...) {
    library(knitr)
    source(purl(x, output = tempfile()))
}


#------------------------------------------------------------------------------#
# 3. CODES----
#------------------------------------------------------------------------------#
#   3.1 PREPARE DATA 
#------------------------------------------------------------------------------#
if (plotsgraphs==TRUE) {
    #~~~~~~~~~~~~~~~~~~~~~~~
    # Cross-sectional analysis~----
    #~~~~~~~~~~~~~~~~~~~~~~~
    # Contracts vs Non-contracts and Information vs Non-information
    ksource(file.path(github_file_path, "02_clean_crosstate_data.Rmd"))
    # Within Treatment 1: Information
    ksource(file.path(github_file_path, "07_Cross_State_Analysis.Rmd"))
}