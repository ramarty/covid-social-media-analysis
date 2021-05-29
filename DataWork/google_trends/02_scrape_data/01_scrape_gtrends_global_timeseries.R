# Scrape Google Trends Timeseries

# DESCRIPTION: Loops through countries and search terms and saves the search
# interest time series for a specified time period. Saves a separate dataset for
# each search; if alredy searched, then skips. (Saving datasets separately and skiping
# enables rerunning the code when new search terms have been added and having the code
# only scrape those search terms).

#Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# PARAMETERS
SLEEP_TIME      <- 3 # number of seconds to pause after each scrape
overwrite_files <- F # overwrite data?

# OUT_FOLDER_LIST <- c("timeseries_2020-01-01_2020-09-26",
#                      "timeseries_2020-07-05_2021-03-31") %>% rev()
OUT_FOLDER_LIST <- c("timeseries_regions_2020-12-01_2021-05-25",
                     "timeseries_regions_2021-03-01_2021-05-25") 
                     # "timeseries_regions_2020-01-01_2020-01-31",
                     # "timeseries_regions_2020-02-01_2020-02-29",
                     # "timeseries_regions_2020-03-01_2020-03-31",
                     # "timeseries_regions_2020-04-01_2020-04-30",
                     # "timeseries_regions_2020-05-01_2020-05-31",
                     # "timeseries_regions_2020-06-01_2020-06-30",
                     # "timeseries_regions_2020-07-01_2020-07-31",
                     # "timeseries_regions_2020-08-01_2020-08-31",
                     # "timeseries_regions_2020-09-01_2020-09-30",
                     # "timeseries_regions_2020-10-01_2020-10-31",
                     # "timeseries_regions_2020-11-01_2020-11-30",
                     # "timeseries_regions_2020-12-01_2020-12-31",
                     # "timeseries_regions_2021-01-01_2021-01-31",
                     # "timeseries_regions_2021-02-01_2021-02-28") 

## Subset for regions
# only used if ALL_COUNTRIES = F
select_countries_vec <- c("US", # United States
                          "CA", # Canada
                          "BR", # Brazil
                          "ZA", # South Africa
                          "ID", # Indonesia
                          "IT", # Italy
                          "PH", # Phillipines
                          "AU", # Australia
                          "IN") 

select_countries_vec <- c("US")

# Function to Scrape Google Data -----------------------------------------------
extract_trends <- function(iso_i,
                           term_i, 
                           language,
                           start_end_date,
                           onlyInterest,
                           sleep_time){
  # DESCRIPTION: Given an country, term, and start/end date, scrapes the data. 
  # Asks for the language code to add to the resulting dataset. sleep_time is done
  # because of google rate limits.
  
  # 1. Scrape
  out <- gtrends(term_i, 
                 geo = iso_i,
                 time = start_end_date,
                 onlyInterest = onlyInterest,
                 low_search_volume=T)

  if(onlyInterest %in% T){
    
    # 2. Grab data, and convert variables to character to avoid type conflict late
    out_df <- out$interest_over_time
    for(var in names(out_df)) out_df[[var]] <- out_df[[var]] %>% as.character()
    
    # 3. Error check
    # Didn't return error, but no hits? Object will be null, which will cause
    # error later. In this case, we just want to skip.
    if((class(out)[1] %in% "gtrends") & is.null(out_df)){
      out_df <- data.frame(NULL)
    } else{
      out_df$language <- language
    }
    
    # 4. Take a quick nap b/c of google rate limits
    Sys.sleep(sleep_time) #  + runif(1)*2
    
    print(iso_i)
    print(nrow(out_df))
  } else{
    out_df <- out
  }
  
  return(out_df)
}

# Load / Initial Data Prep -----------------------------------------------------

## Keywords Dataset
keywords_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                                 "keywords", "FinalData", "covid_keywords_alllanguages_clean.Rds"))

# keywords_df <- read.csv(file.path(dropbox_file_path, "Data", "google_trends", 
#                                   "keywords", "FinalData", "covid_keywords_alllanguages.csv"),
#                         encoding="UTF-8", stringsAsFactors=FALSE)
# keywords_df$keyword_zh 
#keywords_df$keyword_zh 

keywords_df <- keywords_df %>%
  arrange(priority_to_scrape) %>%
  filter(scrape %in% "yes")

## Language Dataset
# Indicates which language to use for each country. 
languages <- read.csv(file.path(dropbox_file_path, 
                                "Data", "country_primary_language", "countries_lang.csv"),
                      stringsAsFactors = F) 

language_codes_all <- languages$Language_code_main %>% unique()
language_codes_all <- language_codes_all[!is.na(language_codes_all)]
language_codes_all <- language_codes_all[language_codes_all != ""]
language_codes_all <- language_codes_all %>% sort()

#language_codes_all <- language_codes_all[language_codes_all != "my"]

# Prep Parameters Based on Folder Name -----------------------------------------
for(OUT_FOLDER in OUT_FOLDER_LIST){
  
  if(grepl("timeseries_regions_", OUT_FOLDER)){
    ALL_TERMS <- F
    ALL_COUNTRIES <- F
    onlyInterest <- F
  } else{
    ALL_TERMS <- T
    ALL_COUNTRIES <- T
    onlyInterest <- T
  }
  
  start_end_date <- OUT_FOLDER %>% 
    str_replace_all("timeseries_regions_", "") %>% 
    str_replace_all("timeseries_", "") %>%
    str_replace_all("_", " ")
  
  if(ALL_TERMS){
    keywords_sub_df <- keywords_df
  } else{
    keywords_sub_df <- keywords_df[keywords_df$category %in% c("vaccine"),]
  }
  
  ## Check if root folter eixts; if not, create
  # dir.create only creates if doesn't already exist
  dir.create(file.path(dropbox_file_path, "Data", "google_trends", "RawData", OUT_FOLDER))
  
  # Loop through languages, countries and terms ----------------------------------
  for(language in language_codes_all){
    
    ## KEYWORDS
    # Grab keyword for the language and cleanup keyword vector. Remove missing and
    # remove newlines
    keywords_vec <- keywords_sub_df[[paste0("keyword_", language)]] %>% tolower() %>% as.character()
    keywords_vec <- keywords_vec[keywords_vec != ""]
    keywords_vec <- keywords_vec %>% str_replace_all("\\n", "") # some have newline
    
    ## ISO CODES
    # Grab iso/country codes where the selected language is the main language
    iso2 <- languages$Code[languages$Language_code_main %in% language]
    iso2 <- iso2[!is.na(iso2)]
    
    if(!ALL_COUNTRIES) iso2 <- iso2[iso2 %in% select_countries_vec]
    
    ## SCRAPE DATA
    for(term_i in keywords_vec){
      for(iso_i in iso2){
        
        comparison_iso <- "US" # DUMMY; delete later
        out_path <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                              OUT_FOLDER,
                              paste0("global_gtrends_ref_",
                                     iso_i, 
                                     "_compr",
                                     comparison_iso,
                                     "_term",
                                     term_i,
                                     "_language",
                                     language,
                                     ".Rds"))
        
        if(!file.exists(out_path) | overwrite_files){
          print(start_end_date)
          print(paste(language, iso_i, term_i, "-------------------------------"))
          
          tryCatch({
            term_df <- extract_trends(iso_i,
                                      term_i,
                                      language,
                                      start_end_date,
                                      onlyInterest,
                                      SLEEP_TIME)
            
            saveRDS(term_df, out_path)
            
            Sys.sleep(0.01) # pause after each term
            
          }, error=function(e){})
          
        }
        
      }
    }
    
    # end language loop
  }
}

