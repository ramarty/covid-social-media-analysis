# Scrapes google trends, using both a comparison state and without a comparison 
# state

#### PARAMETERS
SLEEP_TIME <- 1 # number of seconds to pause after each scrape
overwrite_files <- F
comparison_iso <- "US"

# Load file that indicates which language to use for each country. Contains 
# a language code and country code
languages <- read.csv(file.path(dropbox_file_path, 
                                "Data", "country_primary_language", "countries_lang.csv"),
                      stringsAsFactors = F) 

language_codes_all <- languages$Language_code_main %>% unique()
language_codes_all <- language_codes_all[!is.na(language_codes_all)]
language_codes_all <- language_codes_all[language_codes_all != ""]
language_codes_all <- language_codes_all %>% sort()

for(language in language_codes_all){
  
  # Terms to Scrape --------------------------------------------------------------
  keywords <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                                 "keywords", "FinalData", "covid_keywords_alllanguages_clean.Rds"))
  
  keywords <- keywords %>%
    arrange(priority_to_scrape) %>%
    filter(scrape %in% "yes")
  
  # Clean keyword
  keywords_vec <- keywords[[paste0("keyword_", language)]] %>% tolower() %>% as.character()
  keywords_vec <- keywords_vec[keywords_vec != ""]
  keywords_vec <- keywords_vec %>% str_replace_all("\\n", "") # some have newline
  
  # ISO Codes ------------------------------------------------------------------
  # Grab iso/country codes where the selected language is the main language
  iso2 <- languages$Code[languages$Language_code_main %in% language]
  iso2 <- iso2[!is.na(iso2)]
  
  # Function to Scrape Data ----------------------------------------------------
  extract_trends <- function(iso_i,
                             term_i, 
                             sleep_time = SLEEP_TIME){
    
    print(iso_i)
    
    #### 1. Scrape
    out <- gtrends(term_i, 
                   geo = iso_i,
                   time = "2020-02-05 2020-10-31",
                   onlyInterest=T,
                   low_search_volume=T)
    
    out_df <- out$interest_over_time
    for(var in names(out_df)) out_df[[var]] <- out_df[[var]] %>% as.character()
    
    # Didn't return error, but no hits? Object will be null, which will cause
    # error later. In this case, we just want to skip.
    if((class(out)[1] %in% "gtrends") & is.null(out_df)){
      out_df <- data.frame(NULL)
    } 

    
    #### 4. Take a quick nap b/c of google rate limits
    Sys.sleep(sleep_time) #  + runif(1)*2
    
    print(nrow(out_all_df))
    return(out_df)
    
  }
  
  # Scrape Data ------------------------------------------------------------------
  # Nested for loop isn't ideal, but works so oh well.
  for(term_i in keywords_vec){
    for(iso_i in iso2){
      
      out_path <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                            "global_with_ref_state_by_keyword",
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
        print(paste(iso_i, term_i, "-------------------------------------------"))
        
        tryCatch({
          
          term_df <- extract_trends(iso_i,
                                    term_i,
                                    comparison_iso)
          term_df$language <- language
          
          saveRDS(term_df, out_path)
          
          Sys.sleep(0.01) # pause after each term
          
        }, error=function(e){})
        
      }
      
    }
  }
  
  # end language loop
}

