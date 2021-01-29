# Scrapes google trends, using both a comparison state and without a comparison 
# state

#### PARAMETERS
SLEEP_TIME <- 0.01 # number of seconds to pause after each scrape
overwrite_files <- F
comparison_iso <- "US"

# Function to Scrape Data ----------------------------------------------------
extract_trends <- function(iso_i,
                           term_i, 
                           language,
                           sleep_time){
  
  # Scrape
  out <- gtrends(term_i, 
                 geo = iso_i,
                 time = "2020-06-01 2021-01-27",
                 onlyInterest=T,
                 low_search_volume=T)
  
  # Prep output
  out_df <- out$interest_over_time
  for(var in names(out_df)) out_df[[var]] <- out_df[[var]] %>% as.character()
  
  # Didn't return error, but no hits? Object will be null, which will cause
  # error later. In this case, we just want to skip.
  if((class(out)[1] %in% "gtrends") & is.null(out_df)){
    out_df <- data.frame(NULL)
  } else{
    out_df$language <- language
  }
  
  # Take a quick nap b/c of google rate limits
  Sys.sleep(sleep_time) 
  
  print(nrow(out_df))
  return(out_df)
  
}

# Loop through language and keyword --------------------------------------------
for(language in c("en", "sw")){
  
  # Terms to Scrape ------------------------------------------------------------
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
  iso2 <- "TZ"
  
  # Scrape Data ------------------------------------------------------------------
  # Nested for loop isn't ideal, but works so oh well.
  for(term_i in keywords_vec){
    for(iso_i in iso2){
      
      out_path <- file.path(dropbox_file_path, "Data", "google_trends_tza", "RawData",
                            paste0("gtrends",
                                   iso_i, 
                                   "_term",
                                   term_i,
                                   "_language",
                                   language,
                                   ".Rds"))
      
      if(!file.exists(out_path) | overwrite_files){
        print(paste(language, iso_i, term_i, "-------------------------------"))
        
        tryCatch({
          
          term_df <- extract_trends(iso_i,
                                    term_i,
                                    language,
                                    SLEEP_TIME)

          saveRDS(term_df, out_path)
          
          Sys.sleep(0.01) # pause after each term
          
        }, error=function(e){})
        
      }
      
    }
  }
  
  # end language loop
}

