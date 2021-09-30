# Translate Search Terms

# Load Data --------------------------------------------------------------------
## Keywords to srape
keywords_df <- read_csv(file.path(dropbox_file_path, "Data", "google_trends", 
                                  "keywords", "RawData", "covid_keywords_english.csv"))

## Language for each country
languages_df <- readRDS(file.path(dropbox_file_path, "Data", 
                                  "country_primary_language", "FinalData",
                                  "countries_modified_clean.Rds"))

## Vector of language codes
language_codes <- languages_df$Language_code_5 %>%
  str_split(",") %>%
  unlist() %>%
  unique() %>%
  sort()

language_codes <- language_codes[language_codes != "en"] # remove english

# Scrape Translations ----------------------------------------------------------
for(l_code_i in language_codes){
  print(l_code_i)
  
  translations_out <- r_google_translate_vec(keywords_df$keyword_en,
                                             target = l_code_i,
                                             format = "text",
                                             source = "en",
                                             model = "nmt", # "base",
                                             key = api_key,
                                             sleep = 0)
  
  if(!is.null(translations_out)){
    keywords_df[[paste0("keyword_", l_code_i)]] <- translations_out
  } else{
    print("Null!")
  }
  
}

# Export -----------------------------------------------------------------------
saveRDS(keywords_df, file.path(dropbox_file_path, "Data", "google_trends", 
                               "keywords", "FinalData", "covid_keywords_alllanguages.Rds"))



