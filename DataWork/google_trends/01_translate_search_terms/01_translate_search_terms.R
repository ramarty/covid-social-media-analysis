# Translate Search Terms

#### API Key
api_key <- read_csv(file.path("~", "Dropbox", "World Bank", "Webscraping", "Files for Server", 
                              "api_keys.csv")) %>%
  filter(Account %in% "robertandrew311@gmail.com",
         Service %in% "Google Directions API") %>%
  pull(Key)

# Load Data --------------------------------------------------------------------
## Keywords to srape
keywords_df <- read_csv(file.path(dropbox_file_path, "Data", "google_trends", 
                                  "keywords", "RawData", "covid_keywords_english.csv"))

## Language for each country
languages_df <- read_csv(file.path(dropbox_file_path, "Data", 
                                   "country_primary_language", "countries_lang.csv"))

## Cleanup language codes
language_codes <- languages_df$Language_code_main %>%
  unique() %>%
  na.omit() %>%
  as.vector()

language_codes <- language_codes[language_codes != "en"] # remove english
language_codes <- c(language_codes, "sw") # adding swahili

# Scrape Translations ----------------------------------------------------------
for(l_code_i in sort(language_codes)){
  print(l_code_i)
  
  translations_out <- r_google_translate_vec(keywords_df$keyword_en,
                                             target = l_code_i,
                                             format = "text",
                                             source = "en",
                                             model = "nmt", # "base",
                                             key = api_key,
                                             sleep = 0)
  
  keywords_df[[paste0("keyword_", l_code_i)]] <- translations_out
}

# Export -----------------------------------------------------------------------
write.csv(keywords_df, file.path(dropbox_file_path, "Data", "google_trends", 
                                 "keywords", "FinalData", "covid_keywords_alllanguages.csv"),
          row.names = F)
saveRDS(keywords_df, file.path(dropbox_file_path, "Data", "google_trends", 
                               "keywords", "FinalData", "covid_keywords_alllanguages.Rds"))



