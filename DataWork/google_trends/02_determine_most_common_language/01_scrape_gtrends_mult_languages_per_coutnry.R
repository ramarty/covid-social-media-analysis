# Translate Search Terms

rextract_if_exists <- F
sleep_time <- 3

# Load Data --------------------------------------------------------------------
languages_df <- read_csv(file.path(dropbox_file_path, "Data", 
                                   "country_primary_language", "RawData",
                                   "countries_modified.csv"))

languages_df <- languages_df %>%
  dplyr::filter(!is.na(Language_code_5))

keywords_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                                 "keywords", "FinalData", "covid_keywords_alllanguages.Rds"))

# Extract Data -----------------------------------------------------------------
for(keyword_en_i in c("fever")){
  for(iso_i in unique(languages_df$Code)){
    
    OUT_PATH <- file.path(dropbox_file_path, 
                          "Data", 
                          "country_primary_language", 
                          "FinalData", 
                          "gtrends_data",
                          paste0("gtrends_keyworden_",keyword_en_i,
                                 "iso_", iso_i, ".Rds"))
    
    if(rextract_if_exists | !file.exists(OUT_PATH)){
      print(paste0(iso_i, " - ", keyword_en_i))
      
      # Grab parameters --------------------------------------------------------
      
      ## Grab Languages
      languages_all <- languages_df$Language_code_5[languages_df$Code %in% iso_i] %>% 
        str_split(",") %>% 
        unlist()
      
      ## Grab vector of languages and translations
      keywords_df_i <- keywords_df[keywords_df$keyword_en %in% keyword_en_i,]
      keywords_df_i <- keywords_df_i[,names(keywords_df_i) %in% paste0("keyword_", languages_all)]
      
      terms_i <- keywords_df_i %>% apply(1, as.character) %>% unlist() %>% as.vector() %>% tolower()
      languages_i <- names(keywords_df_i)
      
      term_lng_df <- data.frame(keyword = terms_i,
                                language = languages_i) %>%
        group_by(keyword) %>%
        dplyr::summarise(language = language %>% paste0(collapse = ";")) %>%
        as.data.frame()
      
      # Extract Data -----------------------------------------------------------
      if(length(languages_i) %in% 0){
        df_out <- data.frame(language = "NONE",
                             geo = iso_i,
                             n_languages = 0)
      } else if(length(languages_i) %in% 1){
        df_out <- data.frame(language = languages_i,
                             geo = iso_i,
                             n_languages = 1)
      } else{
        out <- gtrends(unique(terms_i), 
                       geo = iso_i,
                       time = "2020-01-01 2020-12-31",
                       onlyInterest = F,
                       low_search_volume=T)
        Sys.sleep(sleep_time)
        
        ### Error check
        # Didn't return error, but no hits. Object will be null, which will cause
        # error later. In this case, we just want to skip.
        if((class(out)[1] %in% "gtrends") & is.null(out_df)){
          df_out <- data.frame(language = NA,
                               geo = iso_i,
                               n_languages = NA,
                               gtrends_nohits = T)
        } else{
          
          df_out <- out$interest_over_time
          
          df_out <- df_out %>%
            left_join(term_lng_df, by = "keyword")
          df_out$keyword_en <- keyword_en_i
          df_out$n_languages <- length(languages_i)
        }
        
        # Export ---------------------------------------------------------------
        saveRDS(df_out, OUT_PATH)
      }
    }
  }
}

