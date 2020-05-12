# Scrape Data for Brazil only (at the country level) from Google Trends

# RESOURCES
# https://github.com/GeneralMills/pytrends
# https://support.google.com/trends/answer/4365533?hl=en

# Grab admin codes to scrape ---------------------------------------------------


data("countries")

# Categories ----
data("categories")

# Scrape Data ------------------------------------------------------------------

## Initialize dataframe
trends_df_all <- data.frame(NULL)

## Loop through search terms
for(term in c("tosse",
              "febre",
              "cansaço",
              "dificuldade ao respirar",
              "perda de olfato",
              "dor nos olhos",
              "coronavirus", 
              "covid",
              "corona",
              "quais são os sintomas do coronavírus",
              "eu tenho coronavírus",
              "perdi o olfato",
              "estou com falta de ar", 
              "estou com febre",
              "como tratar o coronavírus", 
              "ajuda do coronavírus")){
  
  ## Scrape for specific localities
  trends_df <- lapply(1:1, function(i){
    
    print(i)
    
    tryCatch({  
      out <- gtrends(term, 
                     category = "0",
                     geo = "BR",
                     time = "today 3-m",
                     onlyInterest=T,
                     low_search_volume=T)
      
      out_df <- out$interest_over_time
      out_df$name <- brazil_search$name[i]
      for(var in names(out_df)) out_df[[var]] <- out_df[[var]] %>% as.character()
      
      return(out_df)
    }
    , 
    error = function(e) return(NULL)
    )
    
    
  }) %>% 
    bind_rows()
  
  trends_df_all <- bind_rows(trends_df_all, trends_df)
  
}

# Export -----------------------------------------------------------------------

# Save the date 
date <- Sys.Date()

# Rds and Csv
saveRDS(trends_df_all, file.path(dropbox_file_path, paste0("Data/google_trends/RawData/brazil_country_extract_", date,".Rds", sep = ""))) 
write.csv(trends_df_all, file.path(dropbox_file_path, paste0("Data/google_trends/RawData/brazil_country_extract_", date,".csv", sep = "")), row.names=F)
