# Scrape Data from Google Trends

# RESOURCES
# https://github.com/GeneralMills/pytrends

# Grab admin codes to scrape ---------------------------------------------------
data("countries")
brazil_search <- countries[grepl("BR-", countries$sub_code),] 
brazil_search$sub_code <- brazil_search$sub_code %>% as.character()
brazil_search$name <- brazil_search$name %>% as.character()
brazil_search$id <- 1:nrow(brazil_search)

brazil_search$search_group <- rep(1:550, each=5)[1:nrow(brazil_search)]

# Scrape Data ------------------------------------------------------------------

## Initialize dataframe
trends_df_all <- data.frame(NULL)

## Loop through search terms
for(term in c("febre", 
              "coronavirus", 
              "sintomas do coronavirus",
              "febre alta valor",
              "tosse")){
  
  ## Scrape for specific localities
  trends_df <- lapply(1:27, function(i){
  
    print(i)
    
    tryCatch({  
               out <- gtrends(term, 
                              category = "0",
                              geo = brazil_search$sub_code[i],
                              time = "today+5-y",
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
saveRDS(trends_df_all, file.path(dropbox_file_path, "Data/google_trends/RawData/brazil_extract.Rds"))
write.csv(trends_df_all, file.path(dropbox_file_path, "Data/google_trends/RawData/brazil_extract.csv"), row.names=F)

