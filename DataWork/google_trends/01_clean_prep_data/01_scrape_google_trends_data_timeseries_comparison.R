# Scrape Data from Google Trends
# Loop through states and terms. For each state-term combination, also search
# for the comparison iso/state. Consequently, for each search, we'll get hits
# for 1 term and 2 states.

comparison_iso <- "BR-SP"

# Grab admin codes to scrape ---------------------------------------------------
isocodes <- ISO_3166_2 # from ISOcodes package

br_isocodes <- isocodes %>% 
  janitor::clean_names() %>% 
  filter(str_detect(code, "BR-"),
         code != "BR-FN") %>% 
  dplyr::rename(sub_code = code) %>% 
  dplyr::select(sub_code, name)

# Scrape Data ------------------------------------------------------------------

## Initialize dataframe
trends_df_all <- data.frame(NULL)

## Loop through search terms
for(term in c("perdi o olfato",
              "febre",
              "tosse",
              "cloroquina",
              "isolamento vertical")){
  
  ## Scrape for specific localities
  trends_df <- lapply(br_isocodes$sub_code, function(iso_i){
    
    print(iso_i)
    
    tryCatch({  
      
      out <- gtrends(term, 
                     category = "0",
                     geo = c(iso_i, 
                             comparison_iso) %>%
                       unique(),
                     time = "today 3-m",
                     onlyInterest=T,
                     low_search_volume=T)
      
      #Sys.sleep(1)
      
      out_df <- out$interest_over_time
      out_df$iso_search_group <- i
      out_df$comparison_iso <- comparison_iso
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
saveRDS(trends_df_all, file.path(dropbox_file_path, paste0("Data/google_trends/RawData/brazil_extract_extra_words_compare",comparison_iso,".Rds")))
write.csv(trends_df_all, file.path(dropbox_file_path, paste0("Data/google_trends/RawData/brazil_extract_extra_words_compare",comparison_iso,".csv")), row.names=F)
