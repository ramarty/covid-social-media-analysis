# Scrape Data from Google Trends
# Loop through states and terms. For each state-term combination, also search
# for the comparison iso/state. Consequently, for each search, we'll get hits
# for 1 term and 2 states.

comparison_iso <- "BR-RJ"

terms <- c("perdi o olfato",
           "febre",
           "tosse",
           "cloroquina",
           "isolamento vertical")

# ISO Codes --------------------------------------------------------------------
isocodes <- ISO_3166_2 # from ISOcodes package

br_isocodes <- isocodes %>% 
  janitor::clean_names() %>% 
  filter(str_detect(code, "BR-"),
         code != "BR-FN") %>% 
  dplyr::rename(sub_code = code) %>% 
  dplyr::select(sub_code, name)

# Function to Scrape Data ------------------------------------------------------
extract_trends <- function(term_i, iso_i, comparison_iso, sleep_time = 0.1){

  tryCatch({  

    out <- gtrends(term_i, 
                   category = "0",
                   geo = c(iso_i, 
                           comparison_iso) %>%
                     unique(),
                   time = "today 3-m",
                   #time = "today 12-m",
                   onlyInterest=T,
                   low_search_volume=T)
    
    out_df <- out$interest_over_time
    for(var in names(out_df)) out_df[[var]] <- out_df[[var]] %>% as.character()
    
    # If with comparison state: long to wide
    if(length(unique(out_df$geo)) == 2){
      out_df_compi <- out_df %>%
        filter(geo == comparison_iso) %>%
        dplyr::select(date, hits) %>%
        dplyr::rename(hits_comparison_state = hits)
      
      out_df <- out_df %>%
        filter(geo != comparison_iso) %>%
        left_join(out_df_compi, by = "date")
    }
    
    return(out_df)
  }, 
  error = function(e) return(NULL)
  )
  
}

# Scrape Data ------------------------------------------------------------------
# Nested for loop isn't ideal, but works so oh well.

results_all_df <- data.frame(NULL)

for(term_i in terms){
  print(paste(term_i, "------------------------------------------------------"))
  for(iso_i in br_isocodes$sub_code){
    
    result_i_df <- extract_trends(term_i, iso_i, comparison_iso)
    print(paste(iso_i, "-", nrow(result_i_df)))
    
    results_all_df <- bind_rows(result_i_df,
                                results_all_df)
    
  }
  
  Sys.sleep(5) # pause after each term
}

# Export -----------------------------------------------------------------------
saveRDS(results_all_df, file.path(dropbox_file_path, "Data", "google_trends", "RawData", 
                                  paste0("br_gtrends_ref",comparison_iso,".Rds")))
write.csv(results_all_df, file.path(dropbox_file_path, "Data", "google_trends", "RawData", 
                                    paste0("br_gtrends_ref",comparison_iso,".csv")), row.names=F)


