# Scrapes google trends, using both a comparison state and without a comparison 
# state

comparison_iso <- "BR-SP"

terms <- c("febre",
           "tosse")

# ISO Codes --------------------------------------------------------------------
isocodes <- ISO_3166_2 # from ISOcodes package

br_isocodes <- isocodes %>% 
  janitor::clean_names() %>% 
  filter(str_detect(code, "BR-"),
         code != "BR-FN") %>% 
  dplyr::rename(sub_code = code) %>% 
  dplyr::select(sub_code, name)

# Function to Scrape Data ------------------------------------------------------
extract_trends <- function(term_i, 
                           iso_i, 
                           comparison_iso, 
                           sleep_time = 0.1,
                           also_scrape_without_cstate = T){
  
  tryCatch({  
    
    #### 1. Scrape
    
    # Without comparison state
    if(also_scrape_without_cstate){
      
      out <- gtrends(term_i, 
                     category = "0",
                     geo = iso_i,
                     time = "today 3-m",
                     onlyInterest=T,
                     low_search_volume=T)
      
      out_df <- out$interest_over_time
      for(var in names(out_df)) out_df[[var]] <- out_df[[var]] %>% as.character()
      
    }
    
    # With comparison state
    out_cstate <- gtrends(term_i, 
                          category = "0",
                          geo = c(iso_i, 
                                  comparison_iso) %>%
                            unique(),
                          time = "today 3-m",
                          onlyInterest=T,
                          low_search_volume=T)
    
    out_cstate_df <- out_cstate$interest_over_time
    for(var in names(out_cstate_df)) out_cstate_df[[var]] <- out_cstate_df[[var]] %>% as.character()
    
    
    
    #### 2. Prep comparison state output
    out_cstate_df <- out_cstate_df %>%
      dplyr::rename(hits_with_compstate = hits)
    
    ## Add hits of comparison state as variable (go from long to wide)
    if(iso_i != comparison_iso){
      
      # Grab hits of comparison state
      out_cstate_compstate_df <- out_cstate_df %>%
        filter(geo == comparison_iso) %>%
        dplyr::select(date, hits_with_compstate) %>%
        dplyr::rename(hits_compstate = hits_with_compstate)
      
      # Restrict to state of interest (remove comparison state), and merge
      # hits of comparison state
      out_cstate_df <- out_cstate_df %>%
        filter(geo != comparison_iso) %>%
        left_join(out_cstate_compstate, by = "date")
    } else{
      out_cstate_df$hits_compstate = out_cstate_df$hits_with_compstate
    }
    
    
    
    #### 3. Merge datasets with comp state and without comp state
    if(also_scrape_without_cstate){
      out_all_df <- out_df %>%
        dplyr::select(date, hits) %>%
        left_join(out_cstate_df, by = "date")
    } else{
      out_all_df <- out_cstate_df
    }

    
    #### 4. Take a quick nap b/c of google rate limits
    Sys.sleep(sleep_time)
    
    return(out_all_df)
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
  
  Sys.sleep(15) # pause after each term
}

# Export -----------------------------------------------------------------------
saveRDS(results_all_df, file.path(dropbox_file_path, "Data", "google_trends", "RawData", 
                                  paste0("br_gtrends_ref",comparison_iso,".Rds")))


