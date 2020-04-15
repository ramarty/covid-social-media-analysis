# Scrape Data from Google Trends

# RESOURCES
# https://github.com/GeneralMills/pytrends
# https://support.google.com/trends/answer/4365533?hl=en

# Grab admin codes to scrape ---------------------------------------------------
data("countries")
brazil_search <- countries[grepl("BR-", countries$sub_code),] 
brazil_search$sub_code <- brazil_search$sub_code %>% as.character()
brazil_search$name <- brazil_search$name %>% as.character()
brazil_search$id <- 1:nrow(brazil_search)

brazil_search$search_group <- rep(1:550, each=5)[1:nrow(brazil_search)]

# Categories ----
data("categories")

# Scrape Data ------------------------------------------------------------------

## Initialize dataframe
trends_df_all <- data.frame(NULL)

## Loop through search terms
for(term in c("febre", 
              "covid",
              "coronavirus", 
              "sintomas do coronavirus",
              "febre alta valor",
              "tosse",
              "perda de olfato", 
              "dificuldade ao respirar", 
              "falta de cheiro",
              "dor nos olhos", 
              "ventiladores", 
              "desinfetantes", 
              "termômetros", 
              "cama de hospital", 
              "hidroxicloroquina", 
              "fique em casa",
              'máscara facial', 
              "lavar as mãos", 
              "distância social", 
              "Educação online", 
              "desemprego", 
              "dívida", 
              "terapia", 
              "psicologia", 
              "violência baseada no gênero", 
              "abuso", 
              "abuso sexual")){
  
  ## Scrape for specific localities
  trends_df <- lapply(1:27, function(i){
    
    print(i)
    
    tryCatch({  
      out <- gtrends(term, 
                     category = "0",
                     geo = c(brazil_search$sub_code[i]),
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
saveRDS(trends_df_all, file.path(dropbox_file_path, paste0("Data/google_trends/RawData/brazil_extract_", date,".Rds", sep = ""))) 
write.csv(trends_df_all, file.path(dropbox_file_path, paste0("Data/google_trends/RawData/brazil_extract_", date,".csv", sep = "")), row.names=F)


# Problems with Santa Catarina and Rondonia-------------------------------------
# Notes: There sub_codes are BR-SC and BR-R0
brazil_search_missings <- brazil_search %>% 
  filter(sub_code %in% c("BR-SC", "BR-R0"), 
         name %in% c("SANTA CATARINA", "RONDONIA")) %>% 
  mutate(
    sub_code = ifelse(sub_code == "BR-R0", "BR-RO", sub_code)
  )

# Scrape Data ------------------------------------------------------------------

## Initialize dataframe
trends_df_all <- data.frame(NULL)

## Loop through search terms
for(term in c("febre", 
              "covid",
              "coronavirus", 
              "sintomas do coronavirus",
              "febre alta valor",
              "tosse",
              "perda de olfato", 
              "dificuldade ao respirar", 
              "falta de cheiro",
              "dor nos olhos", 
              "ventiladores", 
              "desinfetantes", 
              "termômetros", 
              "cama de hospital", 
              "hidroxicloroquina", 
              "fique em casa",
              'máscara facial', 
              "lavar as mãos", 
              "distância social", 
              "Educação online", 
              "desemprego", 
              "dívida", 
              "terapia", 
              "psicologia", 
              "violência baseada no gênero", 
              "abuso", 
              "abuso sexual")){
  
  ## Scrape for specific localities
  trends_df <- lapply(1:2, function(i){
    
    print(i)
    
    tryCatch({  
      out <- gtrends(term, 
                     category = "0",
                     geo = c(brazil_search_missings$sub_code[i]),
                     time = "today 3-m",
                     onlyInterest=T,
                     low_search_volume=T)
      
      Sys.sleep(10)
      
      out_df <- out$interest_over_time
      out_df$name <- brazil_search_missings$name[i]
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
saveRDS(trends_df_all, file.path(dropbox_file_path, paste0("Data/google_trends/RawData/brazil_extract_ms_", date,".Rds", sep = ""))) 
write.csv(trends_df_all, file.path(dropbox_file_path, paste0("Data/google_trends/RawData/brazil_extract_ms_", date,".csv", sep = "")), row.names=F)
