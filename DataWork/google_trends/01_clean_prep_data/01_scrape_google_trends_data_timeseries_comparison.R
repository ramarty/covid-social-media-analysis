# Scrape Data from Google Trends

# RESOURCES
# https://github.com/GeneralMills/pytrends
# https://support.google.com/trends/answer/4365533?hl=en


comparison_iso <- "BR-SP"
#comparison_iso <- "BR-RJ"

# Grab admin codes to scrape ---------------------------------------------------

# Iso Codes  ----
library(ISOcodes)

isocodes <- ISO_3166_2

br_isocodes <- isocodes %>% 
  janitor::clean_names() %>% 
  filter(str_detect(code, "BR-"),
         code != "BR-FN") %>% 
  dplyr::rename(sub_code = code) %>% 
  dplyr::select(sub_code, name)

# So can search 4 countries at a time
br_isocodes$iso_search_group <- rep(1:27, each=4)[1:nrow(br_isocodes)]



# Scrape Data ------------------------------------------------------------------

## Initialize dataframe
trends_df_all <- data.frame(NULL)

## Loop through search terms
for(term in c("quais são os sintomas do coronavírus",
              "eu tenho coronavírus",
              "entediado",
              "estou entediado",
              "Perdi o olfato",
              "Estou com falta de ar",
              "meus olhos doem",
              "estou com febre",
              "como tratar o coronavírus",
              "teste de coronavírus",
              "febre",
              "tosse",
              "número de emergência coronavírus",
              "ajuda do coronavírus",
              "Eu fico em casa",
              "Isolamento social",
              "profissionais de saúde",
              "medicos",
              "cloroquina",
              "isolamento vertical",
              "volta brasil")){
  
  ## Scrape for specific localities
  trends_df <- lapply(unique(br_isocodes$iso_search_group), function(i){
    
    print(i)
    
    tryCatch({  
      
      out <- gtrends(term, 
                     category = "0",
                     geo = c(br_isocodes$sub_code[br_isocodes$iso_search_group %in% i], 
                             comparison_iso) %>%
                       unique(),
                     time = "today 3-m",
                     onlyInterest=T,
                     low_search_volume=T)
      
      #Sys.sleep(1)
      
      out_df <- out$interest_over_time
      out_df$iso_search_group <- i
      #out_df$name <- br_isocodes$name[i]
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
