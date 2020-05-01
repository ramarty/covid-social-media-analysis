# Scrape Data from Google Trends

# RESOURCES
# https://github.com/GeneralMills/pytrends
# https://support.google.com/trends/answer/4365533?hl=en


# Import list of countries to scrape
country_list <- file.path(dropbox_file_path, "Documentation/google trends/Country by language.xlsx")
country_list <- 
  country_list %>% 
  read_excel()

country_list   <- 
  country_list %>% 
  mutate(Country = stringi::stri_trans_general(Country, "Latin-ASCII") %>% str_to_upper())

data("countries")


countries_of_interest <- 
  countries %>% 
  filter(sub_code == "") %>% 
  left_join(country_list, by = c("name" = "Country")) %>% 
  filter(!is.na(Language))

#filter for portuguese speaking countries
countries_portug <- 
  countries_of_interest %>% 
  filter(Language == "PO")

# Scrape Data ------------------------------------------------------------------

## Initialize dataframe
trends_df_all <- data.frame(NULL)

## Loop through search terms

for(term in c("febre", 
              "covid",
              "coronavirus", 
              "sintomas do coronavirus",
              "tosse",
              "quais são os sintomas do coronavírus",
              "eu tenho coronavírus",
              "Perdi o olfato",
              "Estou com falta de ar",
              "estou com febre",
              "como tratar o coronavírus",
              "número de emergência coronavírus",
              "ajuda do coronavírus",
              "cloroquina")){
      ## Scrape for specific localities
      trends_df <- lapply(1:5, function(i){
        
        print(i)
        
        tryCatch({  
          out <- gtrends(term, 
                         category = "0",
                         geo = c(countries_portug$country_code[i]),
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
#  saveRDS(results_df, file.path(dropbox_file_path, "Data/google_trends/RawData/brazil_extract_crosstate.Rds"))
#  write.csv(results_df, file.path(dropbox_file_path, "Data/google_trends/RawData/brazil_extract_crosstate.csv"), row.names=F)
  