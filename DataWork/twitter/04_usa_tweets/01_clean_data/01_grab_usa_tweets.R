# Restrict Tweets to Those in Brazil 

OVERRIDE_FILES <- F

# Make Brazil Regex Search -----------------------------------------------------
usa_adm <- readRDS(file.path(dropbox_file_path, "Data", "GADM", "RawData", "gadm36_USA_1_sp.rds"))

names <- usa_adm$NAME_1 %>% tolower() 
abbrevs <- usa_adm$VARNAME_1 %>% str_split("\\|") %>% unlist() %>% tolower() %>% str_replace_all("[[:punct:]]", "") %>% str_squish() %>% unique()
search_words <- c(names, abbrevs)

usa_regex <- paste0("\\b", search_words, "\\b") %>% paste(collapse = "|")

# Load Data --------------------------------------------------------------------
file_names <- list.files(file.path(dropbox_file_path, "Data", "twitter", "RawData", "rds"), pattern = "*.Rds") %>% rev()

#file_name_i <- file_names[300]

temp <- lapply(file_names, function(file_name_i){
  
  print(file_name_i)
  
  #### Check if file already cleaned
  file_already_exists <- file.exists(file.path(dropbox_file_path, "Data", "twitter", "IntermediateData", "usa_tweets", "rds", paste0("usa_", file_name_i)))
  
  if(file_already_exists %in% F | OVERRIDE_FILES){
    #### Load
    tweets_df <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "RawData", "rds", file_name_i))
    
    #### Remove if no location
    tweets_df$location <- tweets_df$location %>% as.character()
    tweets_df <- tweets_df[!(tweets_df$location %in% ""),]
    tweets_df <- tweets_df[!is.na(tweets_df$location),]
    
    tweets_df$location <- tweets_df$location %>% tolower()
    
    tweets_df <- tweets_df %>%
      mutate(location = location %>% tolower %>% str_replace_all("[[:punct:]]", "")) %>%
      mutate(usa = grepl(usa_regex, location)) %>%
      filter(usa %in% T) %>% 
      dplyr::select(-usa) %>%
      
      ## Identify State
      # extracts the first one
      mutate(usa_str = str_extract(location, usa_regex)) 
      
    # If "washington dc", chooses washington; override
    tweets_df$usa_str[grepl("\\bdc\\b", tweets_df$usa_str)] <- "dc"
  
    #### Save
    #print(nrow(tweets_df))
    saveRDS(tweets_df, file.path(dropbox_file_path, "Data", "twitter", "IntermediateData", "usa_tweets", "rds", paste0("usa_", file_name_i)))
  }
  
})



