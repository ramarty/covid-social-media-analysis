# Produce files of relevant information in tweets

# Reads raw tweet information and subsets information to relevant variables.
# Produces CSV and Rds files for each date/hour. 

#### Create list of tweet json files
json_files <- lapply(c("2020-03"), function(yyyy_mm){
  list.files(file.path(covid_twitter_github, yyyy_mm),
             full.names = T,
             pattern = ".jsonl.gz")
}) %>% unlist() 

#### Clean Tweets
temp <- lapply(json_files, function(json_path_i){
  
  print(json_path_i)
  
  #### Clean File Name
  file_name_i <- json_path_i %>%
    str_replace_all(".*/", "") %>%
    str_replace_all(".jsonl.gz", "")
  
  #### Check if the file has already been cleanred
  cleaned_already <- file.exists(
    file.path(dropbox_file_path, "Data", "twitter", "RawData", "rds", paste0(file_name_i, ".Rds"))
  )
  
  #### If not cleaned already, then clean
  if(!(cleaned_already)){
    
    #### Load tweets
    tweets_df <- stream_in(file(json_path_i))
    
    #### Subset to select variables
    tweets_subset <- 
      data.frame(
        full_text = tweets_df$full_text,
        location = tweets_df$user$location,
        lang = tweets_df$lang,
        created_at = tweets_df$created_at,
        retweet_count = tweets_df$retweet_count,
        favorite_count = tweets_df$favorite_count,
        screen_name = tweets_df$user$screen_name
      ) 
    
    #### Check if coordinates exist
    coordinates_exist <- FALSE
    is_error = tryCatch({
      
      # If no coordinates, this will return an error
      temp <- tweets_df$coordinates$coordinates
      coordinates_exist <- TRUE
      
    },
    error=function(e) return(NA))
    
    #### If coordinates exist and is not null, add coordinates
    if(coordinates_exist & !is.null(tweets_subset$coordinates$coordinates)){
      tweets_subset$coordinates = tweets_subset$coordinates$coordinates %>%
        as.character() %>%
        na_if("NULL") %>%
        str_replace_all("\\(|c|\\)", "")
      
      tweets_subset <- tweets_subset %>%
        separate(coordinates, c("longitude", "latitude"), sep=", ") %>%
        mutate(latitude = latitude %>% as.numeric(),
               longitude = longitude %>% as.numeric())
    }
    
    #### Export
    saveRDS(tweets_subset, file.path(dropbox_file_path, "Data", "twitter", "RawData", "rds", paste0(file_name_i, ".Rds")))
    #write.csv(tweets_subset, file.path(dropbox_file_path, "Data", "twitter", "RawData", "csv", paste0(file_name_i, ".csv")), row.names = F)
  }
  
})









