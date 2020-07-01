# Restrict Tweets to Those in Brazil 

# Make Brazil Regex Search -----------------------------------------------------
readRDS_fun <- function(file){
  print(file)
  df <- readRDS(file)
  return(df)
}

usa_sd_tweets <- file.path(dropbox_file_path, "Data", "twitter", "IntermediateData", "usa_tweets", "rds_social_distance") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS_fun) 

# Export -----------------------------------------------------------------------
saveRDS(usa_sd_tweets, file.path(dropbox_file_path, "Data", "twitter", "FinalData", "usa_tweets", "rds", "usa_sd_tweets_appended.Rds"))


