# Restrict Tweets to Those in Brazil 

# Make Brazil Regex Search -----------------------------------------------------
brazil_tweets <- file.path(dropbox_file_path, "Data", "twitter", "IntermediateData", "brazil_tweets", "rds") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS)

# Export -----------------------------------------------------------------------
saveRDS(brazil_tweets, file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended.Rds"))


