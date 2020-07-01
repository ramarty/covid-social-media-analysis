# Restrict Tweets to Those in Brazil 

# Make Brazil Regex Search -----------------------------------------------------
read_subset <- function(file_i){
  print(file_i)
  df <- readRDS(file_i)
  return(df)
}

usa_tweets <- file.path(dropbox_file_path, "Data", "twitter", "IntermediateData", "usa_tweets", "rds") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(read_subset)

# Export -----------------------------------------------------------------------
saveRDS(usa_tweets, file.path(dropbox_file_path, "Data", "twitter", "FinalData", "usa_tweets", "rds", "usa_tweets_appended.Rds"))


