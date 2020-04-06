# Restrict Tweets to Those in Brazil 

# Load Data --------------------------------------------------------------------
file_names <- list.files(file.path(dropbox_file_path, "Data", "twitter", "RawData", "rds"), pattern = "*.Rds")

i <- 1
tweet_summary <- lapply(file_names, function(file_name_i){
  print(i)
  
  tweets_df <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "RawData", "rds", file_name_i))
  
  tweets_sum_df <- tweets_df %>%
    group_by(lang) %>%
    summarise(N = n()) %>%
    mutate(date_time = file_name_i %>% str_replace_all("coronavirus-tweet-id-|.Rds", "") %>% paste0("-00-00") %>% ymd_hms())
  
  i <<- i + 1
  
  return(tweets_sum_df)
}) %>% bind_rows()

# Export
saveRDS(tweet_summary, file.path(dropbox_file_path, "Data", "twitter", "FinalData", "tweets_all_summary", "tweets_all_summary_lang.Rds"))
