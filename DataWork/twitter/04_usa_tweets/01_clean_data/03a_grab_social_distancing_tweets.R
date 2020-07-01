# Restrict Tweets to Those in Brazil 

# Make Brazil Regex Search -----------------------------------------------------
usa_files <- file.path(dropbox_file_path, "Data", "twitter", "IntermediateData", "usa_tweets", "rds") %>%
  list.files(pattern = "*.Rds",
             full.names = F) 

usa_files_i <- usa_files[1]
temp <- lapply(usa_files, function(usa_files_i){
  
  print(usa_files_i)
  
  df <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "IntermediateData", "usa_tweets", "rds", usa_files_i))
  df <- df[grepl("social distan|socialdistan", df$full_text),]
  df <- df[df$lang %in% "en",]
  saveRDS(df, file.path(dropbox_file_path, "Data", "twitter", "IntermediateData", "usa_tweets", "rds_social_distance", usa_files_i))
  
})



