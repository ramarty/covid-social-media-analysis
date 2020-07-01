# Restrict Tweets to Those in Brazil 

i <- 1
readRDS_update <- function(file){
  print(i)
  i <<- i + 1
  out <- readRDS(file)
  return(out)
}

frontline <- list.files(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "global_frontline_workers", "individual_files"),
           full.names = T) %>%
  map_df(readRDS_update)

saveRDS(frontline, file.path(dropbox_file_path, "Data", "twitter", "FinalData", "global_frontline_workers", "frontline_tweets.Rds"))

