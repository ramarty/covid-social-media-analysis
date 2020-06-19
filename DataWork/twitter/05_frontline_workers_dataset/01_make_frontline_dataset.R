# Restrict Tweets to Those in Brazil 

frontline_terms <- c("doctor", "nurse", 
                     "medico", "medica",
                     "médico", "médica",
                     "enfermeiro", "enfermeira", 
                     "md", "physician") 
frontline_terms <- paste0("\\b", frontline_terms, "\\b") %>% paste(collapse = "|")

# Load Data --------------------------------------------------------------------
file_names <- list.files(file.path(dropbox_file_path, "Data", "twitter", "RawData", "rds"), pattern = "*.Rds")

i <- 1
df_appended <- lapply(file_names %>% rev(), function(file_name_i){
  print(paste(i, file_name_i))
  
  tweets_df <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "RawData", "rds", file_name_i))
  
  ## Remove retweets
  tweets_df <- tweets_df[!grepl("^rt", tweets_df$full_text),]
  
  ## Extract frontline
  tweets_df$user_description <- tweets_df$user_description %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_squish()
  tweets_df <- tweets_df[grepl(frontline_terms, tweets_df$user_description),]
  
  i <<- i + 1
  print(nrow(tweets_df))
  
  saveRDS(tweets_df, file.path(dropbox_file_path, "Data", "twitter", "FinalData", "global_frontline_workers", "individual_files", file_name_i))
  
  return(NA)
}) %>%
  bind_rows()


