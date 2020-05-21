# Restrict Tweets to Those in Brazil 

# Make Brazil Regex Search -----------------------------------------------------
tweets_df <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "usa_sd_tweets_appended.Rds"))
tweets_df$uid <- 1:nrow(tweets_df)

# Date -------------------------------------------------------------------------
tweets_df <- tweets_df %>%
  mutate(created_at = created_at %>% parse_date_time(orders = "%a %b %d %H:%M:%S %z %Y")) %>%
  mutate(date = created_at %>% date())

# Add States -------------------------------------------------------------------
usa_adm <- readRDS(file.path(dropbox_file_path, "Data", "GADM", "RawData", "gadm36_USA_1_sp.rds"))

usa_adm$state <- usa_adm$NAME_1
tweets_df$state <- ""
for(i in 1:nrow(usa_adm)){
  print(i)
  
  usa_adm_i <- usa_adm[i,]
  
  names <- usa_adm_i$NAME_1 %>% tolower() 
  abbrevs <- usa_adm_i$VARNAME_1 %>% str_split("\\|") %>% unlist() %>% tolower() %>% str_replace_all("[[:punct:]]", "") %>% str_squish() %>% unique()
  search_words <- c(names, abbrevs)
  
  usa_regex_i <- paste0("\\b", search_words, "\\b") %>% paste(collapse = "|")
  
  tweets_df$state[grepl(usa_regex_i, tweets_df$usa_str)] <- usa_adm$state[i]
  tweets_df$state[grepl("washington dc", tweets_df$usa_str)] <- "District of Columbia"
  
}

# Sentiment Score --------------------------------------------------------------
senti_words <- get_sentiments(lexicon = c("bing"))
senti_words <- senti_words %>%
  mutate(sentiment = case_when(sentiment == "negative" ~ -1,
                               sentiment == "positive" ~ 1)) %>%
  filter(!(word %in% c("virus", "trump")))

stop_words <- stopwords::stopwords("en") %>%
  as.data.frame() %>%
  dplyr::rename(word = ".") %>%
  mutate(word = word %>% as.character()) 

#### Sentiment variables
# Large dataset; do in chunks
chunk_size <- 500
starts <- seq(from = 1, to = nrow(tweets_df), by=chunk_size)

senti_df <- lapply(starts, function(start_i){
  print(start_i)
  
  end_i <- min((start_i + chunk_size - 1), nrow(tweets_df))
  
  tweet_sentiment <- tweets_df[start_i:end_i,] %>%
    dplyr::select(uid, full_text) %>%
    unnest_tokens(word, full_text) %>% 
    anti_join(stop_words, by = "word") %>% 
    left_join(senti_words, by="word") %>%
    group_by(uid) %>%
    summarise(senti_avg = mean(sentiment, na.rm=T),
              senti_pos_N = sum(sentiment[sentiment == 1], na.rm=T),
              senti_neg_N = sum(sentiment[sentiment == -1], na.rm=T),
              senti_words_neg = paste(word[sentiment %in% -1], collapse = ";"),
              senti_words_pos = paste(word[sentiment %in% 1], collapse = ";")) %>%
    ungroup()
  
  return(tweet_sentiment)
}) %>%
  bind_rows()

tweets_df <- merge(tweets_df, senti_df, by="uid")

saveRDS(tweets_df, file.path(dropbox_file_path, "Data", "twitter", "FinalData", "usa_tweets", "rds", "usa_sd_tweets_appended_clean.Rds"))






