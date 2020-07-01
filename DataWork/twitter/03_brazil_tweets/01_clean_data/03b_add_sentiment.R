# Restrict Tweets to Those in Brazil 

# Load Brazil Data -------------------------------------------------------------
brazil_tweets <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean.Rds")) %>% 
  filter(!str_detect(location, "indonesia"),
         lang %in% c("en", "pt", "es")) %>%
  mutate(tweet_id = row_number())
  
# Unnest Tweets ----------------------------------------------------------------  
# 4.1 Get only text from the tweets 
tweets_text <- brazil_tweets %>% 
  dplyr::select(tweet_id, full_text, date)

# 4.2 Eliminate stop words
stopwords_multilang <- c(stopwords::stopwords("en"),
                         stopwords::stopwords("pt"),
                         stopwords::stopwords("es"),
                         "t.co")

stopwords_multilang_df <- as.data.frame(stopwords_multilang) %>% 
  janitor::clean_names() %>% 
  rename(word = stopwords_multilang) %>% 
  mutate(
    word = as.character(word)
  )

# 4.3 Unnest tweets: Words per tweets
unnest_words <- tweets_text %>%
  unnest_tokens(word, full_text) %>% 
  anti_join(stop_words, by = "word") %>% 
  anti_join(stopwords_multilang_df, by = "word") %>% 
  filter(!word %in% c("rt", "pra", "?", "t?", "sjgtzxmbpv", "vinistupido", "ta", 
                      "vcs", "pq", "a?", "pq", "itu", "1", "2", "3", "4", "5",
                      "di", "dan")) %>% 
  # Remove accents in all words for simplicity
  mutate(
    word = stri_trans_general(str = word, id = "Latin-ASCII"),
    word = case_when(word == "19" ~ "covid19", 
                     word == "covid" ~ "covid19", 
                     word == "brazil" ~ "brasil", 
                     TRUE ~ word)
  )


# Load/Prep Sentiment Data -----------------------------------------------------

## Load PT lexicon from the lexiconPT Package
data("sentiLex_lem_PT02") 

# Combine portuguese and english words 
sentiments <- get_sentiments("nrc") %>% 
  mutate(term = word) %>% 
  bind_rows(sentiLex_lem_PT02) %>% 
  mutate(sentiment = case_when(polarity == -1 ~ "negative",
                               polarity == 1 ~ "positive",
                               polarity == 0 ~ "neutral",
                               TRUE ~ as.character(sentiment)),
         word = ifelse(is.na(word), term, word)
  ) %>% 
  dplyr::select(word, sentiment) %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  mutate(
    sentiment = str_to_title(sentiment)
  )

# Add Sentiments to Tweets -----------------------------------------------------

## Join with words that include sentiments
tweets_sentiments <- unnest_words %>%
  right_join(sentiments) %>%
  mutate(sentiment_val = case_when(sentiment == "Negative" ~ -1,
                                   sentiment == "Positive" ~ 1)) %>%
  group_by(tweet_id, date) %>%
  summarise(sentiment_val = mean(sentiment_val),
            sentiment_Npos = sum(sentiment == "Positive"),
            sentiment_Nneg = sum(sentiment == "Negative")) %>%
  ungroup() %>%
  mutate(sentiment_posneg = case_when(sentiment_val > 0 ~ "Positive",
                                      sentiment_val == 0 ~ "Neutral",
                                      sentiment_val < 0 ~ "Negative")) %>%
  dplyr::select(-date)
  
brazil_tweets <- brazil_tweets %>%
  left_join(tweets_sentiments, by="tweet_id")

saveRDS(brazil_tweets, file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean_sentiment.Rds")) 
  