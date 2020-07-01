# Restrict Tweets to Those in Brazil 

# Make Brazil Regex Search -----------------------------------------------------
tweets_df <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "usa_tweets", "rds", "usa_sd_tweets_appended_clean.Rds"))
us_state_io <- state_ranks %>%
  dplyr::select(state, name) %>%
  dplyr::rename(iso = state) %>%
  dplyr::rename(state = name)

tweets_df <- tweets_df %>%
  left_join(us_state_io, by="state")

# Word Cloud -------------------------------------------------------------------
stop_words <- c(stopwords::stopwords("en"), "rt", "social", "distancing", "t.co", 
                "socialdistancing",
                "https",
                "9whhthmdpc",
                "i've") %>%
  as.data.frame() %>%
  dplyr::rename(word = ".") %>%
  mutate(word = word %>% as.character()) 

word_state_df <- tweets_df %>% 
  dplyr::select(state, full_text) %>%
  unnest_tokens(word, full_text) %>% 
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) >= 4) %>%
  group_by(state, word) %>%
  summarise(N = n()) 

word_state_df <- word_state_df %>%
  filter(state != "") %>%
  group_by(state) %>%
  arrange(desc(N)) %>%
  mutate(top_id = 1:n()) %>%
  ungroup() %>%
  filter(top_id <= 20)

p <- ggplot(word_state_df, aes(label = word, size = N)) +
  geom_text_wordcloud() +
  facet_geo(~ state, scales = "free")
ggsave(p, filename = "~/Desktop/test.png", height=18, width=18)



#### Word Cloud
pos_word_unnest <- tweets_df %>%
  dplyr::select(state, senti_words_pos) %>%
  filter(senti_words_pos != "") %>%
  mutate(senti_words_pos = senti_words_pos %>% str_replace_all(";", " ")) %>%
  unnest_tokens(word, senti_words_pos) %>%
  mutate(type = "positive")

neg_word_unnest <- tweets_df %>%
  dplyr::select(state, senti_words_neg) %>%
  filter(senti_words_neg != "") %>%
  mutate(senti_words_neg = senti_words_neg %>% str_replace_all(";", " ")) %>%
  unnest_tokens(word, senti_words_neg) %>%
  mutate(type = "negative")

word_state_df <- bind_rows(pos_word_unnest,
                           neg_word_unnest) %>%
  group_by(word, state, type) %>%
  summarise(N = n()) %>%
  filter(N > 100)

p <- ggplot(word_state_df, aes(label = word, size = N)) +
  geom_text_wordcloud() +
  #scale_size_area(max_size = 40) +
  facet_geo(~ state)
ggsave(p, filename = "~/Desktop/test.png", height=18, width=18)

tweets_df$senti_words_pos

# Line Graph
state_df <- tweets_df %>%
  filter(state != "") %>%
  group_by(state, date) %>%
  summarise(senti = mean(senti_avg, na.rm=T),
            N_pos = sum(senti_avg > 0, na.rm=T),
            N_neg = sum(senti_avg < 0, na.rm=T),
            N = n()) %>%
  mutate(prop = N_pos / N_neg) 


ggplot(state_df, 
       aes(x = date, y = senti)) +
  geom_line() +
  facet_geo(~ state)


state_df <- tweets_df %>%
  group_by(state) %>%
  filter(!is.na(senti_avg)) %>%
  summarise(senti = mean(senti_avg),
            senti_rt = weighted.mean(senti_avg, retweet_count))







