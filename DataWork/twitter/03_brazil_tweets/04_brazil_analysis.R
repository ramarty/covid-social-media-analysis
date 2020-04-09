# Restrict Tweets to Those in Brazil 

# Load Data --------------------------------------------------------------------
brazil_tweets <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean.Rds"))

# Tweets Over Time -------------------------------------------------------------
#### All Tweets
brazil_tweets_daysum <- brazil_tweets %>%
  #filter(date <= "2020-03-02") %>%
  filter(!grepl("rt @", full_text)) %>%
  filter(constant_words %in% T) %>%
  group_by(date) %>%
  summarise(N = n(),
            N_corona = sum(grepl("corona", full_text)),
            N_hospital = sum(grepl("hospital", full_text)),
            N_socialdistance = sum(grepl("distância social", full_text)),
            N_mort = sum(grepl("mort", full_text)),
            N_quarentena = sum(grepl("quarentena", full_text)))

ggplot() +
  geom_line(data=brazil_tweets_daysum, aes(x=date, y=N_quarentena))

#### By Location
brazil_tweets_daysum_name1 <- brazil_tweets %>%
  filter(date >= "2020-01-29") %>%
  #filter(date <= "2020-03-02") %>%
  filter(!grepl("rt @", full_text)) %>%
  filter(constant_words %in% T) %>%
  filter(name_1 != "") %>%
  group_by(date, name_1) %>%
  summarise(N = n(),
            N_corona = sum(grepl("corona", full_text)),
            N_hospital = sum(grepl("hospital", full_text)),
            N_casa = sum(grepl("casa", full_text)),
            N_gripe = sum(grepl("gripe", full_text)),
            N_socialdistance = sum(grepl("distância social", full_text)),
            N_mort = sum(grepl("mort", full_text)),
            N_quarentena = sum(grepl("quarentena", full_text)))

p_regions_N <- ggplot() +
  geom_line(data=brazil_tweets_daysum_name1 %>%
              filter(name_1 %in% c("rio de janeiro",
                                   "são paulo",
                                   "distrito federal",
                                   "minas gerais",
                                   "rio grande do sul")), 
            aes(x=date, y=N, group=name_1, color=name_1),
            size=1) +
  labs(x ="",
       y= "",
       title = "Tweets Across Select Regions: All Tweets",
       color="Region") +
  theme_minimal()
p_regions_N
ggsave(p_regions_N, filename = file.path(brazil_twitter_figures_path, "trends_regions_N.png"), height=3, width=8)


p_regions_quarentena <- ggplot() +
  geom_line(data=brazil_tweets_daysum_name1 %>%
              filter(name_1 %in% c("rio de janeiro",
                                   "são paulo",
                                   "distrito federal",
                                   "minas gerais",
                                   "rio grande do sul")), 
            aes(x=date, y=N_quarentena, group=name_1, color=name_1),
            size=1) +
  labs(x ="",
       y= "",
       title = "Tweets Across Select Regions: Tweets Containing 'Quarentena'",
       color="Region") +
  theme_minimal()
ggsave(p_regions_quarentena, filename = file.path(brazil_twitter_figures_path, "trends_regions_quarentena.png"), height=3, width=8)

trends_all <- ggarrange(p_regions_N,
          p_regions_quarentena,
          ncol=1,
          labels=c("A","B"),
          legend="right",
          common.legend=T)
ggsave(trends_all, filename = file.path(brazil_twitter_figures_path, "trends_regions_all.png"), height=6, width=12)

# Top Words --------------------------------------------------------------------
stopwords_multilang <- c(stopwords::stopwords("en"),
                         stopwords::stopwords("pt"),
                         stopwords::stopwords("es"),
                         "t.co")

grab_phases_for_wordcloud <- function(brazil_tweets){
  # Function that grabs phrases for a word cloud.
  
  brazil_tweets_t <- brazil_tweets %>%
    filter(!grepl("rt @", full_text))
  
  words <- brazil_tweets_t$full_text %>% tokenize_words(stopwords = stopwords_multilang)
  ngram2 <- brazil_tweets_t$full_text %>% tokenize_ngrams(n=2, stopwords = stopwords_multilang)
  ngram3 <- brazil_tweets_t$full_text %>% tokenize_ngrams(n=3, stopwords = stopwords_multilang)
  
  phrases <- c(
               words %>% unlist(),
               ngram2 %>% unlist(),
               ngram3 %>% unlist()) %>%
    table() %>%
    as.data.frame() %>%
    dplyr::rename(word = ".",
                  freq = Freq) %>%
    mutate(word = word %>% as.character())
  
  phrases <- phrases %>%
    filter(freq >= 10) %>%
    filter(word != "t.co") %>%
    filter(!grepl("corona|china|virus|covid|vírus|brasil", word)) %>%
    filter(nchar(word) >= 4) %>%
    arrange(desc(freq))
  
  return(phrases)
}

phrases_all_tweets <- grab_phases_for_wordcloud(brazil_tweets)

phrases_quarentena <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("quarentena", full_text))) %>% filter(word != "quarentena")
phrases_hospital <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("hospital", full_text))) %>% filter(word != "hospital")
phrases_carnaval <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("carnaval", full_text))) %>% filter(word != "carnaval")
phrases_cancelar <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("cancelar", full_text))) %>% filter(word != "cancelar")
phrases_bolsonaro <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("bolsonaro", full_text))) %>% filter(!grepl("bolsonaro", word))

#### Word Cloud - All Tweets
png(file.path(brazil_twitter_figures_path, "alltweets_wordcloud.png"), height=600, width=600, res = 150)
wordcloud::wordcloud(words = phrases_all_tweets$word,
          freq = phrases_all_tweets$freq^1.4,
          max.words=200,
          random.order=T,
          rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()

p_quarentena <- ggplot() +
  geom_col(data=phrases_quarentena[1:30,], aes(x=reorder(word, freq), y=freq),
           fill="dodgerblue4") +
  coord_flip() +
  labs(x="", y ="Number of references", title="If tweet includes quarentena,\npeople also say...") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=12, face="bold"))

p_hospital <- ggplot() +
  geom_col(data=phrases_hospital[1:30,], aes(x=reorder(word, freq), y=freq),
           fill="dodgerblue4") +
  coord_flip() +
  labs(x="", y ="Number of references", title="If tweet includes hospital,\npeople also say...") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=12, face="bold"))

p_carnaval <- ggplot() +
  geom_col(data=phrases_carnaval[1:30,], aes(x=reorder(word, freq), y=freq),
           fill="dodgerblue4") +
  coord_flip() +
  labs(x="", y ="Number of references", title="If tweet includes carnaval,\npeople also say...") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=12, face="bold"))

p_bolsonaro <- ggplot() +
  geom_col(data=phrases_bolsonaro[1:30,], aes(x=reorder(word, freq), y=freq),
           fill="dodgerblue4") +
  coord_flip() +
  labs(x="", y ="Number of references", title="If tweet includes bolsonaro,\npeople also say...") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=12, face="bold"))

p_word_all <- ggarrange(p_quarentena,
          p_hospital,
          p_carnaval,
          p_bolsonaro,
          nrow=1)
ggsave(p_word_all, filename = file.path(brazil_twitter_figures_path, "top_words_if_said.png"), height=12, width=16)

#### If mention word, what are they saying




  #filter(full_text %>% str_detect("isolar")) %>%
  dplyr::select(full_text) 

brazil_tweets_t_word <- brazil_tweets_t %>% 
  unnest_tokens(word, full_text)

brazil_tweets_t_2gram <- brazil_tweets_t %>% 
  unnest_tokens(word, full_text, token="ngrams", n=2)

brazil_tweets_t_3gram <- brazil_tweets_t %>% 
  unnest_tokens(word, full_text, token="ngrams", n=3)
  
phrase_freq <- c(brazil_tweets_t_word$word,
  brazil_tweets_t_2gram$word,
  brazil_tweets_t_3gram$word) %>% table() %>%
  as.data.frame()

phrase_freq <- phrase_freq %>%
  filter(Freq > 10) %>%
  arrange(desc(Freq))



a <- brazil_tweets$full_text %>% unique() %>% tokens(remove_punct=T)
a <- a %>% tokens_ngrams(n = 1:3)


brazil_tweets_t$word %>% table() %>% as.data.frame() %>% View()



a <- analyzeSentiment(brazil_tweets$full_text[1:20])

w <- brazil_tweets_t$word %>%
  table() %>%
  as.data.frame()


tweet_tokens <- tokens(brazil_tweets$full_text, remove_punct = TRUE)
dfmat_irish <- dfm(toks_irish)



long <- brazil_tweets[1:100000,] %>%
  unnest_tokens(word, full_text)

freq_terms(text, 3)


# Time trends words ------------------------------------------------------------




brazil_tweets_daysum$


brazil_tweets$retweet_count