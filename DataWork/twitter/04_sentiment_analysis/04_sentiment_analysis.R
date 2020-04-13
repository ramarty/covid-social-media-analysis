# Restrict Tweets to Those in Brazil 
library(lexiconPT)

# Load Data --------------------------------------------------------------------
brazil_tweets <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean.Rds"))

# Tweets Over Time -------------------------------------------------------------
#### All Tweets
brazil_tweets_daysum <- brazil_tweets %>%
    filter(!grepl("rt @", full_text)) %>%
    filter(constant_words %in% T) %>%
    group_by(date) %>%
    summarise(N = n(),
              N_corona = sum(grepl("corona", full_text)),
              N_coronavirus = sum(grepl("coronavirus", full_text)),
              N_covid = sum(grep("covid", full_text)), 
              N_hospital = sum(grepl("hospital", full_text)),
              N_socialdistance = sum(grepl("distância social", full_text)),
              N_mort = sum(grepl("mort", full_text)),
              N_quarentena = sum(grepl("quarentena", full_text)))

#### SUM Words - Long version
brazil_tweets_daysum_long <- brazil_tweets_daysum %>%
    dplyr::select(-N) %>% 
    pivot_longer(
        cols = starts_with("N_"),
        names_to = "word",
        values_to = "count"
    ) %>% 
    mutate(
        word = gsub("N_", "", word)
    )

# Line graph
brazil_tweets_daysum_long %>% 
    filter(!word %in% c("covid", "socialdistance", "mort")) %>%
    ggplot(aes(x = date, y = count)) + 
    geom_line() + 
    facet_wrap(~word) +
    scale_y_continuous(label = comma) + 
    labs(
        x = "Date", 
        y = "Number of tweets",
        title = "Tweets per day in Brazil - Selected words"
    ) + 
    theme_ipsum_rc() + 
    theme(
        panel.grid.minor = element_blank()
    )

brazil_tweets_daysum_long %>% 
    filter(word == "covid",
           date < "2020-03-21") %>%
    ggplot(aes(x = date, y = count)) + 
    geom_line() + 
    scale_y_continuous(label = comma) + 
    labs(
        x = "Date", 
        y = "Number of tweets",
        title = "Tweets per day that include the word 'coronavirus' in Brazil"
    ) + 
    theme_ipsum_rc() + 
    theme(
        panel.grid.minor = element_blank()
    )

#### Sentiments   --------------------------------------------------------------
unnest_words <- brazil_tweets %>%
    filter(!grepl("rt @", full_text)) %>%
    unnest_tokens(word, full_text)

unnest_words %>%
    right_join(get_sentiments("nrc")) %>%
    filter(!is.na(sentiment)) %>%
    count(sentiment, sort = TRUE)

unnest_words %>%
    mutate(word_count = 1:n(),
           index = word_count %/% 500 + 1) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, index = index , sentiment) %>%
    ungroup() %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>% 
    ggplot(aes(index, sentiment)) + 
    geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE)
    


















# Top Words --------------------------------------------------------------------
stopwords_multilang <- c(stopwords::stopwords("en"),
                         stopwords::stopwords("pt"),
                         stopwords::stopwords("es"),
                         "t.co")

grab_phases_for_wordcloud <- function(brazil_tweets){
    # Function that grabs phrases for a word cloud.
    
    brazil_tweets_t <- brazil_tweets %>% filter(!grepl("rt @", full_text))
    
    words   <- brazil_tweets_t$full_text %>% tokenize_words(stopwords = stopwords_multilang)
    ngram2  <- brazil_tweets_t$full_text %>% tokenize_ngrams(n = 2, stopwords = stopwords_multilang)
    ngram3  <- brazil_tweets_t$full_text %>% tokenize_ngrams(n = 3, stopwords = stopwords_multilang)
    
    phrases <- c(
            words %>% unlist(),
            ngram2 %>% unlist(),
            ngram3 %>% unlist()
        ) %>%
        table() %>%
        as.data.frame() %>%
        dplyr::rename(word = ".",
                      freq = Freq) %>%
        mutate(
            word = word %>% as.character()
        )
    
    phrases <- phrases %>%
        filter(freq >= 10) %>%
        filter(word != "t.co") %>%
        filter(!grepl("corona|china|virus|covid|vÃ?rus|brasil", word)) %>%
        filter(nchar(word) >= 4) %>%
        arrange(desc(freq))
    
    return(phrases)
}

phrases_all_tweets  <- grab_phases_for_wordcloud(brazil_tweets)

phrases_quarentena  <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("quarentena", full_text))) %>% filter(word != "quarentena")
phrases_hospital    <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("hospital", full_text)))   %>% filter(word != "hospital")
phrases_carnaval    <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("carnaval", full_text)))   %>% filter(word != "carnaval")
phrases_cancelar    <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("cancelar", full_text)))   %>% filter(word != "cancelar")
phrases_bolsonaro   <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("bolsonaro", full_text)))  %>% filter(!grepl("bolsonaro", word))
