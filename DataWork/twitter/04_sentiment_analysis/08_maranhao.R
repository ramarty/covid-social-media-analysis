#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Restrict Tweets to Those in Brazil 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Load Data -----------------------------------------------------------------
brazil_tweets <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean.Rds")) %>% 
    filter(!str_detect(location, "indonesia"),
           lang %in% c("en", "pt", "es"))

covid <- read.csv(file.path(dropbox_file_path, "Data", "brazil_admin_data", "brazil_covid19_200419.csv"),
                  encoding = "UTF-8")

covid_maranhao <- covid %>% 
    filter(state == "Maranhão") %>% 
    mutate(
        date = ymd(date)
    )

# 2. Text mining ---------------------------------------------------------------
    # 2.1 Get only text from the tweets 
    tweets_text <- brazil_tweets %>% 
        mutate(
            tweet_id = row_number(),
            user_description = str_to_lower(user_description)
        ) %>% 
        dplyr::select(tweet_id, date, user_description, location, full_text, retweet_count)
    
    # 2.2. Create RTs indicator and remove accents
    tweets_text <- tweets_text %>% 
        mutate(
            # Get an indicator for the quantity of RTs and No RTs per day
            rts = ifelse(grepl("rt @", full_text), "RT", "No RT"),
            # Remove the usernames if a tweets is a retweet
            text = ifelse(rts == "RT", str_replace_all(full_text, "^[^:]+:", ""), full_text),
            # Remove accents
            text = stri_trans_general(str = text, id = "Latin-ASCII"),
        )
    
# 3. Creating unnested dataset -------------------------------------------------
    # 3.1 Getting stop words from different languages
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
    
    # 3.2 Unnest tweets: Words per tweets
    unnest_words <- tweets_text %>%
        # Filter location: Maranhão
        filter(str_detect(location, "maranhao|maranhão")) %>% 
        # Unnesting
        unnest_tokens(word, text) %>% 
        # Remove stop words
        anti_join(stop_words, by = "word") %>% 
        anti_join(stopwords_multilang_df, by = "word") %>% 
        # Filter words that are not related or usernames that could make an impact on the count
        filter(!word %in% c("rt", "pra", "é", "tá", "sjgtzxmbpv", "vinistupido", "ta", "ja",
                            "vcs", "pq", "aí", "pq", "itu", "1", "2", "3", "4", "5",
                            "di", "dan")) %>% 
        # Mutate words consistency
        mutate(
            # Remove accents
            word = stri_trans_general(str = word, id = "Latin-ASCII"),
            word = case_when(word == "19" ~ "covid19", 
                             word == "covid" ~ "covid19", 
                             word == "brazil" ~ "brasil", 
                             word == "corona" ~ "coronavirus",
                             TRUE ~ word)
        )
    
# 4. Top words  ----------------------------------------------------------------
    # 4.1 Count top words for the whole period
    top_words <- unnest_words %>%
        count(word, sort = TRUE) %>%
        filter(!word %in% c("brasil", "brazil")) %>% 
        mutate(word = fct_reorder(word, n)) %>%
        head(20)
    
    # 4.2 Top words graph
    top_words %>%
        ggplot(aes(word, n)) +
        geom_col(color = "black", fill = "#999999") +
        coord_flip() +
        scale_y_continuous(label = comma) + 
            labs(
                x = NULL, 
                y = "Count",
                color = NULL,
                title = "Top 20 common words in Tweets in Maranhão, Brazil",
                subtitle = "From January 21st, 2020 to April 10th, 2020"
            ) + 
            theme_ipsum_rc() + 
            theme(
                panel.grid.minor = element_blank()
            )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "maranhao_tweets_common_words.png"), 
            dpi = 400, height = 6, width = 10)  
    
    # 4.3 Correlation across words
    ## Filtered words
    words_filtered <- unnest_words %>%
        dplyr::select(-date) %>%
        add_count(word) %>% 
        filter(n > 20)
    
    # Occurences
    occurences <- words_filtered %>%
        group_by(word) %>%
        summarize(occurences = n())
    
    # Top words by correlations
    top_word_cors  <- words_filtered %>% 
        pairwise_cor(word, tweet_id, sort = TRUE) %>% 
        head(150)
    
    # Verticies for the graph
    vertices <- occurences %>%
        filter(word %in% top_word_cors$item1 |
               word %in% top_word_cors$item2)
    
    # Set seed
    set.seed(139196775)
    
    # Graph: Top words by correlation
    top_word_cors %>%
        graph_from_data_frame(vertices = vertices) %>%
        ggraph(layout = "fr") +
        geom_edge_link() +
        geom_node_point(aes(size = occurences)) +
        geom_node_text(aes(label = name, 
                           family = "Roboto Condensed"), 
                       repel = TRUE) +
        scale_size_continuous(label = comma) + 
        labs(
            x = NULL,
            y = NULL,
            title = "Cluster of words that appear the most in tweets from Maranhão, Brazil",
            size = "# of occurrences"
        ) +
        theme_ipsum_rc() +
        theme(
            legend.position = "bottom",
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
        )

    ggsave(filename = file.path(brazil_twitter_figures_path, "maranhao_tweets_network.png"), 
           dpi = 400, height = 6, width = 10) 
    
    
# 5. Sentiments   --------------------------------------------------------------
    ## 5.1. Load PT lexicon from the lexiconPT Package
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
            
    # 5.2 Join with words that include sentiments
    tweets_sentiments <- unnest_words %>%
        filter(!word %in% c("virus")) %>% 
        right_join(sentiments) %>% 
        filter(!is.na(date)) 
    
    # 5.3 Differences: Positive and negative words
    tweets_sentiments %>% 
        group_by(date, sentiment) %>% 
        tally() %>% 
        pivot_wider(
            names_from = sentiment, 
            values_from = n,
            values_fill = list(n = 0)
        ) %>% 
        janitor::clean_names() %>% 
        mutate(
            diff = positive - negative
        ) %>% 
        ggplot(aes(x = date, y = diff)) + 
        geom_line(color = "red") + 
        geom_vline(xintercept = ymd("2020-03-21"),
                   linetype = "dashed", colour = "black") + 
        geom_vline(xintercept = ymd("2020-03-26"),
                   linetype = "dashed", colour = "black") + 
        geom_vline(xintercept = ymd("2020-04-06"),
                   linetype = "dashed", colour = "black") + 
        labs(
            x = NULL, 
            y = "Positives - negatives words",
            fill = "Sentiment", 
            title = "Positive and negative words differences in Maranhão, Brazil",
            subtitle = "Jan 21 - Apr 10",
            caption = "Notes: Black dotted lines represents 1st case, 10th, case and 100th case confirmed in Maranhão."
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "maranhao_tweets_positive_ninus_negative.png"), 
           dpi = 400, height = 6, width = 10)
    
    # 5.4 Tweets sentiments graph
    tweets_sentiments %>% 
        group_by(date) %>% 
        count(date, sentiment) %>% 
        ggplot(aes(x = date, y = n, fill = sentiment)) + 
        geom_bar(stat = "identity", width = 1) +
        geom_vline(xintercept = ymd("2020-03-21"),
                   linetype = "dashed", colour = "black") + 
        geom_vline(xintercept = ymd("2020-03-26"),
                   linetype = "dashed", colour = "black") + 
        geom_vline(xintercept = ymd("2020-04-06"),
                   linetype = "dashed", colour = "black") + 
        scale_y_continuous(label = comma) + 
        labs(
            x = "", 
            y = "Number of words",
            fill = "Sentiment", 
            title = "Negative and positive sentiments in Maranhão, Brazil",
            subtitle = "Jan 21 - Apr 10",
            caption = "Notes: Black dotted lines represents 1st case, 10th, case and 100th case confirmed in Maranhão."
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "maranhao_tweets_sentiments.png"), 
           dpi = 400, height = 6, width = 10)
    
    # 5.5 Most common positive and negative words
    sentiments_counts <- tweets_sentiments %>% 
        count(word, sentiment, sort = TRUE) %>% 
        ungroup()  

    sentiments_counts %>% 
        filter(!word %in% c("virus", "confirmed", "positivo", "president", "top", "rico", "vao", "usar", "saber", "evitar")) %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n, fill = sentiment)) + 
        geom_col(show.legend = FALSE) + 
        coord_flip() + 
        facet_wrap(~sentiment, scales = "free_y") + 
        scale_y_continuous(label = comma) + 
        labs(
            x = NULL, 
            y = "Contribution to sentiment",
            title = "Most common positive and negative words in tweets from Brazil"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "maranhao_tweets_top10_negative_positive.png"), 
           dpi = 400, height = 6, width = 10)

# 6. Total number of tweets ----------------------------------------------------
    tweets_text %>%
        # Filter location: Maranhão
        filter(str_detect(location, "maranhao|maranhão")) %>% 
        group_by(date, rts) %>%
        tally() %>% 
        ggplot(aes(x = date, y = n, color = rts)) + 
        geom_line(size = 1) +
        labs(
            x = NULL, 
            y = "Frequency",
            color = NULL, 
            title = "Number of tweets per day with location 'Maranhão' in the dataset",
            subtitle = "Total: RTs = 1676, No RTs = 701"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )

ggsave(filename = file.path(brazil_twitter_figures_path, "maranhao_total_tweets.png"), 
           dpi = 400, height = 6, width = 10)
