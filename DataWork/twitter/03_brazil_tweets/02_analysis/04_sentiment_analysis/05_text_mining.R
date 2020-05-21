#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Restrict Tweets to Those in Brazil 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Load Data --------------------------------------------------------------------
brazil_tweets <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean.Rds")) %>% 
    filter(!str_detect(location, "indonesia"),
           lang %in% c("en", "pt", "es"))

# 4. Text mining ------------------------------------------------------------------
    # 4.1 Get only text from the tweets 
    tweets_text <- brazil_tweets %>% 
        mutate(
            tweet_id = row_number()
        ) %>% 
        dplyr::select(tweet_id, date, user_description, full_text, retweet_count)
    
    tweets_text <- tweets_text %>% 
        mutate(
            # Get an indicator for the quantity of RTs and No RTs per day
            rts = ifelse(grepl("rt @", full_text), "RT", "No RT"),
            # Remove the usernames if a tweets is a retweet
            text = ifelse(rts == "RT", str_replace_all(full_text, "^[^:]+:", ""), full_text),
            # Remove accents
            text = stri_trans_general(str = text, id = "Latin-ASCII"),
        )
    
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
        unnest_tokens(word, text) %>% 
        anti_join(stop_words, by = "word") %>% 
        anti_join(stopwords_multilang_df, by = "word") %>% 
        # Filter words that are not related or usernames that could make an impact on the count
        # Notes: Some of them were removed before by str_trans_general but just in case I have added them again
        filter(!word %in% c("rt", "pra", "é", "tá", "sjgtzxmbpv", "vinistupido", "ta", 
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
    
    # 4.4. Top words for the whole period
    top_words <- unnest_words %>%
        count(word, sort = TRUE) %>%
        filter(!word %in% c("brasil", "brazil")) %>% 
        mutate(word = fct_reorder(word, n)) %>%
        head(20)
    
    # 4.5 Graph: common words
    top_words %>%
        ggplot(aes(word, n)) +
        geom_col(color = "black", fill = "#999999") +
        coord_flip() +
        scale_y_continuous(label = comma) + 
            labs(
                x = NULL, 
                y = "Count",
                color = NULL,
                title = "Top 20 common words in Tweets in Brazil",
                subtitle = "From January 21st, 2020 to April 10th, 2020"
            ) + 
            theme_ipsum_rc() + 
            theme(
                panel.grid.minor = element_blank()
            )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_common_words.png"), 
           dpi = 400, height = 6, width = 10)  
    
    # 4.6 Correlation across words
    words_filtered <- unnest_words %>%
        filter(!word %in% c("video", "eh")) %>% 
        dplyr::select(-date) %>%
        add_count(word) %>%
        filter(n >= 10000)
    
    occurences <- words_filtered %>%
        group_by(word) %>%
        summarize(occurences = n())
    
    top_word_cors  <- words_filtered %>% 
        pairwise_cor(word, tweet_id, sort = TRUE) %>%
        head(200)
    
    vertices <- occurences %>%
        filter(word %in% top_word_cors$item1 |
                   word %in% top_word_cors$item2)
    
    # Set seed
    set.seed(139196775)
    
    # Top words by correlation
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
            title = "Cluster of words that appear the most in tweets from Brazil",
            subtitle = "Figure only includes words with more than 10,000 mentions",
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

    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_network.png"), 
           dpi = 400, height = 6, width = 10) 
    
    # 4.7 Correlation graph
    top_word_cors %>% 
        distinct(correlation, .keep_all = TRUE) %>% 
        unite("pair", item1, item2, sep = "-") %>% 
        mutate(pair = fct_reorder(pair, correlation)) %>% 
        head(10) %>% 
        ggplot(aes(x = correlation, y = pair)) + 
        geom_col(color = "black", fill = "#999999") +
        scale_x_continuous(limits = c(0,1),
                           expand = c(0,0)) +
        labs(
            y = "Pair of words", 
            x = "Correlation",
            title = "Top 10 word pairs with the highest correlation in Tweets of Brazil"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank()
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_correlation.png"), 
           dpi = 400, height = 6, width = 10)        
    
# 4. Sentiments   --------------------------------------------------------------

    ## 4.1. Load PT lexicon from the lexiconPT Package
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
            
    ## Join with words that include sentiments
    tweets_sentiments <- unnest_words %>%
        right_join(sentiments) %>% 
        filter(!is.na(date)) 
    
    ## Tweets sentiments gr
    tweets_sentiments %>% 
        group_by(date) %>% 
        count(date, sentiment) %>% 
        ggplot(aes(x = date, y = n, fill = sentiment)) + 
        geom_bar(stat = "identity", width = 1) +
        scale_y_continuous(label = comma) + 
        labs(
            x = "", 
            y = "Number of words",
            fill = "Sentiment", 
            title = "Negative and positive sentiments in Brazil",
            subtitle = "Jan 21 - Apr 10",
            caption = "Notes: No data for the date: 2/23/2020"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_sentiments.png"), 
           dpi = 400, height = 6, width = 10)
    
    ## Most common positive and negative words
    sentiments_counts <- tweets_sentiments %>% 
        count(word, sentiment, sort = TRUE) %>% 
        ungroup()  

    ## No "Virus" word
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
            x = "", 
            y = "Contribution to sentiment",
            title = "Most common positive and negative words in tweets from Brazil"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_top10_negative_positive_no_virus.png"), 
           dpi = 400, height = 6, width = 10)

# 5. Sentiments with specific words  ----------------------------------------------------------
    # 5.1 List of words
    hashtags <- c("isolamento|isolation",
                  "coronavirus|corona|covid19",
                  "vaipassar|vai passar",
                  "confinamento|lockdown",
                  "euficoemcasa|stayathome|stay at home|eu fico em casa|ficar em casa",
                  "sus",
                  "quarentena|quarantine",
                  "medicos|medicas|enfermeiros|enfermeiras|frontline workers|linha da frente",
                  "mascara|mask",
                  "renda basica|renda básica",
                  "ventiladores|ventilators")
    
    # 5.2 Filter tweets with a specific hashtag
    # Create empty frame
    ht_dfs <- data.frame()
    
    # Loop
    for(term in hashtags){
        print(term)
        
        ht_df <- tweets_text %>% 
            filter(grepl(term, full_text)) %>% 
            unnest_tokens(word, full_text) %>% 
            anti_join(stop_words, by = "word") %>% 
            anti_join(stopwords_multilang_df, by = "word") %>% 
            filter(!word %in% c("rt", "pra", "é", "tá", "sjgtzxmbpv", "vinistupido", "ta", 
                                "vcs", "pq", "aí", "pq", "paulo")) %>% 
            right_join(sentiments) %>% 
            filter(!is.na(date)) %>% 
            mutate(
                variable = paste0(term)
            )
        
        ht_dfs <- bind_rows(ht_dfs, ht_df)
    }
    
    # Clean the new dataframe
    ht_dfs <- ht_dfs %>% 
        mutate(
            variable = case_when(variable == "isolamento|isolation"~"isolamento", 
                                 variable == "coronavirus|corona|covid19" ~ "coronavirus",
                                 variable == "quarentena|quarantine" ~ "quarentena",
                                 variable == "vaipassar|vai passar" ~ "vai passar",
                                 variable == "confinamento|lockdown" ~ "confinamento", 
                                 variable == "euficoemcasa|stayathome|stay at home|eu fico em casa|ficar em casa" ~ "eu fico em casa",
                                 variable == "medicos|medicas|enfermeiros|enfermeiras|frontline workers|linha da frente" ~ "medicos",
                                 variable == "mascara|mask" ~ "mascara",
                                 variable == "renda basica|renda básica" ~ "renda basica",
                                 variable == "ventiladores|ventilators" ~ "ventiladores",
                                 TRUE ~ variable)
        )
    
    # 5.3 Figures
    # Coronavirus, isolamento, and quarentena
    ht_dfs %>% 
        filter(variable %in% c("coronavirus", "isolamento", "quarentena")) %>%
        mutate(
            variable = str_to_title(variable)
        ) %>% 
        group_by(date, variable) %>% 
        count(date, sentiment) %>% 
        ggplot(aes(x = date, y = n, fill = sentiment)) + 
        geom_bar(stat = "identity", width = 1) +
        scale_y_continuous(label = comma) + 
        facet_wrap(~variable, scales = "free", nrow = 1) + 
        labs(
            x = "", 
            y = "Number of words",
            fill = "Sentiment", 
            title = "Negative and positive sentiments in Brazil that included the word:",
            subtitle = "Jan 21 - Apr 10",
            caption = "Notes: No data for the date: 2/23/2020"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_sentiments_corona_isolamento_quarentena.png"), 
           dpi = 400, height = 6, width = 12)
    
    # Renda basica, mascara, ventiladores.
    ht_dfs %>% 
        filter(variable %in% c("renda basica", "mascara", "ventiladores")) %>%
        mutate(
            variable = str_to_title(variable)
        ) %>% 
        group_by(date, variable) %>% 
        count(date, sentiment) %>% 
        ggplot(aes(x = date, y = n, fill = sentiment)) + 
        geom_bar(stat = "identity", width = 1) +
        scale_y_continuous(label = comma) + 
        facet_wrap(~variable, scales = "free_y", nrow = 1) + 
        labs(
            x = "", 
            y = "Number of tweets",
            fill = "Sentiment", 
            title = "Tweets per day with negative and positive sentiments in Brazil that included the word:"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_sentiments_mascara_renda_ventiladores.png"), 
           dpi = 400, height = 6, width = 12)
    
    
    # Frontline workers ---------------
    frontline <- tweets_text %>% 
        mutate(
            # All words to lower case
            user = str_to_lower(user_description),
            # Remove accents
            user = stri_trans_general(str = user, id = "Latin-ASCII")
        ) %>% 
        dplyr::select(tweet_id, text, user, date)
    
    # words related to frontline workers    
    frontline_words <- c("enfermeiro|enfermeira|medico|medica|doctor|doctora|nurse|frontline|frontline workers|linha da frente")
    
    # Filter those tweets with 
    frontline_tweets <- frontline %>% 
        filter(str_detect(user, frontline_words)) 
    
    frontline_text <- frontline_tweets %>%  
        unnest_tokens(word, text) %>% 
        anti_join(stop_words, by = "word") %>% 
        anti_join(stopwords_multilang_df, by = "word") %>% 
        filter(!word %in% c("rt", "pra", "é", "tá", "sjgtzxmbpv", "vinistupido", "ta", 
                            "vcs", "pq", "aí", "pq", "itu", "1", "2", "3", "4", "5",
                            "di", "dan")) %>% 
        right_join(sentiments) %>% 
        filter(!is.na(date)) 
    
    frontline_text %>% 
        group_by(date) %>% 
        count(date, sentiment) %>% 
        ggplot(aes(x = date, y = n, fill = sentiment)) + 
        geom_bar(stat = "identity") +
        scale_y_continuous(label = comma) + 
        labs(
            x = "", 
            y = "Number of tweets",
            fill = "Sentiment", 
            title = "Tweets from Frontline workers in Brazil: negative and positive sentiments",
            subtitle = "No data for the date: 2/23/2020"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_frontline_workers_sentiments.png"), 
           dpi = 400, height = 6, width = 12)
    
    
    # List of hashtags
    hashtags <- c("isolamento|isolation",
                  "euficoemcasa|stayathome|stay at home|eu fico em casa|ficar em casa",
                  "sus",
                  "quarentena|quarantine",
                  "mascara|mask",
                  "cama de hospital|hospital|emergencia",
                  "ventiladores|ventilators")
    
    # Filter tweets with a specific hashtag
    ht_dfs <- data.frame()
    
    for(term in hashtags){
        print(term)
        
        ht_df <- frontline_tweets %>% 
            filter(str_detect(text, term)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words, by = "word") %>% 
            anti_join(stopwords_multilang_df, by = "word") %>% 
            filter(!word %in% c("rt", "pra", "é", "tá", "sjgtzxmbpv", "vinistupido", "ta", 
                                "vcs", "pq", "aí", "pq", "itu", "1", "2", "3", "4", "5",
                                "di", "dan")) %>% 
            right_join(sentiments) %>% 
            filter(!is.na(date)) %>% 
            mutate(
                variable = paste0(term)
            )
        
        ht_dfs <- bind_rows(ht_dfs, ht_df)
    }
    
    ht_dfs <- ht_dfs %>% 
        mutate(
            variable = case_when(variable == "mascara|mask" ~ "mascara",
                                 variable == "isolamento|isolation" ~ "isolamento",
                                 variable == "quarentena|quarantine" ~ "quarentena",
                                 variable == "euficoemcasa|stayathome|stay at home|eu fico em casa|ficar em casa" ~ "eu fico em casa",
                                 variable == "cama de hospital|hospital|emergencia" ~ "hospital", 
                                 variable == "ventiladores|ventilators" ~ "ventiladores",
                                 TRUE ~ variable)
        )
    
    ht_dfs %>% 
        filter(variable %in% c("mascara", "ventiladores")) %>%
        mutate(
            variable = str_to_title(variable)
        ) %>% 
        group_by(date, variable) %>% 
        count(date, sentiment) %>% 
        ggplot(aes(x = date, y = n, fill = sentiment)) + 
        geom_bar(stat = "identity", width = 1) +
        facet_wrap(~variable, scales = "free_y") + 
        labs(
            x = "", 
            y = "Number of words",
            fill = "Sentiment", 
            title = "Negative and positive sentiments in Brazil from frontline workers that included the word:",
            subtitle = "Jan 21 - Apr 10",
            caption = "Notes: No data for the date: 2/23/2020\nMascara=mascara, máscara, mask\nVentiladores=ventiladores,ventilators"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_frontline_workers_sentiments_mask_ventiladores.png"), 
           dpi = 400, height = 6, width = 12)
    
    ht_dfs %>% 
        filter(variable %in% c("mascara", "hospital", "ventiladores")) %>%
        mutate(
            variable = str_to_title(variable)
        ) %>% 
        group_by(date, variable) %>% 
        count(date, sentiment) %>% 
        ggplot(aes(x = date, y = n, fill = sentiment)) + 
        geom_bar(stat = "identity", width = 1) +
        facet_wrap(~variable, scales = "free_y") + 
        labs(
            x = "", 
            y = "Number of words",
            fill = "Sentiment", 
            title = "Negative and positive sentiments in Brazil from frontline workers that included the word:",
            subtitle = "Jan 21 - Apr 10",
            caption = "Notes: No data for the date: 2/23/2020\nMascara=mascara, máscara, mask\nVentiladores=ventiladores,ventilators\nHospital=cama de hospital, hospital, emergencia"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_frontline_workers_sentiments_mask_hospital_ventiladores.png"), 
           dpi = 400, height = 6, width = 12)
    
    ht_dfs %>% 
        filter(variable %in% c("quarentena", "isolamento", "eu fico em casa")) %>%
        mutate(
            variable = str_to_title(variable)
        ) %>% 
        group_by(date, variable) %>% 
        count(date, sentiment) %>% 
        ggplot(aes(x = date, y = n, fill = sentiment)) + 
        geom_bar(stat = "identity", width = 1) +
        facet_wrap(~variable, scales = "free_y") + 
        labs(
            x = "", 
            y = "Number of words",
            fill = "Sentiment", 
            title = "Negative and positive sentiments in Brazil from frontline workers that included the word:",
            subtitle = "Jan 21 - Apr 10",
            caption = "Notes: No data for the date: 2/23/2020\nQuarentena=quarentena, quarentine\nIsolamento=isolamento, isolation\nEu fico em casa = euficoemcasa, stayathome, stay at home, eu fico em casa, ficar em casa"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_frontline_workers_sentiments_quarentena_iso_casa.png"), 
           dpi = 400, height = 6, width = 12)
    