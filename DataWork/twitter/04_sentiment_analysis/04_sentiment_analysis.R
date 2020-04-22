#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Restrict Tweets to Those in Brazil 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Load Data --------------------------------------------------------------------
brazil_tweets <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean.Rds")) %>% 
    filter(!str_detect(location, "indonesia"),
           lang %in% c("en", "pt", "es"))

# 2. Tweets Over Time -------------------------------------------------------------

    #### 2.1. Daily sum: All Tweets 
    brazil_tweets_daysum <- brazil_tweets %>%
        filter(constant_words %in% T) %>%
        mutate(
            # Get an indicator for the quantity of RTs and No RTs per day
            rts = ifelse(grepl("rt @", full_text), "RT", "No RT") 
        ) %>% 
        group_by(date, rts) %>%
        summarise(N = n(),
                  N_corona = sum(grepl("corona", full_text)),
                  N_coronavirus = sum(grepl("coronavirus", full_text)),
                  N_covid = sum(grepl("covid", full_text)), 
                  N_hospital = sum(grepl("hospital", full_text)),
                  N_socialdistance = sum(grepl("distância social", full_text)),
                  N_mort = sum(grepl("mort", full_text)),
                  N_quarentena = sum(grepl("quarentena", full_text)))
    
    ####2.2. SUM Words - Long version - All Tweets
    brazil_tweets_daysum_long <- brazil_tweets_daysum %>%
        dplyr::select(-N) %>% 
        pivot_longer(
            cols = starts_with("N_"),
            names_to = "word",
            values_to = "count"
        ) %>% 
        mutate(
            word = gsub("N_", "", word),
            word = str_to_title(word)
        )

# 3. Overal Trends: Graphs ----------------------------------------------------------
    
    #### 3.1 Corona, coronavirus, Social Distance, Mort    
        #### 3.1.1 Combined rts and no rts summary
        brazil_tweets_daysum_long %>% 
            filter(!word %in% c("Covid", "Socialdistance", "Mort")) %>%
            group_by(rts) %>% 
            ggplot(aes(x = date, y = count, color = rts)) + 
            geom_line(size = 1) + 
            facet_wrap(~word) +
            scale_y_continuous(label = comma) + 
            labs(
                x = NULL, 
                y = "# of Tweets",
                color = NULL,
                title = "Tweets per day in Brazil that include the word:",
                subtitle = "Selected words"
            ) + 
            theme_ipsum_rc() + 
            theme(
                panel.grid.minor = element_blank(),
            )
        
        ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_selected_words_combined.png"), 
               dpi = 400, height = 6, width = 10)
    
        #### 3.1.2.  Separate RTs and No RTs
        rts <- c("RT", "No RT")
        
        for(i in rts){
            print(i)
            
            print(
                brazil_tweets_daysum_long %>% 
                    filter(!word %in% c("Covid", "Socialdistance", "Mort"),
                           rts != i) %>%
                    ggplot(aes(x = date, y = count, color = word)) + 
                    geom_line(size = 1) + 
                    facet_wrap(~word) +
                    scale_y_continuous(label = comma) + 
                    labs(
                        x = NULL, 
                        y = "# of Tweets",
                        color = NULL,
                        title = "Tweets per day in Brazil that include the word:",
                        subtitle = "Selected words"
                    ) + 
                    theme_ipsum_rc() + 
                    theme(
                        panel.grid.minor = element_blank(),
                        legend.position = "none"
                    )
            )
            
            ggsave(filename = file.path(brazil_twitter_figures_path, paste0("tweets_selected_words_", i, ".png")), 
                   dpi = 400, height = 6, width = 10)
        }

    #### 3.2: Line graph = COVID
        #### 3.2.1 
        brazil_tweets_daysum_long %>% 
            filter(word == "Covid") %>%
            ggplot(aes(x = date, y = count, color = rts)) + 
            geom_line(size = 1) + 
            scale_y_continuous(label = comma) + 
            labs(
                x = NULL, 
                y = "# of Tweets",
                color = NULL,
                title = "Tweets that mention the word COVID in Brazil"
            ) + 
            theme_ipsum_rc() + 
            theme(
                panel.grid.minor = element_blank()
            )
        
        ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_covid.png"), 
               dpi = 400, height = 6, width = 10)

    #### 3.3: All words one graphs
        #### 3.2.1 
        brazil_tweets_daysum_long %>% 
            filter(word != "Socialdistance") %>%
            ggplot(aes(x = date, y = count, color = rts)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = as.numeric(ymd("2020-02-26")),
                       linetype = "dashed", colour="black") + 
            geom_vline(xintercept = as.numeric(ymd("2020-03-17")),
                       linetype = "dashed", colour="black") + 
            facet_wrap(~word, scales = "free_y", ncol = 1) +
            scale_y_continuous(label = comma) + 
            labs(
                x = NULL, 
                y = "# of Tweets",
                color = NULL,
                title = "Tweets per day in Brazil that include the word:",
                caption = "Notes: Vertical dashed lines represents the date of the\n1st confirmed case (Feb, 26th) and 1st death (March, 17th) in Brazil."
            ) + 
            theme_ipsum_rc() + 
            theme(
                panel.grid.minor = element_blank()
            )
        
        ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_main_words_rts_divided.png"), 
               dpi = 500, height = 15, width = 10, scale = 0.7)
        
        #### 3.2.1 
        brazil_tweets_daysum_long %>% 
            filter(word != "Socialdistance") %>%
            group_by(date, word) %>% 
            summarise(count = sum(count)) %>% 
            ggplot(aes(x = date, y = count)) + 
            geom_line(size = 1) + 
            geom_vline(xintercept = as.numeric(ymd("2020-02-26")),
                       linetype = "dashed", colour="black") + 
            geom_vline(xintercept = as.numeric(ymd("2020-03-17")),
                       linetype = "dashed", colour="black") + 
            facet_wrap(~word, scales = "free_y", ncol = 1) +
            scale_y_continuous(label = comma) + 
            labs(
                x = NULL, 
                y = "# of Tweets",
                color = NULL,
                title = "Tweets per day in Brazil that include the word:",
                caption = "Notes: Vertical dashed lines represents the date of the\n1st confirmed case (Feb, 26th) and 1st death (March, 17th) in Brazil."
            ) + 
            theme_ipsum_rc() + 
            theme(
                panel.grid.minor = element_blank()
            )
        
        ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_main_words_all.png"), 
               dpi = 500, height = 15, width = 10, scale = 0.7)
        
# 4. Text mining
    # 4.1 Get only text from the tweets 
    tweets_text <- brazil_tweets %>% 
        mutate(
            tweet_id = row_number()
        ) %>% 
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
        filter(!word %in% c("rt", "pra", "é", "tá", "sjgtzxmbpv", "vinistupido", "ta", 
                            "vcs", "pq", "aí", "pq", "itu", "1", "2", "3", "4", "5",
                            "di", "dan")) %>% 
        # Remove accents in all words for simplicity
        mutate(
            word = stri_trans_general(str = word, id = "Latin-ASCII"),
            word = case_when(word == "19" ~ "covid19", 
                             word == "covid" ~ "covid19", 
                             word == "brazil" ~ "brasil", 
                             TRUE ~ word)
        )
    
    # 4.4. Top words for the whole period
    top_words <- unnest_words %>%
        count(word, sort = TRUE) %>%
        mutate(word = fct_reorder(word, n)) %>%
        head(20)
    
    # 4.5 Graph: common words

    top_words %>%
        ggplot(aes(word, n)) +
        geom_col() +
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
           dpi = 400, height = 10, width = 10)  
    
    # 4.6 Correlation across words
    words_filtered <- unnest_words %>%
        dplyr::select(-date) %>%
        add_count(word) %>%
        filter(n >= 10000)
    
    occurences <- words_filtered %>%
        group_by(word) %>%
        summarize(occurences = n())
    
    top_word_cors  <- words_filtered %>% 
        pairwise_cor(word, tweet_id, sort = TRUE) %>%
        head(175)
    
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
        geom_col() + 
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
        geom_bar(stat = "identity") +
        scale_y_continuous(label = comma) + 
        labs(
            x = "", 
            y = "Number of tweets",
            fill = "Sentiment", 
            title = "Tweets per day with negative and positive sentiments in Brazil",
            subtitle = "No data for the date: 2/23/2020"
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
        
    sentiments_counts %>% 
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
            title = "Words that contribute to positive and negative sentiment in Brazil"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_top10_negative_positive.png"), 
           dpi = 400, height = 6, width = 10)
    
    ## No "Virus" word
    sentiments_counts %>% 
        filter(word != "virus") %>% 
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
            title = "Words that contribute to positive and negative sentiment in Brazil"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_top10_negative_positive_no_virus.png"), 
           dpi = 400, height = 6, width = 10)

# 5. Hashtags analaysis   ------------------------------------------------------------
    # List of hashtags
    hashtags <- c("fiqueemcasa",
                  "covid19",
                  "coronavirus",
                  "vaipassar",
                  "distanciasalva",
                  "euficoemcasa",
                  "isolamentosocial",
                  "sus",
                  "profissionaisdesaúde",
                  "medicos",
                  "medicas",
                  "enfermeiros", 
                  "enfermeiras",
                  "mascara")
    
    # Filter tweets with a specific hashtag
    ht_dfs <- data.frame()
    
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
    
    # Grouped GGPLOT
    ht_dfs %>% 
        filter(variable %in% c("coronavirus", "covid19", "sus")) %>%
        mutate(
            variable = paste0("#", variable)
        ) %>% 
        group_by(date, variable) %>% 
        count(date, sentiment) %>% 
        ggplot(aes(x = date, y = n, fill = sentiment)) + 
        geom_bar(stat = "identity", width = 1) +
        scale_y_continuous(label = comma) + 
        facet_wrap(~variable) + 
        labs(
            x = "", 
            y = "Number of tweets",
            fill = "Sentiment", 
            title = "Tweets per day with negative and positive sentiments in Brazil that included the HT:"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, "tweets_sentiments_hashtags.png"), 
           dpi = 400, height = 6, width = 10)
    
    # INDIVIDUAL PLOTS for selected hashtags
    for(term in c("coronavirus", "sus", 
                  "covid19", "mascara", 
                  "fiqueemcasa", "medicos", "enfermeiros")) {
        
        print(term)
        
        print(
            ht_dfs %>% 
                filter(variable == term) %>%
                mutate(
                    variable = paste0("#", variable)
                ) %>% 
                group_by(date, variable) %>% 
                count(date, sentiment) %>% 
                ggplot(aes(x = date, y = n, fill = sentiment)) + 
                geom_bar(stat = "identity", width = 1) +
                scale_y_continuous(label = comma) + 
                facet_wrap(~variable, scales = "free") + 
                labs(
                    x = "", 
                    y = "Number of tweets",
                    fill = "Sentiment", 
                    title = "Tweets per day with negative and positive sentiments in Brazil that included the HT:"
                ) + 
                theme_ipsum_rc() + 
                theme(
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom"
                )
        )
        
        ggsave(filename = file.path(brazil_twitter_figures_path, paste0("tweets_sentiments_hashtags_", term, ".png")), 
               dpi = 400, height = 6, width = 10)
    }

# 6. Top Words --------------------------------------------------------------------
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
            filter(!grepl("corona|china|virus|covid|virus|brasil", word)) %>%
            filter(nchar(word) >= 4) %>%
            arrange(desc(freq))
        
        return(phrases)
    }
    
    phrases_all_tweets  <- grab_phases_for_wordcloud(brazil_tweets)
    
    phrases_quarentena  <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("quarentena", full_text))) %>% filter(word != "quarentena")
    phrases_coronavirus <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("coronavirus", full_text)))   %>% filter(word != "coronavirus")
    phrases_hospital    <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("hospital", full_text)))   %>% filter(word != "hospital")
    phrases_carnaval    <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("carnaval", full_text)))   %>% filter(word != "carnaval")
    phrases_cancelar    <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("cancelar", full_text)))   %>% filter(word != "cancelar")
    phrases_bolsonaro   <- grab_phases_for_wordcloud(brazil_tweets %>% filter(grepl("bolsonaro", full_text)))  %>% filter(!grepl("bolsonaro", word))


    # GGPLOT: Top words when said: 
    phrase_plot <- function(data, title){
        
        data %>% 
            filter(word != "vírus") %>% 
            head(10) %>% 
            ggplot(aes(x = fct_reorder(word,freq), y = freq)) +
            geom_col(fill = "darkslategrey", color = "black") +
            coord_flip() + 
            labs(
                x = NULL, 
                y = "Number of references", 
                title = title
            ) +
            theme_ipsum_rc() + 
            theme(
                panel.grid.minor = element_blank(),
                legend.position = "none"
            )
    }
    
    quarentena  <- phrase_plot(phrases_quarentena,  "quarentena")
    coronavirus <- phrase_plot(phrases_coronavirus, "coronavirus")
    hospital    <- phrase_plot(phrases_hospital,    "hospital")
    carnaval    <- phrase_plot(phrases_carnaval,    "carnaval")
    cancelar    <- phrase_plot(phrases_cancelar,    "cancelar")
    bolsonaro   <- phrase_plot(phrases_bolsonaro,   "Bolsonaro")
    
    quarentena + coronavirus + hospital + bolsonaro + carnaval + cancelar +
        plot_annotation(
            title = 'Top words mentioned when tweets include the word:', 
            theme = theme(
                        plot.title = element_text(size = 20, family = "Roboto Condensed", face = "bold")
                    )
        )

    ggsave(filename = file.path(brazil_twitter_figures_path, paste0("tweets_top_words_if_said.png")), 
               dpi = 400, height = 10, width = 15)


# 7. Relationship between words: n-grams ---------------------------------------
    #### Bigram
    tweets_bigrams <- tweets_text %>%
        unnest_tokens(word, full_text, token = "ngrams", n = 2)
    
    tweets_bigrams_separated <- tweets_bigrams %>% 
        separate(word, c("word1", "word2"), sep = " ")
    
    
    tweets_bigrams_filtered <- tweets_bigrams_separated %>% 
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>% 
        filter(!word1 %in% stopwords_multilang_df$word) %>%
        filter(!word2 %in% stopwords_multilang_df$word) %>% 
        filter(!word1 %in% c("rt", "pra", "é", "tá", "sjgtzxmbpv", "vinistupido", "ta", 
                            "vcs", "pq", "aí", "pq", "paulo")) %>% 
        filter(!word2 %in% c("rt", "pra", "é", "tá", "sjgtzxmbpv", "vinistupido", "ta", 
                            "vcs", "pq", "aí", "pq", "paulo")) 
    
    tweets_bigrams_united <- tweets_bigrams_filtered %>% 
        unite(bigram, word1, word2, sep = " ")
    
    ## CORONA: Words preceded by: something
    corona_words <- tweets_bigrams_separated %>% 
        filter(word1 == "corona",
               !word2 %in% c("virus", "beer")) %>% 
        inner_join(sentiments, by = c(word2 = "word")) %>% 
        count(word2, sentiment, sort = TRUE)
    
    corona_words %>% 
        filter(!word2 %in% c("lover")) %>% 
        mutate(value = ifelse(sentiment == "Negative", -1, 1),
               contribution = n * value) %>% 
        arrange(desc(abs(contribution))) %>% 
        head(30) %>% 
        mutate(word2 = reorder(word2, contribution)) %>% 
        ggplot(aes(x = word2, y = n * value, fill = sentiment)) +
        geom_col(show.legend = FALSE) + 
        coord_flip() + 
        labs(
            x = NULL, 
            y = "Sentiment value times number of occurences",
            fill = "Sentiment", 
            title = "Words preceded by \"corona\""
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, paste0("tweets_words_preceded_corona.png")), 
           dpi = 400, height = 6, width = 10)

    ## QUARENTENA, HOSPITAL: Words preceded by: something
    preceded_words <- tweets_bigrams_separated %>% 
        filter(word1 %in% c("quarentena", "hospital"), 
               !word2 %in% c("virus", "beer")) %>% 
        inner_join(sentiments, by = c(word2 = "word")) %>% 
        count(word1, word2, sentiment, sort = TRUE)
    
    preceded_words %>% 
        group_by(word1) %>% 
        top_n(15) %>% 
        mutate(value = ifelse(sentiment == "Negative", -1, 1),
               contribution = n * value) %>% 
        ungroup() %>% 
        mutate(word2 = reorder_within(word2, contribution, word1),
               word1 = str_to_title(word1)) %>% 
        ggplot(aes(x = word2, y = n * value, fill = sentiment)) +
        geom_col(show.legend = FALSE) + 
        coord_flip() + 
        scale_x_reordered() +
        scale_y_continuous(expand = c(0,0)) +
        facet_wrap(~word1, scales = "free_y") + 
        labs(
            x = NULL, 
            y = "Sentiment value times number of occurences",
            fill = "Sentiment", 
            title = "Top 15 words preceded by:"
        ) + 
        theme_ipsum_rc() + 
        theme(
            panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold"),
            legend.position = "bottom",

        )
    
    ggsave(filename = file.path(brazil_twitter_figures_path, paste0("tweets_words_preceded_hos_qua.png")), 
           dpi = 400, height = 6, width = 10)
