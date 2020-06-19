# Restrict Tweets to Those in Brazil 

frontline <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "global_frontline_workers", "frontline_tweets.Rds"))

frontline <- frontline[!grepl("^rt", tolower(frontline$full_text)),]

frontline <- frontline %>%
  mutate(full_text = full_text %>%
           tolower() %>%
           str_replace_all("[[:punct:]]", "") %>%
           str_replace_all("\\|", "") %>%
           str_replace_all("via.*", "") %>%
           str_squish()) %>%
  mutate(n_words = str_count(full_text, "\\S+")) %>%
  filter(n_words >= 5) %>%
  
  group_by(screen_name) %>%
  mutate(N_screen_name = n()) %>%
  ungroup() %>%
  filter(N_screen_name <= 300)

i <- 1
top_words <- frontline$full_text %>%
  #sample(size=20000) %>%
  lapply(function(x){ 
    i <<- i + 1
    print(i)
    out <- ngram::ngram_asweka(x, min=3, max=5)
    return(out)
    }
    ) %>%
  unlist %>%
  table() %>%
  as.data.frame() %>%
  dplyr::rename(freq = Freq,
                word = ".") %>%
  filter(freq >= 10)

png(file.path(brazil_twitter_figures_path, paste0("frontline_global", "_wordcloud.png")), height=600, width=600, res = 150)
wordcloud::wordcloud(words = top_words$word,
                     freq = top_words$freq/2,
                     max.words=100,
                     random.order=T,
                     rot.per=0.35,
                     colors=brewer.pal(8, "Dark2"))
dev.off()
