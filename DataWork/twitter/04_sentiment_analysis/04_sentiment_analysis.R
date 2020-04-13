# Restrict Tweets to Those in Brazil 

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
              N_hospital = sum(grepl("hospital", full_text)),
              N_socialdistance = sum(grepl("distância social", full_text)),
              N_mort = sum(grepl("mort", full_text)),
              N_quarentena = sum(grepl("quarentena", full_text)))

brazil_tweets_daysum %>% 
    ggplot() +
    geom_line(aes(x=date, y=N_quarentena))



brazil_tweets %>%
    filter(!grepl("rt @", full_text))
