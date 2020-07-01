# Restrict Tweets to Those in Brazil 

# SETTINGS
word <- "quarentena" # quarentena, bolsonaro, covid


# Load Data --------------------------------------------------------------------
brazil_tweets <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean_sentiment.Rds"))
covid <- read.csv(file.path(dropbox_file_path, "Data", "brazil_admin_data", "brazil_covid19_200419.csv"),
                  encoding = "UTF-8")

brazil_tweets <- brazil_tweets[!grepl("^rt", brazil_tweets$full_text),]

for(word in rev(c("noticias", "ajuda", "máscara", "bolsonaro", "hospital", "quarentena","covid","isolamento", "mort", "doente",
              "medo", "pior", "fake", "triste", "guerra", "terror", "preocupado", "cupla",
              "sao", "real", "salvar", "forte", "bom", "verdade", "amigo", "paciente", "capaz"))){
  
  print(paste(word, " -------------------------------------------------------"))
  
  quarentena_tweets <- brazil_tweets %>%
    filter(grepl(word, full_text)) %>%
    filter(!is.na(sentiment_val)) 
  
  if(nrow(quarentena_tweets) < 300) next
  
  quarentena_sum <- quarentena_tweets %>%
    group_by(date) %>%
    summarise(Negative = sum(sentiment_posneg == "Negative", na.rm = T),
              Positive = sum(sentiment_posneg == "Positive", na.rm = T),
              Neutral = sum(sentiment_posneg == "Neutral", na.rm = T)) %>%
    ungroup() %>%
    mutate(prop_pos = Positive / (Positive + Negative))
  
  quarentena_sum_long <- quarentena_sum %>%
    dplyr::select(date, Negative, Positive, Neutral) %>%
    pivot_longer(cols=c(Negative, Positive, Neutral)) 
  
  # Prep Cases -------------------------------------------------------------------
  # Add new cases variable
  covid <- covid %>%
    arrange(date) %>%
    group_by(region, state) %>%
    mutate(cases_new = c(NA, diff(cases))) %>%
    mutate(deaths_new = c(NA, diff(deaths))) %>%
    ungroup()
  
  # N Tweets ---------------------------------------------------------------------
  covid_sum <- covid %>%
    group_by(date) %>%
    summarise(cases_new = sum(cases_new),
              deaths_new = sum(deaths_new),
              cases = sum(cases),
              deaths = sum(deaths)) %>%
    mutate(date = date %>% as.character() %>% as.Date()) %>%
    filter(date <="2020-04-10" %>% as.Date()) %>%
    filter(date >="2020-03-01" %>% as.Date())
  
  quarentena_sum$total <- quarentena_sum$Negative + quarentena_sum$Positive + quarentena_sum$Neutral
  scale <- max(covid_sum$cases_new, na.rm=T) / max(quarentena_sum$total[quarentena_sum$date >= "2020-03-01"], na.rm=T) 
  
  p <- ggplot() +
    geom_col(data = quarentena_sum_long[quarentena_sum_long$date >= "2020-03-01",],
             aes(x=date,
                 y=value, 
                 group=name,
                 fill=name)) +
    geom_line(data = covid_sum,
              aes(x=date, y=cases_new/scale)) + 
    scale_y_continuous(
      
      # Features of the first axis
      name = "Number of Tweets",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*scale, name="New Cases")
    ) +
    labs(x="",
         fill = "Tweets",
         title = word) +
    theme_ipsum()
  
  ggsave(p, filename = file.path(brazil_twitter_figures_path, paste0(word, "_N_tweets_cases.png")), height=5, width=6)
  
  # Top Phrases ------------------------------------------------------------------
  quarentena_tweets <- quarentena_tweets %>%
    mutate(full_text = full_text %>%
             tolower() %>%
             str_replace_all("[[:punct:]]", "") %>%
             str_replace_all("[[:punct:]]", "") %>%
             str_replace_all("\\|", "") %>%
             str_replace_all("via.*", "") %>%
             str_squish()) %>%
    mutate(n_words = str_count(full_text, "\\S+")) %>%
    filter(n_words >= 5)
  
  top_words <- quarentena_tweets$full_text %>%
    lapply(function(x) ngram::ngram_asweka(x, min=3, max=5)) %>%
    unlist %>%
    table() %>%
    as.data.frame() %>%
    dplyr::rename(freq = Freq,
                  word = ".") %>%
    filter(freq >= 10)
  
  png(file.path(brazil_twitter_figures_path, paste0(word, "_wordcloud.png")), height=600, width=600, res = 150)
  wordcloud::wordcloud(words = top_words$word,
                       freq = top_words$freq/2,
                       max.words=100,
                       random.order=T,
                       rot.per=0.35,
                       colors=brewer.pal(8, "Dark2"))
  dev.off()
  
}
