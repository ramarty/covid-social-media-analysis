# Restrict Tweets to Those in Brazil 

# Load Data --------------------------------------------------------------------
brazil_tweets <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean_sentiment.Rds"))
covid <- read.csv(file.path(dropbox_file_path, "Data", "brazil_admin_data", "brazil_covid19_200419.csv"),
                  encoding = "UTF-8")

# Prep Tweets ------------------------------------------------------------------
brazil_tweets <- brazil_tweets[!grepl("^rt", brazil_tweets$full_text),]

quarentena_tweets <- brazil_tweets %>%
filter(grepl("quarentena", full_text)) %>%
  filter(!is.na(sentiment_val)) 

quarentena_sum <- quarentena_tweets %>%

  group_by(date) %>%
  summarise(Negative = sum(sentiment_posneg == "Negative", na.rm = T),
            Positive = sum(sentiment_posneg == "Positive", na.rm = T)) %>%
  ungroup() %>%
  mutate(prop_pos = Positive / (Positive + Negative))

quarentena_sum_long <- quarentena_sum %>%
  dplyr::select(date, Negative, Positive) %>%
  pivot_longer(cols=c(Negative, Positive)) 

# N Tweets ---------------------------------------------------------------------
covid_sum <- covid %>%
  group_by(date) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>%
  mutate(date = date %>% as.character() %>% as.Date()) %>%
  filter(date <="2020-04-10" %>% as.Date()) %>%
  filter(date >="2020-03-01" %>% as.Date())

p <- ggplot() +
  geom_col(data = quarentena_sum_long[quarentena_sum_long$date >= "2020-03-01",],
           aes(x=date,
               y=value, 
               group=name,
               fill=name)) +
  geom_line(data = covid_sum,
            aes(x=date, y=cases/150)) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Number of Tweets",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*150, name="Cases")
  ) +
  labs(x="",
       fill = "Tweets",
       title = "quarentena") +
  theme_ipsum()

ggsave(p, filename = file.path(brazil_twitter_figures_path, "N_tweets_cases.png"), height=5, width=6)

# Prop Tweets ---------------------------------------------------------------------
p <- ggplot() +
  geom_line(data = quarentena_sum[quarentena_sum$date >= "2020-03-01",],
           aes(x=date,
               y=prop_pos),
           color="blue4") +
  geom_line(data = covid_sum,
            aes(x=date, y=cases/20000),
            color="red4") + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Proportion Positive Tweets",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*20000, name="Cases")
  ) +
  labs(x="",
       fill = "Tweets",
       title = "quarentena") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = "blue4", size=13),
    axis.title.y.right = element_text(color = "red4", size=13)
  )

ggsave(p, filename = file.path(brazil_twitter_figures_path, "prop_tweets_cases.png"), height=5, width=6)


