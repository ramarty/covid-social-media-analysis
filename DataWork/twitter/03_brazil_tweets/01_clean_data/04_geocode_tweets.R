# Restrict Tweets to Those in Brazil 

# Make Brazil Regex Search -----------------------------------------------------
brazil_tweets <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended.Rds"))
brazil_adm3 <- readRDS(file.path(dropbox_file_path, "Data", "GADM", "RawData", "gadm36_BRA_3_sp.rds"))

# Cleaning ---------------------------------------------------------------------
#### Create variables
brazil_tweets <- brazil_tweets %>%
  mutate(created_at = created_at %>% parse_date_time(orders = "%a %b %d %H:%M:%S %z %Y")) %>%
  mutate(date = created_at %>% date())

#### Clean tweets
stopwords <- paste0("\\b",
                    stopwords(kind = "pt"),
                    paste0("\\b")) %>%
  paste(collapse = "|")

brazil_tweets$full_text <- brazil_tweets$full_text %>%
  tolower() %>%
  str_replace_all("http[[:alnum:]]*", "") %>% # Remove links
  #str_replace_all(stopwords, "") %>% # Remove stop words
  str_squish()

#### Determine Location
brazil_adm3$NAME_1 <- brazil_adm3$NAME_1 %>% tolower()
brazil_tweets$name_1 <- ""

for(name_1 in unique(brazil_adm3$NAME_1)){
  print(name_1)
  brazil_tweets$name_1[grepl(name_1, brazil_tweets$location)] <- name_1
}

#### Constant
terms_restrict <- c("Coronavirus", 
                    "Koronavirus",
                    "Corona",
                    "CDC",
                    "Wuhancoronavirus",
                    "Wuhanlockdown",
                    "Ncov",
                    "Wuhan",
                    "N95",
                    "Kungflu",
                    "Epidemic",
                    "outbreak",
                    "Sinophobia",
                    "China") %>%
  tolower() %>%
  paste(collapse = "|")

brazil_tweets <- brazil_tweets %>%
  mutate(constant_words = full_text %>% str_detect(terms_restrict))

# Export -----------------------------------------------------------------------
saveRDS(brazil_tweets, file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean.Rds"))


