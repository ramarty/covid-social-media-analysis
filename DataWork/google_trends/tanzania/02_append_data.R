# Append Data

# Append Google Data -----------------------------------------------------------
df <- file.path(dropbox_file_path, "Data", "google_trends_tza", "RawData") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  distinct(keyword, date, .keep_all = T)

# Load and Merge English Keyword -----------------------------------------------
keywords <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                              "keywords", "FinalData", "covid_keywords_alllanguages_clean.Rds"))

keywords <- keywords %>%
  dplyr::select(keyword_en, keyword_sw) %>%
  dplyr::mutate(keyword_sw = keyword_sw %>% tolower,
                keyword_en = keyword_en %>% tolower) %>%
  dplyr::rename(keyword = keyword_sw) %>%
  distinct(keyword, .keep_all = T)

df <- merge(df, keywords, by = "keyword", all.x=T, all.y=F)
df$keyword_en[df$language %in% "en"] <- df$keyword[df$language %in% "en"]

# Load/Merge Cases -------------------------------------------------------------
cases <- read.csv(file.path(dropbox_file_path, "Data", "global_admin_data", 
                            "RawData", "WHO-COVID-19-global-data.csv"),
                  stringsAsFactors = F)

cases <- cases %>%
  filter(Country_code %in% "TZ") %>%
  dplyr::select(Date_reported, New_cases, Cumulative_cases, New_deaths, Cumulative_deaths) %>%
  dplyr::rename(date = Date_reported,
                new_cases = New_cases,
                cumulative_cases = Cumulative_cases,
                new_deaths = New_deaths,
                cumulative_deaths = Cumulative_deaths)

df <- merge(df, cases, by = "date", all.x=T,all.y=F)

# Cleanup Hits and 7 Day Average -----------------------------------------------
# Hits character; to numeric
df$hits <- df$hits %>% as.numeric()

df$date <- df$date %>% as.Date()

# 7 day moving average
df <- df %>%
  arrange(date) %>%
  group_by(geo, keyword) %>%
  mutate(hits_ma7 = runMean(hits, n = 7),
         new_cases_ma7 = runMean(new_cases, n = 7),
         new_deaths_ma7 = runMean(new_deaths, n = 7))

# Export -----------------------------------------------------------------------
saveRDS(df, file.path(dropbox_file_path, "Data", "google_trends_tza", "FinalData", "gtrends_tza.Rds"))
write.csv(df, file.path(dropbox_file_path, "Data", "google_trends_tza", "FinalData", "gtrends_tza.csv"), row.names = F)





