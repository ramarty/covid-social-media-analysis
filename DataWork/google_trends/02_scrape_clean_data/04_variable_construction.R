# Applies equation to make hits comparable across time/states using a comparison
# state

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata.Rds"))

# Moving Averaage --------------------------------------------------------------
## Doesn't work with non-leading NAs, so remove NAs then merge back in
gtrends_ma_df <- gtrends_df %>%
  filter(!is.na(hits)) %>%
  arrange(date) %>%
  group_by(geo, keyword_en) %>%
  mutate(hits_ma7 = runMean(hits, n = 7),
         cases_new_ma7 = runMean(cases_new, n = 7),
         death_new_ma7 = runMean(death_new, n = 7)) %>%
  dplyr::select(geo, date, language, keyword, keyword_en, hits_ma7, cases_new_ma7, death_new_ma7) %>%
  right_join(gtrends_df,
             by = c("geo", "date", "keyword_en", "keyword", "language"))

gtrends_ma_df <- gtrends_ma_df %>%
  mutate(date = date %>% as.Date())

#### Add total cases/deaths 
gtrends_ma_df <- gtrends_ma_df %>%
  group_by(geo) %>%
  mutate(cases_total = max(cases, na.rm = T),
         death_total = max(death, na.rm = T))

# Export -----------------------------------------------------------------------
saveRDS(gtrends_ma_df,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                  "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

