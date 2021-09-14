# Applies equation to make hits comparable across time/states using a comparison
# state

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata.Rds"))

# Moving Averaage --------------------------------------------------------------
## Doesn't work with non-leading NAs, so remove NAs then merge back in
gtrends_ma_hits_df <- gtrends_df %>%
  filter(!is.na(hits)) %>%
  arrange(date) %>%
  group_by(geo, keyword_en) %>%
  dplyr::mutate(hits_ma7 = runMean(hits, n = 7)) %>%
  dplyr::select(geo, date, language, keyword, keyword_en, hits_ma7) 

gtrends_df <- gtrends_df %>%
  left_join(gtrends_ma_hits_df, by = c("geo", "date", "keyword_en", "keyword", "language")) 

# Days since lockdown - year agnostic ------------------------------------------
# Calculate year since lockdown, agnostic of year. So April 21, 2020 and April
# 21, 2019 would have the same value

gtrends_df <- gtrends_df %>%
  dplyr::mutate(year = date %>% year,
                c_policy_mm_dd = c_policy %>% substring(6,10)) %>%
  dplyr::mutate(c_policy_yearcurrent = paste0(year, "-", c_policy_mm_dd) %>% ymd()) %>%
  dplyr::mutate(days_since_c_policy_yearcurrent = date - c_policy_yearcurrent) %>%
  dplyr::mutate(days_since_c_policy_yearcurrent_post = days_since_c_policy_yearcurrent >= 0) %>%
  dplyr::select(-c(c_policy_mm_dd))

# Variable Fixes ---------------------------------------------------------------
gtrends_df <- gtrends_df %>%
  mutate(date = date %>% as.Date()) %>%
  
  group_by(geo) %>%
  mutate(cases_total = max(cases, na.rm = T),
         death_total = max(death, na.rm = T)) %>%
  ungroup()

# Other variables --------------------------------------------------------------
gtrends_df <- gtrends_df %>%
  dplyr::mutate(mm_dd = date %>% substring(6,10))

# Export -----------------------------------------------------------------------
saveRDS(gtrends_df,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                  "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))





