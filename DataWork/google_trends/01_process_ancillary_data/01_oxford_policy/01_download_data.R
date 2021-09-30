# Download latest data from Oxford Policy Response Tracker

# Read data from OxCGRT Githib -------------------------------------------------
ox_df <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")

# Cleanup ----------------------------------------------------------------------
ox_clean_df <- ox_df %>%
  dplyr::mutate(Date = Date %>% as.character() %>% ymd(),
                geo = countrycode(CountryCode, origin = "iso3c", destination = "iso2c")) %>%
  dplyr::mutate(geo = case_when(
    CountryCode == "RKS" ~ "XK",
    TRUE ~ geo
  )) %>%
  dplyr::rename(date = Date,
                country = CountryName) %>%
  dplyr::select(-CountryCode)

# Export data ------------------------------------------------------------------
saveRDS(ox_clean_df, file.path(oxpol_dir, "RawData", "OxCGRT_latest.Rds"))




