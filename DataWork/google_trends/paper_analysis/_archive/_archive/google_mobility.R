# What variables explain correlation?

# Load Data --------------------------------------------------------------------
gtrends_full_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                     "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

gtrends_full_df %>%
  filter(geo == "IN") %>%
  filter(keyword_en %in% "boredom") %>%
  ggplot() +
  geom_line(aes(x = date,
                y = gmobility_parks_percent_change_from_baseline))

gtrends_full_df %>%
  filter(geo == "IN") %>%
  filter(keyword_en %in% "boredom") %>%
  ggplot() +
  geom_line(aes(x = date,
                y = hits_ma7))


