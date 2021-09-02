# gTrends hits built across multiple timeseries. Here, we confirm it creates
# a consistent time series, and produce a couple figures demonstrating it works

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends.Rds"))

gtrends_df %>%
  dplyr::filter(geo %in% "US",
                keyword_en %in% "loss of smell") %>%
  ggplot() +
  geom_line(aes(x = date,
                y = hits_t3_adj,
                color = "Consistent"),
            size = 1) +
  geom_line(aes(x = date,
                y = hits_t1,
                color = "t1"),
            size = 0.4) +
  geom_line(aes(x = date,
                y = hits_t2,
                color = "t2"),
            size = 0.4) +
  geom_line(aes(x = date,
                y = hits_t3,
                color = "t3"),
            size = 0.4)

