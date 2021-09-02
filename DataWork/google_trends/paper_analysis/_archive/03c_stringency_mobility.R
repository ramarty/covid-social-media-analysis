
# 1. Load / Prep Data ----------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

gtrends_df$geo_name <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "iso.name.en")

gtrends_df %>%
  dplyr::filter(keyword_en %in% "boredom") %>%
  dplyr::filter(geo %in% "US") %>%
  ggplot() +
  geom_line(aes(x = date,
                y = StringencyIndex))

felm(hits_ma7 ~ StringencyIndex | geo | 0 | date, data = gtrends_df[gtrends_df$keyword_en %in% "boredom",]) %>%
  summary()
felm(hits_ma7 ~ gmobility_workplaces_percent_change_from_baseline | geo | 0 | date, data = gtrends_df[gtrends_df$keyword_en %in% "boredom",]) %>%
  summary()
