# What variables explain correlation?

keywords_en_use <- c("loss of smell", 
                     "loss of taste",
                     "fever",
                     "ageusia",
                     "anosmia",
                     "i can't smell",
                     "how to treat coronavirus")

# Load Data --------------------------------------------------------------------
cor_01_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "correlation_datasets",
                               "correlations_gtrends_since2020-01-01.Rds")) %>%
  dplyr::mutate(date_since = "2020-01-01")

cor_05_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "correlation_datasets",
                               "correlations_gtrends_since2020-05-01.Rds")) %>%
  dplyr::mutate(date_since = "2020-05-01")

cor_10_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "correlation_datasets",
                               "correlations_gtrends_since2020-10-01.Rds")) %>%
  dplyr::mutate(date_since = "2020-10-01")

# Prep Data --------------------------------------------------------------------
cor_df <- bind_rows(cor_01_df,
                    cor_05_df,
                    cor_10_df) %>%
  dplyr::filter(type %in% "Cases") %>%
  dplyr::filter(keyword_en %in% keywords_en_use) %>%
  group_by(keyword_en) %>%
  dplyr::mutate(cor_since_01 = median(cor_nolag[date_since = "2020-01-01"]))

# Figures ----------------------------------------------------------------------
cor_df %>%
  ggplot() +
  geom_boxplot(aes(y = keyword_en,
                   x = cor,
                   fill = date_since))




