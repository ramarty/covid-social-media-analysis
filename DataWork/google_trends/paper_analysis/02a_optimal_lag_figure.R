# What variables explain correlation?

keywords_en_use <- c("loss of smell", 
                     "loss of taste",
                     "fever",
                     "ageusia",
                     "anosmia",
                     "i can't smell",
                     "how to treat coronavirus")

# Load Data --------------------------------------------------------------------
cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "correlation_datasets",
                               "correlations_gtrends_since2020-01-01.Rds")) %>%
  dplyr::mutate(date_since = "2020-01-01")

cor_df <- cor_df %>%
  dplyr::filter(type %in% "Cases") %>%
  dplyr::filter(cor > 0.5)

cor_df <- cor_df %>%
  dplyr::filter(keyword_en %in% "loss of smell") 




