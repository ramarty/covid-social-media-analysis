# Vaccine figures

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean_complete.Rds"))

gtrends_df <- gtrends_df %>%
  dplyr::filter(!is.na(days_since_v1_vaccine_1) |
                  !is.na(days_since_v1_vaccine_1) |
                  !is.na(days_since_v2_vaccine_2) |
                  !is.na(days_since_v2_vaccine_2) |
                  !is.na(days_since_v2_vaccine_2))

gtrends_df %>%
  dplyr::filter(keyword_en %in% "boredom") %>%
  dplyr::filter(abs(days_since_v1_vaccine_2) <= 60) %>%
  ggplot(aes(x = days_since_v1_vaccine_2,
             y = hits_ma7)) +
  geom_line() +
  geom_vline(xintercept = 0,
             color = "red") +
  facet_wrap(~country)




