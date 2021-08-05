# What variables explain correlation?

# Load Data --------------------------------------------------------------------
cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries", "correlation_datasets",
                               "correlations_gtrends_otherdata_varclean_since2020-02-01.Rds"))

cor_df <- cor_df %>%
  mutate(cases_per_pop = cases_total / population)

cor_cases_df <- cor_df %>%
  filter(type %in% "Cases")

cor_cases_df$cor[cor_cases_df$population >= 10000000] %>% summary()
cor_cases_df$cor[cor_cases_df$population < 10000000] %>% summary()

lm(cor ~ population, data = cor_cases_df) %>% summary()

