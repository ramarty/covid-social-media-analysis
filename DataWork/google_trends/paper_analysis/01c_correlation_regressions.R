# What variables explain correlation?

# Load Data --------------------------------------------------------------------
cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "correlation_datasets",
                               "correlations_gtrends_since2020-01-01.Rds")) %>%
  dplyr::mutate(date_since = "2020-01-01")

# Prep Data --------------------------------------------------------------------
cor_df <- cor_df %>%
  dplyr::filter(type %in% "Cases") %>%
  mutate(cases_pc = cases_total / population)

cor_smell_df <- cor_df[cor_df$keyword_en %in% "loss of smell",]

lm(cor_nolag ~ factor(language), data = cor_smell_df) %>% 
  summary()

cor_smell_df$language


