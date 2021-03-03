# Global Blog Stats

begin_day_i <- "2020-02-01"

# gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
#                                 "global_with_refstate",
#                                 paste0("gl_gtrends_ref","US","_adj_cases_cor_since_",begin_day_i,".Rds")))

cor_max_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",begin_day_i,".Rds")))

## Number of countries
cor_max_df$geo %>% unique() %>% length()

cor_max_df <- cor_max_df %>%
  filter(type %in% "Cases") 

cor_max_df %>%
  filter(keyword_en %in% "fever") %>%
  filter(cor > 0.8) %>%
  nrow()

cor_collpased_df <- cor_max_df %>%
  group_by(keyword_en) %>%
  dplyr::summarise(N = n(),
                   lag_mean = median(lag),
                   lag_mean_adj = mean(lag > -21),
                   cor_mean = mean(cor),
                   cor_p25 = quantile(cor, .25),
                   cor_p5 = quantile(cor, .5),
                   cor_p75 = quantile(cor, .75),
                   cor_min = min(cor),
                   cor_max = max(cor))





