# Example Trends

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                "correlations_gtrends_since2020-01-01_until2021-07-31.Rds"))

gtrends_losssmell_df <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "loss of smell",
                type %in% "Cases") %>%
  mutate()



lm1 <- lm(cor_nolag ~ log(cases_total), data = gtrends_losssmell_df)
lm2 <- lm(cor_nolag ~ per_pop_using_internet, data = gtrends_losssmell_df)
lm3 <- lm(cor_nolag ~ mobile_cell_sub_per100, data = gtrends_losssmell_df)
lm4 <- lm(cor_nolag ~ factor(income), data = gtrends_losssmell_df)
lm5 <- lm(cor_nolag ~ log(cases_total) + per_pop_using_internet + mobile_cell_sub_per100 + factor(income), data = gtrends_losssmell_df)

stargazer(lm1,
          lm2,
          lm3,
          lm4,
          lm5)


lm(cor_nolag ~ factor(income) + log(cases_total) + per_pop_using_internet + mobile_cell_sub_per100, data = gtrends_losssmell_df) %>%
  summary()

gtrends_sub_df$income %>% table()

gtrends_sub_df %>%
  ggplot() +
  geom_point(aes(x = log(cases_total),
                 y = cor_nolag))


gtrends_sub_df$per_pop_using_internet %>% table()
