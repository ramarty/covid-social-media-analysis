# Lockdown Difference-in-Difference Analysis

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

gtrends_sum_df <- gtrends_df %>%
  dplyr::filter(year %in% c(2019, 2020),
                keyword_en %in% KEYWORDS_CONTAIN_USE,
                days_since_c_policy_yearcurrent > -60,
                days_since_c_policy_yearcurrent < 60) %>%
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_ma7_min = min(hits_ma7, na.rm=T),
                hits_ma7_max = max(hits_ma7, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(hits_ma7_std = ((hits_ma7 - hits_ma7_min) / (hits_ma7_max - hits_ma7_min))*100) %>%
  dplyr::group_by(keyword_en, year, days_since_c_policy_yearcurrent) %>%
  dplyr::summarise(hits_ma7_std = mean(hits_ma7_std, na.rm = T)) %>%
  dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase())

p <- gtrends_sum_df %>% 
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_line(aes(x = days_since_c_policy_yearcurrent,
                y = hits_ma7_std,
                color = factor(year))) +
  labs(color = "Year",
       x = "Days Since Lockdown",
       y = "Average\nSearch\nInterest") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  scale_color_manual(values = c("gray40", "darkorange")) +
  facet_wrap(~keyword_en,
             ncol = 4) 

ggsave(p, filename = file.path(paper_figures, "global_lockdown_trends.png"),
       height = 9, width = 9)
# height = 5, width = 9
