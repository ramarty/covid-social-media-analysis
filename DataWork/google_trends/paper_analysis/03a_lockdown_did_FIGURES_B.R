# Lockdown Difference-in-Difference Analysis

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                                "did_results.Rds"))

results_df <- results_df %>%
  dplyr::filter(variable == "days_since_lockdown_min_yearcurrent_post_X_year2020") %>%
  dplyr::mutate(region = region %>% 
                  factor(levels = c("Global",
                                    "East Asia & Pacific",
                                    "Europe & Central Asia",
                                    "Latin America & Caribbean",
                                    "Middle East & North Africa",
                                    "North America",
                                    "South Asia",
                                    "Sub-Saharan Africa") %>% rev())) %>%
  dplyr::mutate(category = case_when(
    keyword %in% c("boredom", "anxiety") ~ "mental health"
  ))

#results_df$keyword %>% unique()

# Can facet over type
results_df %>%
  dplyr::filter(category %in% "mental health") %>% 
  ggplot(aes(xmin = p025,
             xmax = p975,
             x = b,
             y = region)) +
  geom_vline(xintercept = 0) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_linerange(position = position_dodge(width = 0.9)) +
  labs(y = NULL,
       x = "Coefficient (+/-95% CI)") +
  facet_wrap(~keyword,
             scales = "free_x")

