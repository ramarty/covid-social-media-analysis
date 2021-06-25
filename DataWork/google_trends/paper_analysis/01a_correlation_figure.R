# What variables explain correlation?

# TODO:
# Check: cor -- 0s in there??
# Include in figure
# N
# 50th Percentile
# 75th Percentile

keywords_en_use <- c("loss of smell", 
                     "loss of taste",
                     "pneumonia",
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

cor_12_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "correlation_datasets",
                               "correlations_gtrends_since2020-12-01.Rds")) %>%
  dplyr::mutate(date_since = "2020-12-01")

# Prep Data --------------------------------------------------------------------
cor_df <- bind_rows(cor_01_df,
                    cor_12_df) %>%
  dplyr::filter(type %in% "Cases") %>%
  dplyr::filter(keyword_en %in% keywords_en_use) %>%
  
  # For ranking correlations show terms with highest median correlation
  group_by(keyword_en) %>%
  dplyr::mutate(cor_sort_all = quantile(cor_nolag, 0.95, na.rm=T)) %>%
  ungroup() %>%
  
  # Cleanup variables
  mutate(keyword_en = keyword_en %>% 
           tools::toTitleCase() %>% 
           str_replace_all("\\bi\\b", "I"))

cor_df$keyword_en <- factor(cor_df$keyword_en, 
                            levels=unique(cor_df$keyword_en[order(cor_df$cor_sort_all)]), 
                            ordered=TRUE)

cor_df$date_since <- cor_df$date_since %>% as.factor() %>% fct_rev

# Summary Stats Data -----------------------------------------------------------
cor_sum_df <- cor_df %>%
  group_by(date_since, keyword_en) %>%
  dplyr::summarise(cor_p0_05 = quantile(cor, 0.05),
                   cor_p0_25 = quantile(cor, 0.25),
                   cor_p0_50 = quantile(cor, 0.50),
                   cor_p0_75 = quantile(cor, 0.75),
                   cor_p0_95 = quantile(cor, 0.95),
                   N = n()) 

# pivot_wider(values_from = c(cor_p0_05,
#                             cor_p0_25,
#                             cor_p0_50,
#                             cor_p0_75,
#                             cor_p0_95,
#                             N),
#             names_from = date_since) 

# Figures ----------------------------------------------------------------------
ALPHA_VLINE = 0.3

x_pos_N <- 1.2
x_pos_p_50 <- 1.6
x_pos_p_95 <- 2
text_title_y_height_low = 8.4
text_title_y_height_high = 8.6

cor_sum_df$x_pos_N <- x_pos_N
cor_sum_df$x_pos_p_50 <- x_pos_p_50
cor_sum_df$x_pos_p_95 <- x_pos_p_95
cor_df %>%
  ggplot(aes(y = keyword_en,
             x = cor_nolag,
             fill = date_since)) +
  geom_vline(xintercept = -1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
  geom_vline(xintercept = -0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
  geom_vline(xintercept = 0, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
  geom_vline(xintercept = 0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
  geom_vline(xintercept = 1, size = 0.1, color = "black", alpha =ALPHA_VLINE) +
  geom_boxplot(outlier.size=0, alpha = 0.3) +
  geom_point(position = position_jitterdodge(jitter.width=0.3,
                                             dodge.width = 0.85),
             pch = 21,
             size = 0.7,
             color = "black") +
  geom_text(aes(x=x_pos_N,
                y=9,
                label = "N"),
            color = "white") +
  geom_text(aes(x=x_pos_N,
                y=text_title_y_height_low,
                label = "N")) +
  geom_text(aes(x=x_pos_p_50,
                y=text_title_y_height_low,
                label = "Median")) +
  geom_text(aes(x=x_pos_p_95,
                y=text_title_y_height_high,
                label = "95th")) +
  geom_text(aes(x=x_pos_p_95,
                y=text_title_y_height_low,
                label = "Percentile")) +
  geom_text(data = cor_sum_df,
            aes(y = keyword_en,
                x = x_pos_N,
                label = N),
            position = position_dodge(width = 0.85)) +
  geom_text(data = cor_sum_df,
            aes(y = keyword_en,
                x = x_pos_p_50,
                label = round(cor_p0_50, 2)),
            position = position_dodge(width = 0.85)) +
  geom_text(data = cor_sum_df,
            aes(y = keyword_en,
                x = x_pos_p_95,
                label = round(cor_p0_95, 2)),
            position = position_dodge(width = 0.85)) +
  labs(x = "Correlation between search interest and COVID-19 cases",
       y = NULL,
       fill = "Using data since:") +
  scale_fill_manual(values = c("dodgerblue1", "orange"),
                    guide = guide_legend(reverse = TRUE)) +
  xlim(-0.6, 2.5) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "black")) +
  ggsave(filename = file.path(paper_figures, "cor_fig.png"),
         height = 8, width = 10)

