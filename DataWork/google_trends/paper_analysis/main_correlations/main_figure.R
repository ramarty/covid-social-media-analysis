# Correlations: Main Figure

keywords_en_use <- c("loss of smell", 
                     "loss of taste",
                     "pneumonia",
                     "fever",
                     "ageusia",
                     "anosmia",
                     "i can't smell",
                     "how to treat coronavirus")

# BOXPLOT FIGURE ===============================================================

# ** Load Data -------------
cor_1_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries",
                              "correlation_datasets",
                              "correlations_gtrends_since2020-01-01_until2021-07-31.Rds")) %>%
  dplyr::mutate(date_since = "2020-01-01")

cor_2_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries",
                              "correlation_datasets",
                              "correlations_gtrends_since2021-01-01_until2021-07-31.Rds")) %>%
  dplyr::mutate(date_since = "2021-01-01")

cor_df <- bind_rows(cor_1_df,
                    cor_2_df) %>%
  dplyr::filter(type %in% "Cases") %>%
  dplyr::filter(keyword_en %in% keywords_en_use) %>%
  
  # For ranking correlations show terms with highest median correlation
  group_by(keyword_en) %>%
  dplyr::mutate(cor_sort_all = quantile(cor_nolag, 0.95, na.rm=T)) %>%
  ungroup() %>%
  
  # Cleanup variables
  mutate(keyword_en = keyword_en %>% 
           tools::toTitleCase() %>% 
           str_replace_all("\\bi\\b", "I"),
         date_since = date_since %>% as.factor() %>% fct_rev)

cor_df$keyword_en <- factor(cor_df$keyword_en, 
                            levels=unique(cor_df$keyword_en[order(cor_df$cor_sort_all)]), 
                            ordered=TRUE)

# Boxplot ======================================================================
cor_avg_df <- cor_df %>%
  dplyr::filter(date_since %in% c("2020-01-01")) %>%
  group_by(keyword_en) %>%
  dplyr::summarise(cor_avg = median(cor_nolag)) %>%
  arrange(desc(cor_avg)) %>%
  dplyr::mutate(keyword_en = keyword_en %>% as.character())

cor_df$keyword_en <- cor_df$keyword_en %>%
  as.character() %>%
  factor(levels = rev(cor_avg_df$keyword_en))

make_boxplot <- function(var, title, cor_df){
  
  ALPHA_VLINE <- 0.3
  
  cor_df$var <- cor_df[[var]]
  
  p <- cor_df %>%
    ggplot(aes(x = var,
               y = keyword_en,
               fill = date_since)) +
    geom_vline(xintercept = -1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_vline(xintercept = -0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_vline(xintercept = 0, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_vline(xintercept = 0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_vline(xintercept = 1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_boxplot(alpha = 0.7) +
    geom_point(position = position_jitterdodge(jitter.width=0.3,
                                               dodge.width = 0.85),
               pch = 21,
               size = 0.9, # 0.7
               stroke = 0.2, # 0.1
               color = "black") +
    geom_vline(xintercept = 0, color = "firebrick1") +
    scale_fill_manual(values = c("orange2", "dodgerblue3")) +
    labs(fill = "Using data since:",
         title = title,
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  if(var %in% c("cor", "cor_nolag")){
    p <- p +
      xlim(c(-1,1))
  }
  
  p
  
}

p1 <- make_boxplot("cor_nolag", "A. Correlation", cor_df)
p2 <- make_boxplot("cor", "B. Correlation using best lag", cor_df)
p3 <- make_boxplot("lag", "C. Lag with best correlation", cor_df)

p <- ggarrange(p1,p2,p3,
               common.legend = T,
               nrow = 3)

ggsave(p, filename = file.path(paper_figures, "cor_lag_fig.png"),
       height = 12, width = 8)





