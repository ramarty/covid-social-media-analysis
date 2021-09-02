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

# Prep Data --------------------------------------------------------------------
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
           str_replace_all("\\bi\\b", "I"))

cor_df$keyword_en <- factor(cor_df$keyword_en, 
                            levels=unique(cor_df$keyword_en[order(cor_df$cor_sort_all)]), 
                            ordered=TRUE)

cor_df$date_since <- cor_df$date_since %>% as.factor() %>% fct_rev

# Boxplot ======================================================================
cor_avg_df <- cor_df %>%
  #dplyr::mutate(cor_avg = (cor + cor_nolag)/2) %>%
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

# Table ========================================================================

# Prep Data --------------------------------------------------------------------
cor_sum_df <- bind_rows(
  cor_df %>%
    group_by(date_since, keyword_en) %>%
    dplyr::summarise(min = min(cor, na.rm=T),
                     cor_p0_05 = quantile(cor, 0.05, na.rm=T),
                     cor_p0_25 = quantile(cor, 0.25, na.rm=T),
                     cor_p0_50 = quantile(cor, 0.50, na.rm=T),
                     cor_p0_75 = quantile(cor, 0.75, na.rm=T),
                     cor_p0_95 = quantile(cor, 0.95, na.rm=T),
                     max = max(cor, na.rm=T),
                     N = n()) %>%
    dplyr::mutate(type = "cor"),
  
  cor_df %>%
    group_by(date_since, keyword_en) %>%
    dplyr::summarise(min = min(cor_nolag, na.rm=T),
                     cor_p0_05 = quantile(cor_nolag, 0.05, na.rm=T),
                     cor_p0_25 = quantile(cor_nolag, 0.25, na.rm=T),
                     cor_p0_50 = quantile(cor_nolag, 0.50, na.rm=T),
                     cor_p0_75 = quantile(cor_nolag, 0.75, na.rm=T),
                     cor_p0_95 = quantile(cor_nolag, 0.95, na.rm=T),
                     max = max(cor_nolag, na.rm=T),
                     N = n()) %>%
    dplyr::mutate(type = "cor_nolag"),
  
  cor_df %>%
    #dplyr::filter(zscore >= 3) %>%
    group_by(date_since, keyword_en) %>%
    dplyr::summarise(min = min(lag, na.rm=T),
                     cor_p0_05 = quantile(lag, 0.05, na.rm=T),
                     cor_p0_25 = quantile(lag, 0.25, na.rm=T),
                     cor_p0_50 = quantile(lag, 0.50, na.rm=T),
                     cor_p0_75 = quantile(lag, 0.75, na.rm=T),
                     cor_p0_95 = quantile(lag, 0.95, na.rm=T),
                     max = max(lag, na.rm=T),
                     N = n()) %>%
    dplyr::mutate(type = "lag") 
)

cor_sum_wide_df <- cor_sum_df %>%
  pivot_wider(values_from = c(min,
                              cor_p0_05,
                              cor_p0_25,
                              cor_p0_50,
                              cor_p0_75,
                              cor_p0_95,
                              max,
                              N),
              names_from = date_since) %>%
  arrange(desc(keyword_en)) %>%
  mutate_if(is.numeric, round, 2)

cor_sum_wide_df <- cor_sum_wide_df %>%
  dplyr::mutate(tex = paste(keyword_en,
                            `min_2020-01-01`,
                            `cor_p0_05_2020-01-01`,
                            `cor_p0_25_2020-01-01`,
                            `cor_p0_50_2020-01-01`,
                            `cor_p0_75_2020-01-01`,
                            `cor_p0_95_2020-01-01`,
                            `max_2020-01-01`,
                            `N_2020-01-01`,
                            
                            `min_2021-01-01`,
                            `cor_p0_05_2021-01-01`,
                            `cor_p0_25_2021-01-01`,
                            `cor_p0_50_2021-01-01`,
                            `cor_p0_75_2021-01-01`,
                            `cor_p0_95_2021-01-01`,
                            `max_2021-01-01`,
                            `N_2021-01-01`,
                            sep = " & ")) %>%
  mutate(tex = paste(tex, " \\\\ \n"))

# Table ------------------------------------------------------------------------
sink(file.path(paper_tables, "cor_lag_table.tex"))

cat("\\begin{tabular}{l llllllll | llllllll} \n")
cat("\\hline \n")
cat(" & \\multicolumn{8}{c|}{{\\bf Using data since 2020-01-01}} & \\multicolumn{8}{c}{{\\bf Using data since 2021-01-01}} \\\\ \n")
cat("Term & & \\multicolumn{5}{c}{Percentile} &  &  &  & \\multicolumn{5}{c}{Percentile} & Max & \\\\ \n")
cat(" & Min & 5th & 25th & 50th & 75th & 95th & Max & N & Min & 5th & 25th & 50th & 75th & 95th & Max & N \\\\ \n")
cat("\\hline \n")

cat("\\multicolumn{9}{l|}{{\\bf Correlation}} & & & & & & & & \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor_nolag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l|}{{\\bf Correlation using best lag}} & & & & & & & & \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l|}{{\\bf Lag with best correlation}} & & & & & & & & \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "lag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex[i])

cat("\\hline \n")
cat("\\end{tabular} ")

sink()

if(F){
  # Figure: Cor ==================================================================
  
  ALPHA_VLINE = 0.3
  
  cor_df %>%
    ggplot(aes(y = keyword_en,
               x = cor_nolag,
               fill = date_since)) +
    geom_vline(xintercept = -1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_vline(xintercept = -0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_vline(xintercept = 0, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_vline(xintercept = 0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_vline(xintercept = 1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    #geom_boxplot(outlier.size=0, alpha = 0.3) +
    geom_point(position = position_jitterdodge(jitter.width=0.3,
                                               dodge.width = 0.85),
               pch = 21,
               size = 0.7,
               stroke = 0.1,
               color = "black") +
    labs(x = "Correlation between search interest and COVID-19 cases",
         y = NULL,
         fill = "Using data since:") +
    scale_fill_manual(values = c("dodgerblue1", "orange"),
                      guide = guide_legend(reverse = TRUE)) +
    #xlim(-0.6, 1) +
    theme_minimal() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold", color = "black")) 
  ggsave(filename = file.path(paper_figures, "cor_fig.png"),
         height = 4, width = 5)
  
  # Figure: Lag ==================================================================
  
  ALPHA_VLINE = 0.3
  
  cor_df %>%
    dplyr::filter(zscore > 1) %>%
    ggplot(aes(x = lag)) +
    geom_histogram() +
    labs(x = "Correlation between search interest and COVID-19 cases",
         y = NULL,
         fill = "Using data since:") +
    scale_fill_manual(values = c("dodgerblue1", "orange"),
                      guide = guide_legend(reverse = TRUE)) +
    #xlim(-0.6, 1) +
    theme_minimal() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold", color = "black")) +
    facet_grid(date_since~keyword_en)
  ggsave(filename = file.path(paper_figures, "cor_fig.png"),
         height = 4, width = 5)
  
  
  
  
  # Figure with table combined ---------------------------------------------------
  if(F){
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
      ggsave(filename = file.path(paper_figures, "cor_fig_table.png"),
             height = 8, width = 10)
  }
  
}