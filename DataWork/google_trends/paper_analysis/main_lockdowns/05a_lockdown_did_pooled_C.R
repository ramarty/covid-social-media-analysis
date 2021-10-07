# DiD: Pooled Results

keywords_to_use <- c("debt",
                     "file for unemployment",
                     "unemployment",
                     "unemployment benefits",
                     "unemployment insurance",
                     "unemployment office",
                     
                     "anxiety",
                     "anxiety attack",
                     #"anxiety Symptoms",
                     "boredom",
                     #"hysteria",
                     "insomnia",
                     #"loneliness",
                     "lonely",
                     "panic",
                     "social isolation",
                     "suicide",
                     
                     "divorce",
                     "wedding",
                     "emergency pill",
                     "pregnancy test",
                     
                     "social distance",
                     "stay at home")

prep_keywords <- function(df){
  
  df <- df %>%
    dplyr::mutate(keyword_type = case_when(
      keyword_en %in% c("anxiety",
                        "anxiety attack",
                        "anxiety symptoms",
                        "boredom",
                        "hysteria",
                        "insomnia",
                        "loneliness",
                        "lonely",
                        "panic",
                        "social isolation",
                        "overwhelmed",
                        "suicide") ~ "Mental Health",
      
      keyword_en %in% c("debt",
                        "file for unemployment",
                        "unemployment",
                        "unemployment benefits",
                        "unemployment insurance",
                        "unemployment office") ~ "Economic",
      
      keyword_en %in% c("abortion",
                        "break up",
                        "condom",
                        "dating app",
                        "divorce",
                        "emergency pill",
                        "plan child",
                        "plan other children",
                        "pregnancy test",
                        "relationship",
                        "tinder",
                        "wedding") ~ "Relationships\n&Family Planning",
      
      keyword_en %in% c("social distance",
                        "stay at home") ~ "Social Distancing"
    )) %>%
    dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase()) 
  
  keyword_factor_order <- df %>%
    distinct(keyword_en, keyword_type) %>%
    arrange(keyword_type, keyword_en) %>%
    pull(keyword_en)
  
  df <- df %>%
    dplyr::mutate(keyword_en = factor(keyword_en, levels = keyword_factor_order))
  
  return(df)
}

# Load/Prep Data [For Trends] --------------------------------------------------
df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                        "did_pooled_data.Rds")) %>%
  
  # Filter keywords
  dplyr::filter(keyword_en %in% keywords_to_use) %>%
  
  ## Standardized hits value
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_ma7_min = min(hits_ma7, na.rm=T),
                hits_ma7_max = max(hits_ma7, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(hits_ma7_std = ((hits_ma7 - hits_ma7_min) / (hits_ma7_max - hits_ma7_min))*100) %>%
  dplyr::group_by(keyword_en, pandemic_time, days_since_c_policy_yearcurrent) %>%
  dplyr::summarise(hits_ma7_std = mean(hits_ma7_std, na.rm = T)) %>%
  ungroup() %>%
  
  # Prep variables
  prep_keywords() %>%
  dplyr::mutate(pandemic_time = case_when(
    pandemic_time == 1 ~ "Pandemic",
    pandemic_time == 0 ~ "Pre-Pandemic"
  ))

# Load/Prep Regression Results -------------------------------------------------
coef_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                             "did_pooled_results.Rds")) %>%
  
  ## Prep keywords
  mutate(keyword_en = keyword) %>%
  dplyr::filter(keyword_en %in% keywords_to_use) %>%
  prep_keywords() %>%
  
  ## Prep Coefficients
  dplyr::filter(variable %>% 
                  str_detect("days_since_c_policy_yearcurrent_post_X_year2020|did_")) %>%
  dplyr::filter(type %in% c(
    "did_gm_avg_min_AND_did_EconomicSupportIndex_max",
    "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max",
    "Overall")) %>%
  mutate(variable = case_when(
    variable == "days_since_c_policy_yearcurrent_post_X_year2020" ~ "Post Policy X Pandemic",
    variable == "did_EconomicSupportIndex_max" ~ "Post Policy X Pandemic X Econ Support",
    variable == "did_gm_avg_min" ~               "Post Policy X Pandemic X Mobility Reduction",
    variable == "did_StringencyIndex_max" ~      "Post Policy X Pandemic X Stringency Index",
    variable == "did_gm_avg_min_X_did_EconomicSupportIndex_max" ~               
      "Post Policy X Pandemic X Mobility Reduction X Econ Support",
    variable == "did_StringencyIndex_max_X_did_EconomicSupportIndex_max" ~      
      "Post Policy X Pandemic X Stringency Index X Econ Support"
  )) %>%
  mutate(type = case_when(
    type == "did_gm_avg_min_AND_did_EconomicSupportIndex_max_INTER" ~ "mobility_reduction",
    type == "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max_INTER" ~ "stringency_index",
    type == "did_gm_avg_min_AND_did_EconomicSupportIndex_max" ~ "mobility_reduction",
    type == "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max" ~ "stringency_index",
    TRUE ~ type,
  )) %>%
  dplyr::mutate(variable = variable %>% fct_rev) 

# Trends Figure ----------------------------------------------------------------
p_trends <- df %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_line(aes(x = days_since_c_policy_yearcurrent,
                y = hits_ma7_std,
                color = factor(pandemic_time))) +
  labs(color = "Time Period",
       x = "Days Since Lockdown",
       y = "Average Search Interest",
       title = "A. Trends in Search Interest") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 9),
        legend.position = "top") +
  scale_color_manual(values = c("darkorange", "gray40")) +
  facet_wrap(~keyword_en,
             ncol = 4,
             scales = "free_y") 
#         axis.title.y = element_text(angle = 0, vjust = 0.5)

# Overall Impact Figure --------------------------------------------------------
p_overall <- coef_df %>% 
  dplyr::filter(type == "Overall") %>%
  dplyr::mutate(keyword_en = keyword_en %>% fct_rev()) %>%
  ggplot(aes(xmin = p025,
             xmax = p975,
             x = b,
             y = keyword_en,
             color = keyword_type)) +
  geom_point() +
  geom_linerange() +
  geom_vline(xintercept = 0, 
             color = "red") +
  theme_minimal() +
  labs(color = "Category",
       x = "Coefficient (+/- 95% CI)",
       y = "Search Term",
       title = "B. Diff-in-Diff Results: Impact of Contaiment Policies on\nSearch Interest") +
  theme(legend.position = "none")

# Interaction Figures ----------------------------------------------------------
type_i = "stringency_index"

p_interact <- list()
for(type_i in c("stringency_index", 
                "mobility_reduction")){
  
  p_interact[[type_i]] <- coef_df %>% 
    dplyr::filter(type != "Overall") %>%
    dplyr::filter(type == type_i) %>%
    ggplot(aes(xmin = p025,
               xmax = p975,
               x = b,
               y = variable,
               color = keyword_type)) +
    geom_point() +
    geom_linerange() +
    geom_vline(xintercept = 0, 
               color = "red") +
    labs(x = "Coefficient (+/- 95% CI)",
         y = "Variable",
         color = "Category") +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold",
                                    size = 7.6),
          axis.text.y = element_text(color = "black")) +
    theme(legend.position = "bottom") +
    #scale_color_manual(values = c("black", "firebrick2")) +
    facet_wrap(~keyword_en,
               scales = "free_x")
  
}

# Append Figures ---------------------------------------------------------------
title_theme <- theme(plot.title = element_text(face = "bold", 
                                               color = "black"),
                     plot.title.position = "plot")

p_top <- ggarrange(p_trends + title_theme, 
                   p_overall + title_theme,
                   nrow = 1,
                   widths = c(0.55, 0.45))
p <- ggarrange(p_top, 
               p_interact$mobility_reduction +
                 labs(title = "C. Diff-in-Diff Results: Heterogeneity of Impacts of Contaiment Policies on Search Interest\nby Levels of Contaiment Restriveness and Economic Support") +
                 title_theme,
               nrow = 2)

ggsave(p, filename = file.path(paper_figures, paste0("did_pooled.png")),
       height = 15, width = 13)

# Figure: Contaiment Restrictiveness - Strigency Index -------------------------
ggsave(p_interact$stringency_index, filename = file.path(paper_figures, paste0("did_pooled_","strigency",".png")),
       height = 5, width = 13)
