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

# Load/Prep Data: Trends -------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean_complete.Rds"))

gtrends_sum_df <- gtrends_df %>%
  dplyr::filter(!is.na(days_since_c_policy_yearcurrent),
                keywords_en %in% keywords_to_use)

gtrends_sum_df <- gtrends_sum_df %>%
  dplyr::filter(pandemic_time %in% c(0, 1),
                keyword_en %in% KEYWORDS_CONTAIN_USE,
                days_since_c_policy_yearcurrent >= -90,
                days_since_c_policy_yearcurrent <= 90) %>%
  dplyr::select(geo, keyword_en, pandemic_time, days_since_c_policy_yearcurrent, hits_ma7) %>%
  dplyr::mutate(days_since_c_policy_yearcurrent = days_since_c_policy_yearcurrent %>% as.numeric) %>%
  tidyr::complete(geo = unique(gtrends_sum_df$geo), 
                  keyword_en = KEYWORDS_CONTAIN_USE, 
                  days_since_c_policy_yearcurrent = -90:90, 
                  pandemic_time = c(0, 1),
                  fill = list(hits_ma7 = 0)) 

## Delete if no hits or policy data
gtrends_sum_df <- gtrends_sum_df %>%
  group_by(geo, keyword_en) %>%
  dplyr::mutate(hits_ma7_geoSUM = sum(hits_ma7)) %>%
  ungroup() %>%
  dplyr::filter(hits_ma7_geoSUM > 0)

gtrends_sum_df <- gtrends_sum_df %>%
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_ma7_min = min(hits_ma7, na.rm=T),
                hits_ma7_max = max(hits_ma7, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(hits_ma7_std = ((hits_ma7 - hits_ma7_min) / (hits_ma7_max - hits_ma7_min))*100) %>%
  dplyr::group_by(keyword_en, pandemic_time, days_since_c_policy_yearcurrent) %>%
  dplyr::summarise(hits_ma7_std = mean(hits_ma7_std, na.rm = T)) %>%
  #dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase() %>% paste0("\nN Countries = ", N_countries_keyword))
  dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase()) %>%
  dplyr::mutate(keyword_en = keyword_en %>%
                  factor(levels = c("Debt",
                                    "File for Unemployment",
                                    "Unemployment",
                                    "Unemployment Benefits",
                                    "Unemployment Insurance",
                                    "Unemployment Office",

                                    "Anxiety",
                                    "Anxiety Attack",
                                    #"Anxiety Symptoms",
                                    "Boredom",
                                    #"Hysteria",
                                    "Insomnia",
                                    #"Loneliness",
                                    "Lonely",
                                    "Panic",
                                    "Social Isolation",
                                    "Suicide",

                                    "Divorce",
                                    "Wedding",
                                    "Emergency Pill",
                                    "Pregnancy Test",

                                    "Social Distance",
                                    "Stay at Home"))) %>%
  dplyr::mutate(pandemic_time = case_when(
    pandemic_time == 1 ~ "Pandemic",
    pandemic_time == 0 ~ "Pre-Pandemic"
  ))

# Load/Prep Data: DiD Data -----------------------------------------------------
df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                        "did_pooled_results.Rds"))

df <- df %>%
  dplyr::filter(variable %>% 
                  str_detect("days_since_c_policy_yearcurrent_post_X_year2020|did_"),
                keyword %in% keywords_to_use) %>%
  dplyr::filter(type %in% c(#"did_gm_avg_min_AND_did_EconomicSupportIndex_max_INTER",
    #"did_StringencyIndex_max_AND_did_EconomicSupportIndex_max_INTER",
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
  mutate(sig = ifelse(pvalue <= 0.05, "Sig", "Not Sig")) %>%
  dplyr::mutate(keyword_type = case_when(
    keyword %in% c("anxiety",
                   "anxiety attack",
                   "anxiety symptoms",
                   "boredom",
                   "hysteria",
                   "insomnia",
                   "loneliness",
                   "lonely",
                   "panic",
                   "social isolation",
                   "suicide") ~ "Mental Health",
    
    keyword %in% c("debt",
                   "file for unemployment",
                   "unemployment",
                   "unemployment benefits",
                   "unemployment insurance",
                   "unemployment office") ~ "Economic",
    
    keyword %in% c("divorce",
                   "wedding",
                   "emergency pill",
                   "pregnancy test") ~ "Relationships\n&Family Planning",
    
    keyword %in% c("social distance",
                   "stay at home") ~ "Social Distancing"
  )) %>%
  mutate(keyword = keyword %>%
           as.character() %>%
           tools::toTitleCase()) %>%
  dplyr::mutate(keyword = keyword %>%
                  factor(levels = c("Debt",
                                    "File for Unemployment",
                                    "Unemployment",
                                    "Unemployment Benefits",
                                    "Unemployment Insurance",
                                    "Unemployment Office",
                                    
                                    "Anxiety",
                                    "Anxiety Attack",
                                    #"Anxiety Symptoms",
                                    "Boredom",
                                    #"Hysteria",
                                    "Insomnia",
                                    #"Loneliness",
                                    "Lonely",
                                    "Panic",
                                    "Social Isolation",
                                    "Suicide",
                                    
                                    "Divorce",
                                    "Wedding",
                                    "Emergency Pill",
                                    "Pregnancy Test",
                                    
                                    "Social Distance",
                                    "Stay at Home"))) %>%
  dplyr::mutate(variable = variable %>% fct_rev) 

# Trends Figure ----------------------------------------------------------------
p_trends <- gtrends_sum_df[!(gtrends_sum_df$keyword_en %in% "Plan Other Children"),] %>% 
  dplyr::mutate(pandemic_time_str = ifelse(pandemic_time == 1,
                                           "Pandemic",
                                           "Pre-Pandemic")) %>%
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
p_overall <- df %>% 
  dplyr::filter(type == "Overall") %>%
  mutate(keyword = keyword %>% fct_rev()) %>%
  ggplot(aes(xmin = p025,
             xmax = p975,
             x = b,
             y = keyword,
             color = keyword_type)) +
  geom_point() +
  geom_linerange() +
  geom_vline(xintercept = 0, 
             color = "red") +
  theme_minimal() +
  labs(color = "Category",
       x = "Coefficient (+/- 95% CI)",
       y = "Search Term",
       title = "B. Diff-in-Diff Results: Impact of Contaiment Policies on Search Interest") +
  theme(legend.position = "none")

# Interaction Figures ----------------------------------------------------------
type_i = "stringency_index"

p_interact <- list()
for(type_i in c("stringency_index", 
                "mobility_reduction")){
  
  p_interact[[type_i]] <- df %>% 
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
    facet_wrap(~keyword,
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
