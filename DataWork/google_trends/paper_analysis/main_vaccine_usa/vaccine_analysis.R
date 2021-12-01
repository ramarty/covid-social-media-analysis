# Vaccine Analysis

# TODO: How to deal with some not having much data...
# Show N by continent? 
# Heterogenity by continent!

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", 
                                "gtrends_otherdata_varclean_complete_vaccine.Rds"))

## Remove Keywords
gtrends_df <- gtrends_df %>%
  dplyr::filter(!(keyword_en %in% c("vaccine approval",
                                    "vaccine conspiracy",
                                    "pharmacy",
                                    "vaccine dna",
                                    "vaccine failure",
                                    "vaccine fraud",
                                    "vaccine injuries",
                                    "vaccine mercury",
                                    "vaccine toxins",
                                    "vaccines are poison",
                                    "vaccines kill",
                                    "can i get the vaccine",
                                    "blood clots",
                                    "medical freedom",
                                    "where covid vaccine",
                                    "covid vaccine austism",
                                    "covid vaccine cause autism",
                                    "negative side effects of covid vaccine",
                                    "covid vaccine is poison",
                                    "covid vaccine second dose sick",
                                    "do you get sick after covid vaccine",
                                    "is covid vaccine the mark of the beast",
                                    "can covid vaccine cause infertility",
                                    "where can i get the covid vaccine",
                                    "get covid vaccine near me",
                                    "vaccine aluminum",
                                    "where get covid vaccine near me",
                                    "where to get vaccine covid near me",
                                    "does covid vaccine cause infertility", 
                                    "covid vaccine ineffective",
                                    "covid vaccine unsafe",
                                    "covid vaccine appointment near me",
                                    "sick from covid vaccine",
                                    "does covid vaccine cause infertility")))

# PREP DATA ====================================================================
# Prep data that will be relevant for all analysis

## If standard deviation of hits is zero, remove
gtrends_df <- gtrends_df %>%
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_sd = sd(hits)) %>%
  ungroup() %>%
  dplyr::filter(hits_sd > 0)

## Categorize
#keyword_n_df <- gtrends_df %>%
#  distinct(keyword_en, geo) %>%
#  group_by(keyword_en) %>%
#  dplyr::summarise(N = n()) %>%
#  arrange(N)
#keyword_n_df

# gtrends_df$keyword_en %>% unique() %>% sort()

gtrends_df <- gtrends_df %>%
  dplyr::mutate(keyword_cat = case_when(
    # Searches specifically related to appointments
    keyword_en %in% c("can i get the covid vaccine",
                      "covid vaccine appointment",
                      "covid vaccine center") ~ "Vaccine Appointment",
    
    keyword_en %in% c("covid vaccine",
                      "covid vaccine priority",
                      "covid vaccine priority list",
                      "covid vaccine approved",
                      "is covid vaccine approved", ## COULD DELETE
                      "covid vaccine second dose",
                      "vaccine near me", ## COULD DELETE
                      "vaccine appointment",
                      "where to get covid vaccine") ~ "Vaccine General",
    
    keyword_en %in% c("covid vaccine blood clots",
                      "covid vaccine safety",
                      "covid vaccine sick", ## COULD DELETE
                      "covid vaccine side effects",
                      "safety of covid vaccine",
                      "vaccine allergy",
                      "long term effects of covid vaccine",
                      "vaccine reaction") ~ "Side Effects & Safety",
    
    keyword_en %in% c("covid microchip",
                      "covid vaccine microchip",
                      "covid vaccine cause infertility",
                      "covid vaccine infertility",
                      "covid vaccine change dna",
                      "does covid vaccine change dna",
                      "covid vaccine dangerous",
                      "is the covid vaccine the mark of the beast",
                      "covid vaccine mercury",
                      "ivermectin") ~ "Misinformation"
  ))

# A. CORRELATION WITH VACCINATION RATES ========================================
# TODO: Could do within 30 days, for example
gtrends_vaccor_df <- gtrends_df %>%
  dplyr::filter(!is.na(total_vaccinations_per_hundred),
                !is.na(hits_ma7),
                total_vaccinations_per_hundred > 0) %>%
  dplyr::filter(date_first_vaccine_given <= ymd("2021-06-01")) %>%
  group_by(keyword_en, geo, keyword_cat) %>%
  dplyr::summarise(cor = cor(hits_ma7, total_vaccinations_per_hundred)) %>%
  dplyr::filter(!is.na(cor)) %>%
  dplyr::mutate(keyword_en = keyword_en %>% 
                  tools::toTitleCase())

gtrends_vaccor_df %>%
  ggplot(aes(x = cor, 
             y = reorder(keyword_en, cor, FUN = median),
             fill = keyword_cat)) +
  geom_vline(aes(xintercept = 0)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width=0.3,
                                             dodge.width = 0.85),
             pch = 21,
             size = 0.9, # 0.7
             stroke = 0.2, # 0.1
             color = "black") 

# Limit data and scale between 0-100 -------------------------------------------
#### Prep data for event study
gtrends_es_df <- gtrends_df %>%
  dplyr::filter(!is.na(days_since_first_vaccine_given)) %>%
  dplyr::filter(days_since_first_vaccine_given >= -90,
                days_since_first_vaccine_given <= 90) %>%
  dplyr::select(geo, keyword_en, keyword_cat, days_since_first_vaccine_given, hits_ma7) %>%
  dplyr::mutate(days_since_first_vaccine_given = days_since_first_vaccine_given %>% as.numeric) %>%
  dplyr::mutate(post_vaccine = as.numeric(days_since_first_vaccine_given > 0)) 
#%>%
#tidyr::complete(geo = unique(gtrends_df$geo), 
#                keyword_en = unique(gtrends_df$keyword_en), 
#                days_since_first_vaccine_given = -90:90, 
#                fill = list(hits_ma7 = 0)) 

#### Scaled - Average
gtrends_es_scaled_avg_df <- gtrends_es_df %>%
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_ma7_min = min(hits_ma7, na.rm=T),
                hits_ma7_max = max(hits_ma7, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(hits_ma7_std = ((hits_ma7 - hits_ma7_min) / (hits_ma7_max - hits_ma7_min))*100) %>%
  dplyr::group_by(keyword_en, keyword_cat, days_since_first_vaccine_given) %>%
  dplyr::summarise(hits_ma7_std = mean(hits_ma7_std, na.rm = T)) %>%
  #dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase() %>% paste0("\nN Countries = ", N_countries_keyword))
  dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase())

p <- gtrends_es_scaled_avg_df %>%
  ggplot() +
  geom_line(aes(x = days_since_first_vaccine_given,
                y = hits_ma7_std,
                color = keyword_cat)) +
  geom_vline(aes(xintercept = 0)) +
  facet_wrap(~keyword_en, scales = "free_y")

ggsave(p, filename = file.path("~/Desktop/since_vac.png"),
       height = 12, width = 18)

#### Diff-in-Diff
gtrends_es_df <- gtrends_es_df %>%
  dplyr::mutate(hits_ma7_log = log(hits_ma7 + 1))

coefs_df <- map_df(unique(gtrends_es_df$keyword_en), function(keyword_en_i){
  print(keyword_en_i)
  
  felm(hits_ma7_log ~ post_vaccine | geo, data = gtrends_es_df %>%
         dplyr::filter(keyword_en %in% keyword_en_i)) %>%
    lm_post_confint_tidy() %>%
    dplyr::mutate(keyword_en = keyword_en_i)
  
})

coefs_df <- coefs_df %>%
  mutate(keyword_en = fct_reorder(keyword_en, b))

coefs_df %>%
  ggplot(aes(x = b,
             xmin = p025,
             xmax = p975,
             y = keyword_en)) +
  geom_point() +
  geom_linerange()








