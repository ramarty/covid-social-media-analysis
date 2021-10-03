# DiD: Pooled Results

min_narm <- function(x){
  out <- min(x, na.rm = T)
  out[out %in% c(Inf, -Inf)] <- NA
  return(out)
}

max_narm <- function(x){
  out <- max(x, na.rm = T)
  out[out %in% c(Inf, -Inf)] <- NA
  return(out)
}

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                        "did_pooled_data.Rds"))

names(df) <- names(df) %>%
  str_replace_all(" ", "_") %>%
  str_replace_all("/", "_")



# a <- df %>%
#   group_by(geo) %>%
#   summarise(EconomicSupportIndex = max_narm(EconomicSupportIndex))
# a$EconomicSupportIndex[a$EconomicSupportIndex > 0] %>% hist()
# table(a$EconomicSupportIndex == 0)
# table(a$EconomicSupportIndex > 0 & a$EconomicSupportIndex <= 50)
# table(a$EconomicSupportIndex > 50 & a$EconomicSupportIndex)

# df$`E1_Income support` %>% table()
# df$`E2_Debt/contract relief` %>% table()
# log(df$`E3_Fiscal measures`+1) %>% hist()
# scale(log(df$`E4_International support`+ 1)) %>% hist()
# df$EconomicSupportIndex %>% hist()
# df$StringencyIndex %>% hist()
# df$C %>% table()
# df$c2_workplace_closing
# df$c3_cancel_public_events
# df$c4_restrictions_on_gatherings
# df$c5_close_public_transport
# df$c6_stay_at_home_requirements
# df$c7_restrictions_on_internal_movement
# df$c8_international_travel_controls

# Prep Data --------------------------------------------------------------------
df <- df %>%
  dplyr::mutate(ln_cases_total = log(cases_total),
                income_high = income == "High income",
                income_low = income == "Low income",
                income_low_middle = income == "Lower middle income",
                income_upper_middle = income == "Upper middle income") %>%
  dplyr::filter(!is.na(ln_cases_total),
                ln_cases_total > 0) %>%
  dplyr::group_by(geo) %>%
  dplyr::mutate(gm_retail_min = 
                  min_narm(gmobility_retail_and_recreation_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                          (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_grocery_min = 
                  min_narm(gmobility_grocery_and_pharmacy_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                         (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_parks_min = 
                  min_narm(gmobility_parks_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                          (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_transit_min = 
                  min_narm(gmobility_transit_stations_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                     (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_workplace_min = 
                  min_narm(gmobility_workplaces_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                               (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_residential_max = 
                  max_narm(gmobility_residential_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  
  dplyr::mutate(StringencyIndex_max = 
                  max_narm(StringencyIndex[(pandemic_time == 1) & 
                                             (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  
  dplyr::mutate(GovernmentResponseIndex_max = 
                  max_narm(GovernmentResponseIndex[(pandemic_time == 1) & 
                                                     (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(EconomicSupportIndex_max = 
                  max_narm(EconomicSupportIndex[(pandemic_time == 1) & 
                                                  (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  
  dplyr::mutate(E1_Income_support_max = 
                  max_narm(E1_Income_support[(pandemic_time == 1) & 
                                               (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  
  dplyr::mutate(E1_Income_support_max = 
                  max_narm(E1_Income_support[(pandemic_time == 1) & 
                                               (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  
  dplyr::mutate(E1_Income_support_max = 
                  max_narm(E1_Income_support[(pandemic_time == 1) & 
                                               (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  
  dplyr::mutate(E2_Debt_contract_relief_max = 
                  max_narm(E2_Debt_contract_relief[(pandemic_time == 1) & 
                                                     (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  
  dplyr::mutate(E3_Fiscal_measures_max = 
                  max_narm(E3_Fiscal_measures[(pandemic_time == 1) & 
                                                (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  
  dplyr::mutate(E4_International_support_max = 
                  max_narm(E4_International_support[(pandemic_time == 1) & 
                                                      (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  ungroup() %>%
  mutate(EconomicSupportIndex_max_cat = case_when(
    EconomicSupportIndex_max == 0 ~ 0,
    EconomicSupportIndex_max > 0 & EconomicSupportIndex_max <= 50 ~ 1,
    EconomicSupportIndex_max > 50 ~ 2
  )) %>%
  dplyr::mutate(gm_retail_min = gm_retail_min * -1,
                gm_grocery_min = gm_grocery_min * -1,
                gm_parks_min = gm_parks_min * -1,
                gm_transit_min = gm_transit_min * -1,
                gm_workplace_min = gm_workplace_min * -1) %>%
  dplyr::mutate(gm_avg_min = (gm_retail_min +
                                gm_grocery_min +
                                gm_parks_min +
                                gm_transit_min +
                                gm_workplace_min) / 5) %>%
  dplyr::mutate_at(vars(gm_retail_min,
                        gm_grocery_min,
                        gm_parks_min,
                        gm_transit_min,
                        gm_workplace_min,
                        gm_avg_min,
                        StringencyIndex_max, 
                        GovernmentResponseIndex_max, 
                        EconomicSupportIndex_max), scale) %>%
  dplyr::mutate_at(vars(gm_retail_min,
                        gm_grocery_min,
                        gm_parks_min,
                        gm_transit_min,
                        gm_workplace_min,
                        gm_avg_min,
                        StringencyIndex_max, 
                        GovernmentResponseIndex_max, 
                        EconomicSupportIndex_max), as.numeric) %>%
  dplyr::mutate(did_ln_cases_total = days_since_c_policy_yearcurrent_post_X_year2020 * ln_cases_total,
                did_per_pop_using_internet = days_since_c_policy_yearcurrent_post_X_year2020 * per_pop_using_internet,
                did_mobile_cell_sub_per100 = days_since_c_policy_yearcurrent_post_X_year2020 * mobile_cell_sub_per100,
                did_gm_retail_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_retail_min,
                did_gm_grocery_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_grocery_min,
                did_gm_parks_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_parks_min,
                did_gm_transit_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_transit_min,
                did_gm_workplace_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_workplace_min,
                did_gm_avg_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_avg_min,
                did_gm_residential_max = days_since_c_policy_yearcurrent_post_X_year2020 * gm_residential_max,
                
                did_income_high = days_since_c_policy_yearcurrent_post_X_year2020 * income_high,
                did_income_low = days_since_c_policy_yearcurrent_post_X_year2020 * income_low,
                did_income_low_middle = days_since_c_policy_yearcurrent_post_X_year2020 * income_low_middle,
                did_income_upper_middle = days_since_c_policy_yearcurrent_post_X_year2020 * income_upper_middle,
                
                did_StringencyIndex_max = days_since_c_policy_yearcurrent_post_X_year2020 * StringencyIndex_max,
                did_GovernmentResponseIndex_max = days_since_c_policy_yearcurrent_post_X_year2020 * GovernmentResponseIndex_max,
                did_EconomicSupportIndex_max = days_since_c_policy_yearcurrent_post_X_year2020 * EconomicSupportIndex_max,
                did_EconomicSupportIndex_max_cat = days_since_c_policy_yearcurrent_post_X_year2020 * EconomicSupportIndex_max_cat,
                
                did_E1_Income_support_max = days_since_c_policy_yearcurrent_post_X_year2020 * E1_Income_support_max,
                did_E2_Debt_contract_relief_max = days_since_c_policy_yearcurrent_post_X_year2020 * E2_Debt_contract_relief_max,
                did_E3_Fiscal_measures_max = days_since_c_policy_yearcurrent_post_X_year2020 * E3_Fiscal_measures_max,
                did_E4_International_support_max = days_since_c_policy_yearcurrent_post_X_year2020 * E4_International_support_max) %>%
  
  dplyr::mutate(did_gm_avg_min_X_did_EconomicSupportIndex_max = did_gm_avg_min * did_EconomicSupportIndex_max,
                did_StringencyIndex_max_X_did_EconomicSupportIndex_max = did_StringencyIndex_max * did_EconomicSupportIndex_max,
                
                did_gm_avg_min_X_did_EconomicSupportIndex_max_cat = did_gm_avg_min * did_EconomicSupportIndex_max_cat,
                did_StringencyIndex_max_X_did_EconomicSupportIndex_max_cat = did_StringencyIndex_max * did_EconomicSupportIndex_max_cat)

# Analysis ---------------------------------------------------------------------
keyword_i <- "file for unemployment"

run_regs <- function(keyword_i, df){
  print(keyword_i)
  
  out1 <- felm(hits_ma7_log ~ pandemic_time + 
                 days_since_c_policy_yearcurrent_post + 
                 days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
               data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "Overall")
  
  out2 <- felm(hits_ma7_log ~ pandemic_time + 
                 days_since_c_policy_yearcurrent_post + 
                 days_since_c_policy_yearcurrent_post_X_year2020 +
                 did_EconomicSupportIndex_max | geo + week | 0 | 0, 
               data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_EconomicSupportIndex_max")
  
  out3 <- felm(hits_ma7_log ~ pandemic_time + 
                 days_since_c_policy_yearcurrent_post + 
                 days_since_c_policy_yearcurrent_post_X_year2020 +
                 did_EconomicSupportIndex_max_cat | geo + week | 0 | 0, 
               data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_EconomicSupportIndex_max_cat")
  
  out4 <- felm(hits_ma7_log ~ pandemic_time + 
                 days_since_c_policy_yearcurrent_post + 
                 days_since_c_policy_yearcurrent_post_X_year2020 +
                 did_E1_Income_support_max | geo + week | 0 | 0, 
               data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_E1_Income_support_max")
  
  out5 <- felm(hits_ma7_log ~ pandemic_time + 
                 days_since_c_policy_yearcurrent_post + 
                 days_since_c_policy_yearcurrent_post_X_year2020 +
                 did_E2_Debt_contract_relief_max | geo + week | 0 | 0, 
               data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_E2_Debt_contract_relief_max")
  
  out6 <- felm(hits_ma7_log ~ pandemic_time + 
                 days_since_c_policy_yearcurrent_post + 
                 days_since_c_policy_yearcurrent_post_X_year2020 +
                 did_E3_Fiscal_measures_max | geo + week | 0 | 0, 
               data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_E3_Fiscal_measures_max")
  
  out7 <- felm(hits_ma7_log ~ pandemic_time + 
                 days_since_c_policy_yearcurrent_post + 
                 days_since_c_policy_yearcurrent_post_X_year2020 +
                 did_E4_International_support_max | geo + week | 0 | 0, 
               data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_E4_International_support_max")
  
  out8 <- felm(hits_ma7_log ~ pandemic_time + 
                 days_since_c_policy_yearcurrent_post + 
                 days_since_c_policy_yearcurrent_post_X_year2020 +
                 did_StringencyIndex_max | geo + week | 0 | 0, 
               data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_StringencyIndex_max")
  
  out9 <- felm(hits_ma7_log ~ pandemic_time + 
                 days_since_c_policy_yearcurrent_post + 
                 days_since_c_policy_yearcurrent_post_X_year2020 +
                 did_gm_avg_min | geo + week | 0 | 0, 
               data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_gm_avg_min")
  
  out10 <- felm(hits_ma7_log ~ pandemic_time + 
                  days_since_c_policy_yearcurrent_post + 
                  days_since_c_policy_yearcurrent_post_X_year2020 +
                  did_gm_avg_min +
                  did_EconomicSupportIndex_max | geo + week | 0 | 0, 
                data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_gm_avg_min_AND_did_EconomicSupportIndex_max")
  
  out11 <- felm(hits_ma7_log ~ pandemic_time + 
                  days_since_c_policy_yearcurrent_post + 
                  days_since_c_policy_yearcurrent_post_X_year2020 +
                  did_StringencyIndex_max +
                  did_EconomicSupportIndex_max | geo + week | 0 | 0, 
                data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max")
  
  out12 <- felm(hits_ma7_log ~ pandemic_time + 
                  days_since_c_policy_yearcurrent_post + 
                  days_since_c_policy_yearcurrent_post_X_year2020 +
                  did_gm_avg_min +
                  did_EconomicSupportIndex_max +
                  did_gm_avg_min_X_did_EconomicSupportIndex_max | geo + week | 0 | 0, 
                data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_gm_avg_min_AND_did_EconomicSupportIndex_max_INTER")
  
  out13 <- felm(hits_ma7_log ~ pandemic_time + 
                  days_since_c_policy_yearcurrent_post + 
                  days_since_c_policy_yearcurrent_post_X_year2020 +
                  did_StringencyIndex_max +
                  did_EconomicSupportIndex_max +
                  did_StringencyIndex_max_X_did_EconomicSupportIndex_max | geo + week | 0 | 0, 
                data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max_INTER")
  
  ###########
  out14 <- felm(hits_ma7_log ~ pandemic_time + 
                  days_since_c_policy_yearcurrent_post + 
                  days_since_c_policy_yearcurrent_post_X_year2020 +
                  did_gm_avg_min +
                  did_EconomicSupportIndex_max_cat | geo + week | 0 | 0, 
                data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_gm_avg_min_AND_did_EconomicSupportIndex_max_cat")
  
  out15 <- felm(hits_ma7_log ~ pandemic_time + 
                  days_since_c_policy_yearcurrent_post + 
                  days_since_c_policy_yearcurrent_post_X_year2020 +
                  did_StringencyIndex_max +
                  did_EconomicSupportIndex_max_cat | geo + week | 0 | 0, 
                data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max_cat")
  
  out16 <- felm(hits_ma7_log ~ pandemic_time + 
                  days_since_c_policy_yearcurrent_post + 
                  days_since_c_policy_yearcurrent_post_X_year2020 +
                  did_gm_avg_min +
                  did_EconomicSupportIndex_max_cat +
                  did_gm_avg_min_X_did_EconomicSupportIndex_max_cat | geo + week | 0 | 0, 
                data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_gm_avg_min_AND_did_EconomicSupportIndex_max_cat_INTER")
  
  out17 <- felm(hits_ma7_log ~ pandemic_time + 
                  days_since_c_policy_yearcurrent_post + 
                  days_since_c_policy_yearcurrent_post_X_year2020 +
                  did_StringencyIndex_max +
                  did_EconomicSupportIndex_max_cat +
                  did_StringencyIndex_max_X_did_EconomicSupportIndex_max_cat | geo + week | 0 | 0, 
                data = df[df$keyword_en %in% keyword_i,]) %>%
    lm_post_confint_tidy() %>%
    mutate(type = "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max_cat_INTER")
  
  
  if(F){
    library(effects)
    
    out12_lm <- lm(hits_ma7_log ~ pandemic_time + 
                     days_since_c_policy_yearcurrent_post + 
                     days_since_c_policy_yearcurrent_post_X_year2020 +
                     did_StringencyIndex_max +
                     did_EconomicSupportIndex_max +
                     did_StringencyIndex_max*did_EconomicSupportIndex_max, 
                   data = df[df$keyword_en %in% keyword_i,])
    
    ef1 <- effect(term="did_StringencyIndex_max*did_EconomicSupportIndex_max", 
                  xlevels= list(did_EconomicSupportIndex_max=c(0, 50, 100)), 
                  mod=out12_lm)
    efdata1<-as.data.frame(ef1)
    efdata1$did_EconomicSupportIndex_max <- as.factor(efdata1$did_EconomicSupportIndex_max)
    
    ggplot(efdata1, 
           aes(x=did_StringencyIndex_max, 
               y=fit, 
               color=did_EconomicSupportIndex_max,
               group=did_EconomicSupportIndex_max)) + 
      geom_point() + 
      geom_line(size=1.2) +
      geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=did_EconomicSupportIndex_max),alpha=0.3) + 
      labs(title = "", 
           x= "Strigency", 
           y="Search Interest", 
           color="Economic Support", 
           fill="Economic Support") + 
      theme_classic() + 
      theme(text=element_text(size=20))
    
  }
  
  out_all <- bind_rows(
    out1,
    out2,
    out3,
    out4,
    out5,
    out6,
    out7,
    out8,
    out9,
    out10,
    out11,
    out12,
    out13,
    out14,
    out15,
    out16,
    out17
  )
  
  out_all$keyword <- keyword_i
  
  return(out_all)
}

coef_df <- map_df(unique(df$keyword_en), run_regs, df)

# Export -----------------------------------------------------------------------
saveRDS(coef_df,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                  "did_pooled_results.Rds"))






