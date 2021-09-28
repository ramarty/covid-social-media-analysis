# DiD: Pooled Results

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                        "did_pooled_data.Rds"))

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
                  min(gmobility_retail_and_recreation_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                     (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_grocery_min = 
                  min(gmobility_grocery_and_pharmacy_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                    (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_parks_min = 
                  min(gmobility_parks_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                     (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_transit_min = 
                  min(gmobility_transit_stations_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_workplace_min = 
                  min(gmobility_workplaces_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                          (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  dplyr::mutate(gm_residential_max = 
                  max(gmobility_residential_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                           (days_since_c_policy_yearcurrent_post %in% T)])) %>%
  ungroup() %>%
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
                        gm_avg_min), scale) %>%
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
                did_income_upper_middle = days_since_c_policy_yearcurrent_post_X_year2020 * income_upper_middle) 

out <- felm(hits_ma7_log ~ pandemic_time + 
              days_since_c_policy_yearcurrent_post + 
              days_since_c_policy_yearcurrent_post_X_year2020 | geo + week + wday | 0 | 0, 
            data = df[df$keyword_en %in% "boredom",])

out <- felm(hits_ma7_log ~ pandemic_time + 
              days_since_c_policy_yearcurrent_post + 
              days_since_c_policy_yearcurrent_post_X_year2020 +
              income_low_middle +
              income_upper_middle +
              income_high +
              did_income_low_middle +
              did_income_upper_middle +
              did_income_high| geo + week + wday | 0 | 0, 
            data = df[df$keyword_en %in% "boredom",])
summary(out)



out <- felm(hits_ma7_log ~ pandemic_time + 
              days_since_c_policy_yearcurrent_post + 
              days_since_c_policy_yearcurrent_post_X_year2020 +
              gm_avg_min + 
              did_gm_avg_min  | week + wday | 0 | 0, 
            data = df[df$keyword_en %in% "boredom",])

out <- felm(hits_ma7_log ~ pandemic_time + 
              days_since_c_policy_yearcurrent_post + 
              days_since_c_policy_yearcurrent_post_X_year2020 +
              did_ln_cases_total +
              gm_avg_min + 
              did_gm_avg_min +
              did_ln_cases_total | geo + week + wday | 0 | 0, 
            data = df[df$keyword_en %in% "unemployment",])

summary(out)

out <- felm(hits_ma7_log ~ pandemic_time + 
              days_since_c_policy_yearcurrent_post + 
              days_since_c_policy_yearcurrent_post_X_year2020 +
              did_gm_avg_min  | geo + week + wday | 0 | 0, 
            data = df[df$keyword_en %in% "social isolation",])

summary(out)

a <- df[df$keyword_en %in% "unemployment",]

summary(out)
