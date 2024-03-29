# Applies equation to make hits comparable across time/states using a comparison
# state

for(keyword_type in c("symptoms", "contain", "vaccine")){
  print(paste(keyword_type, "================================================"))
  
  # Load Data --------------------------------------------------------------------
  gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                  "gtrends_full_timeseries", 
                                  paste0("gtrends_otherdata_complete_",keyword_type,".Rds")))
  
  # Moving Averaage --------------------------------------------------------------
  ## Doesn't work with non-leading NAs, so remove NAs then merge back in
  gtrends_df <- gtrends_df %>%
    arrange(date) %>%
    group_by(geo, keyword_en) %>%
    dplyr::mutate(hits_ma7 = runMean(hits, n = 7)) %>%
    ungroup()
  
  # gtrends_ma_hits_df <- gtrends_df %>%
  #   arrange(date) %>%
  #   group_by(geo, keyword_en) %>%
  #   dplyr::mutate(hits_ma7 = runMean(hits, n = 7)) %>%
  #   dplyr::select(geo, date, language, keyword_en, hits_ma7) 
  # 
  # gtrends_df <- gtrends_df %>%
  #   left_join(gtrends_ma_hits_df, by = c("geo", "date", "keyword_en", "language")) 
  
  # Days since lockdown - year agnostic ------------------------------------------
  # Calculate year since lockdown, agnostic of year. So April 21, 2020 and April
  # 21, 2019 would have the same value
  
  gtrends_df <- gtrends_df %>%
    dplyr::mutate(year = date %>% year,
                  c_policy_mm_dd = c_policy_first_date %>% substring(6,10)) %>%
    dplyr::mutate(c_policy_2020 = paste0("2020-", c_policy_mm_dd) %>% ymd(),
                  c_policy_2019 = paste0("2019-", c_policy_mm_dd) %>% ymd(),
                  pandemic_time = as.numeric(date >= ymd("2019-09-01"))) %>%
    dplyr::mutate(days_since_c_policy_2020 = as.numeric(date - c_policy_2020),
                  days_since_c_policy_2019 = as.numeric(date - c_policy_2019)) %>%
    dplyr::mutate(days_since_c_policy_yearcurrent = case_when(
      pandemic_time %in% 1 ~ days_since_c_policy_2020,
      pandemic_time %in% 0 ~ days_since_c_policy_2019
    )) %>%
    dplyr::mutate(days_since_c_policy_yearcurrent_post = days_since_c_policy_yearcurrent >= 0) %>%
    dplyr::select(-c(c_policy_mm_dd))
  
  #gtrends_df <- gtrends_df %>%
  #  dplyr::mutate(year = date %>% year,
  #                c_policy_mm_dd = c_policy %>% substring(6,10)) %>%
  #  dplyr::mutate(c_policy_yearcurrent = paste0(year, "-", c_policy_mm_dd) %>% ymd()) %>%
  #  dplyr::mutate(days_since_c_policy_yearcurrent = date - c_policy_yearcurrent) %>%
  #  dplyr::mutate(days_since_c_policy_yearcurrent_post = days_since_c_policy_yearcurrent >= 0) %>%
  #  dplyr::select(-c(c_policy_mm_dd))
  
  # Variable Fixes ---------------------------------------------------------------
  gtrends_df <- gtrends_df %>%
    mutate(date = date %>% as.Date()) %>%
    
    group_by(geo) %>%
    mutate(cases_total = max(cases, na.rm = T),
           death_total = max(death, na.rm = T)) %>%
    ungroup()
  
  gtrends_df$cases_total[gtrends_df$cases_total %in% -Inf] <- NA
  gtrends_df$death_total[gtrends_df$death_total %in% -Inf] <- NA
  
  # Other variables --------------------------------------------------------------
  gtrends_df <- gtrends_df %>%
    dplyr::mutate(mm_dd = date %>% substring(6,10))
  
  # Add Categories -------------------------------------------------------------
  if(keyword_type %in% "vaccine"){
    
    gtrends_df <- gtrends_df %>%
      dplyr::mutate(keyword_cat = case_when(
        # Searches specifically related to appointments
        keyword_en %in% c("can i get the covid vaccine",
                          "covid vaccine appointment",
                          "vaccine appointment",
                          "covid vaccine center") ~ "Vaccine Appointment",
        
        keyword_en %in% c("covid vaccine",
                          "covid vaccine priority",
                          "covid vaccine priority list",
                          "covid vaccine approved",
                          "is covid vaccine approved", ## COULD DELETE
                          "covid vaccine second dose",
                          "vaccine near me", ## COULD DELETE
                          "where to get covid vaccine",
                          "vaccine") ~ "Vaccine General",
        
        keyword_en %in% c("covid vaccine blood clots",
                          "covid vaccine safety",
                          "covid vaccine sick", ## COULD DELETE
                          "covid vaccine side effects",
                          "safety of covid vaccine",
                          "vaccine allergy",
                          "long term effects of covid vaccine",
                          "vaccine reaction",
                          "fear of needles",
                          "needle phobia",
                          "trypanophobia") ~ "Side Effects & Safety",
        
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
    
  }
  
  # Export =======================================================================
  saveRDS(gtrends_df,
          file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries", 
                    paste0("gtrends_otherdata_varclean_complete_",keyword_type,".Rds")))
  
  # ## Export subsets for quicker loading
  # saveRDS(gtrends_df %>%
  #           dplyr::filter(keyword_en %in% KEYWORDS_CONTAIN_USE),
  #         file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
  #                   "gtrends_full_timeseries", "gtrends_otherdata_varclean_complete_containment.Rds"))
  # 
  # saveRDS(gtrends_df %>%
  #           dplyr::filter(keyword_en %in% KEYWORDS_SYMTPOMS),
  #         file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
  #                   "gtrends_full_timeseries", "gtrends_otherdata_varclean_complete_symptoms.Rds"))
  # 
  # saveRDS(gtrends_df %>%
  #           dplyr::filter(keyword_en %in% VACCINE_KEYWORDS),
  #         file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
  #                   "gtrends_full_timeseries", "gtrends_otherdata_varclean_complete_vaccine.Rds"))
  
  
}



