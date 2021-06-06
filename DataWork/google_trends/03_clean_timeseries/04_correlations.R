# Calculate correlations

# Adds correlations to dataset, indicating best correlation and lag/lead of
# that correlation.

# 1. Add moving averages
# 2. Add leads/lags
# 3. Compute max correlations
# 4. Merge correlations with main data
# 5. Cleanup data
# 6. Export

# https://gist.github.com/drsimonj/2038ff9f9c67063f384f10fac95de566

begin_day <- c("2020-01-01",
               "2020-02-01",
               "2020-03-01",
               "2020-04-01",
               "2020-05-01",
               "2020-06-01",
               "2020-07-01",
               "2020-08-01",
               "2020-09-01",
               "2020-10-01",
               "2020-11-01",
               "2020-12-01")

for(begin_day_i in begin_day){
  
  print(paste(begin_day_i, "================================================="))

  # Load Data --------------------------------------------------------------------
  gtrends_full_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                       "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))
  
  gtrends_df <- gtrends_full_df %>%
    dplyr::select(geo, date, keyword_en, cases_new_ma7, death_new_ma7, hits_ma7) %>%
    dplyr::filter(date >= as.Date(begin_day_i))
  
  # Correlations across different leads/lags -----------------------------------
  gtrends_cor_long_df <- map_df(-21:21, function(leadlag){
    
    print(leadlag)
    
    ## Prep lead/lag hits variable
    leadlag_abs <- abs(leadlag)
    
    if(leadlag < 0){
      
      gtrends_df <- gtrends_df %>%
        dplyr::mutate(hits_ma7_leadlag = lag(hits_ma7, leadlag_abs))
      
    } else if(leadlag > 0){
      
      gtrends_df <- gtrends_df %>%
        dplyr::mutate(hits_ma7_leadlag = lead(hits_ma7, leadlag_abs))
      
    } else if(leadlag == 0){
      
      gtrends_df <- gtrends_df %>%
        dplyr::mutate(hits_ma7_leadlag = hits_ma7)
      
    }
    
    gtrends_cor_df_i <- gtrends_df %>%
      dplyr::arrange(date) %>%
      dplyr::filter(!is.na(hits_ma7_leadlag),
                    !is.na(cases_new_ma7),
                    !is.na(death_new_ma7)) %>%
      group_by(geo, keyword_en) %>%
      dplyr::summarise(cor_casesMA7_hitsMA7 = cor(cases_new_ma7, hits_ma7_leadlag),
                       cor_deathMA7_hitsMA7 = cor(death_new_ma7, hits_ma7_leadlag)) %>%
      dplyr::mutate(leadlag = leadlag)
    
    return(gtrends_cor_df_i)
  })
  
  # Summarize to geo/keyword level ---------------------------------------------
  
  #### Summarize to geo/keyword level
  gtrends_cor_df <- gtrends_cor_long_df %>%
    group_by(geo, keyword_en) %>%
    dplyr::summarise(cor_casesMA7_hitsMA7_nolag = cor_casesMA7_hitsMA7[leadlag == 0][1],
                     cor_deathMA7_hitsMA7_nolag = cor_deathMA7_hitsMA7[leadlag == 0][1],
                     
                     cor_casesMA7_hitsMA7_max = max(cor_casesMA7_hitsMA7, na.rm=T),
                     cor_deathMA7_hitsMA7_max = max(cor_deathMA7_hitsMA7, na.rm=T),
                     
                     cor_casesMA7_hitsMA7_mean = mean(cor_casesMA7_hitsMA7, na.rm=T),
                     cor_deathMA7_hitsMA7_mean = mean(cor_deathMA7_hitsMA7, na.rm=T),
                     
                     cor_casesMA7_hitsMA7_sd = sd(cor_casesMA7_hitsMA7, na.rm=T),
                     cor_deathMA7_hitsMA7_sd = sd(cor_deathMA7_hitsMA7, na.rm=T),
                     
                     cor_casesMA7_hitsMA7_lag = leadlag[which.max(cor_casesMA7_hitsMA7)],
                     cor_deathMA7_hitsMA7_lag = leadlag[which.max(cor_deathMA7_hitsMA7)]) %>%
    mutate(cor_casesMA7_hitsMA7_zscore = (cor_casesMA7_hitsMA7_max - cor_casesMA7_hitsMA7_mean) / cor_casesMA7_hitsMA7_sd,
           cor_deathMA7_hitsMA7_zscore = (cor_deathMA7_hitsMA7_max - cor_deathMA7_hitsMA7_mean) / cor_deathMA7_hitsMA7_sd,
           
           cor_casesMA7_hitsMA7_nolag_zscore = (cor_casesMA7_hitsMA7_nolag - cor_casesMA7_hitsMA7_mean) / cor_casesMA7_hitsMA7_sd,
           cor_deathMA7_hitsMA7_nolag_zscore = (cor_deathMA7_hitsMA7_nolag - cor_deathMA7_hitsMA7_mean) / cor_deathMA7_hitsMA7_sd)
  
  #### Stack Cases/Deaths together
  cor_max_df <- bind_rows(gtrends_cor_df %>%
                            dplyr::rename(cor_nolag = cor_casesMA7_hitsMA7_nolag,
                                          cor = cor_casesMA7_hitsMA7_max,
                                          lag = cor_casesMA7_hitsMA7_lag,
                                          zscore = cor_casesMA7_hitsMA7_zscore,
                                          zscore_nolag = cor_casesMA7_hitsMA7_nolag_zscore) %>%
                            mutate(type = "Cases"),
                          
                          gtrends_cor_df %>%
                            dplyr::rename(cor_nolag = cor_deathMA7_hitsMA7_nolag,
                                          cor = cor_deathMA7_hitsMA7_max,
                                          lag = cor_deathMA7_hitsMA7_lag,
                                          zscore = cor_deathMA7_hitsMA7_zscore,
                                          zscore_nolag = cor_deathMA7_hitsMA7_nolag_zscore) %>%
                            mutate(type = "Deaths")) %>%
    dplyr::select(geo, keyword_en, cor, cor_nolag, lag, zscore, zscore_nolag, type) 
  
  # Merge Correlations with main data --------------------------------------------
  gtrends_panel_df <- merge(gtrends_full_df, gtrends_cor_df, by = c("geo", "keyword_en"), all.x=T, all.y=F)
  
  # Merge "other day" with correlation data ------------------------------------
  # Merge non google trends data (eg, wdi) with correlations data
  gtrends_otherdata <- gtrends_full_df %>%
    dplyr::select(-contains("hits")) %>%
    dplyr::select(-contains("cases_new")) %>%
    dplyr::select(-contains("death_new")) %>%
    dplyr::select(-contains("days_since")) %>%
    dplyr::select(-contains("gmobility_")) %>%
    dplyr::select(-c(keyword, keyword_en, cases, death, date)) %>%
    distinct(geo, .keep_all = T)
  
  cor_max_df <- merge(cor_max_df, gtrends_otherdata, by = "geo", all.x=T, all.y=F)
  
  # Export ---------------------------------------------------------------------
  saveRDS(gtrends_panel_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                      "gtrends_full_timeseries",
                                      "correlation_datasets",
                                      paste0("gtrends_since",begin_day_i,".Rds")))
  
  saveRDS(cor_max_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                paste0("correlations_gtrends_since",begin_day_i,".Rds")))
}


