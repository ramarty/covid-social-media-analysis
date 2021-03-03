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

comparison_iso <- "US"

begin_day <- c("2020-02-01",
               "2020-03-01",
               "2020-04-01",
               "2020-05-01",
               "2020-06-01",
               "2020-07-01",
               "2020-08-01")

for(begin_day_i in begin_day){
  
  print(begin_day_i)
  
  # Load Data --------------------------------------------------------------------
  gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                  "global_with_refstate",
                                  paste0("gl_gtrends_ref",comparison_iso,"_adj_cases.Rds")))
  
  # Moving Average ---------------------------------------------------------------
  gtrends_df <- gtrends_df %>%
    arrange(date) %>%
    group_by(geo, keyword_en) %>%
    mutate(hits_ma7 = runMean(hits, n = 7),
           cases_new_ma7 = runMean(cases_new, n = 7),
           death_new_ma7 = runMean(death_new, n = 7))
  
  # Lags/Leads -------------------------------------------------------------------
  lead_lags <- 1:21
  
  hits_ma7_lead_functions <- setNames(paste("dplyr::lead(., ", lead_lags, ")"), 
                                      paste0("hits_ma7_lead_", lead_lags))
  
  hits_ma7_lag_functions <- setNames(paste("dplyr::lag(., ", lead_lags, ")"), 
                                     paste0("hits_ma7_lag_", lead_lags))
  
  gtrends_df <- gtrends_df %>%
    arrange(date) %>%
    group_by(geo, keyword_en) %>%
    mutate_at(vars(hits_ma7), funs_(hits_ma7_lead_functions)) %>%
    mutate_at(vars(hits_ma7), funs_(hits_ma7_lag_functions)) %>%
    mutate(hits_ma7_lead_0 = hits_ma7)
  
  # Correlations -----------------------------------------------------------------
  # 1. Stack leads/lags
  # 2. Correlations, grouped by: geo, keyword, lead/lag
  # 3. Merge back in
  
  #### Lead lag variables
  hits_leadlag_vars <- names(gtrends_df) %>% 
    str_subset("hits_ma7_") %>%
    str_subset("lead|lag")
  
  #### Stack
  gtrends_long_df <- gtrends_df %>%
    
    ## Restrict to after February
    filter(date >= as.Date(begin_day_i)) %>%
    
    ## To Long
    dplyr::select(c("geo", "keyword_en", "date", "cases_new_ma7", "death_new_ma7", 
                    hits_leadlag_vars)) %>%
    pivot_longer(cols = -c(geo, keyword_en, date, cases_new_ma7, death_new_ma7)) %>%
    dplyr::rename(hits_ma7 = value) %>%
    mutate(leadlag = name %>%
             str_replace_all("hits_ma7_", "") %>%
             str_replace_all("_", "") %>%
             str_replace_all("lag", "-") %>%
             str_replace_all("lead", "") %>%
             as.numeric()) %>%
    
    ## Replace NAs with 0s
    mutate(hits_ma7 = replace_na(hits_ma7, 0),
           cases_new_ma7 = replace_na(cases_new_ma7, 0),
           death_new_ma7 = replace_na(death_new_ma7, 0))
  
  #### Add Correlation
  # Give dummy value for NA; needed for next step. Remove these cases later
  gtrends_long_df <- gtrends_long_df %>%
    group_by(geo, keyword_en, leadlag) %>%
    mutate(cor_casesMA7_hitsMA7 = cor(cases_new_ma7, hits_ma7),
           cor_deathMA7_hitsMA7 = cor(death_new_ma7, hits_ma7))
  
  #### Add Z-Score
  # If NA, give index of 1
  which.max_na <- function(x){
    out <- which.max(x)
    if(length(out) %in% 0) out <- 1
    return(out)
  }
  
  gtrends_long_df <- gtrends_long_df %>%
    group_by(geo, keyword_en) %>%
    dplyr::summarise(cor_casesMA7_hitsMA7_nolag = cor_casesMA7_hitsMA7[leadlag == 0][1],
                     cor_deathMA7_hitsMA7_nolag = cor_deathMA7_hitsMA7[leadlag == 0][1],
                     
                     cor_casesMA7_hitsMA7_max = max(cor_casesMA7_hitsMA7, na.rm=T),
                     cor_deathMA7_hitsMA7_max = max(cor_deathMA7_hitsMA7, na.rm=T),
                     
                     cor_casesMA7_hitsMA7_mean = mean(cor_casesMA7_hitsMA7, na.rm=T),
                     cor_deathMA7_hitsMA7_mean = mean(cor_deathMA7_hitsMA7, na.rm=T),
                     
                     cor_casesMA7_hitsMA7_sd = sd(cor_casesMA7_hitsMA7, na.rm=T),
                     cor_deathMA7_hitsMA7_sd = sd(cor_deathMA7_hitsMA7, na.rm=T),
                     
                     cor_casesMA7_hitsMA7_lag = leadlag[which.max_na(cor_casesMA7_hitsMA7)],
                     cor_deathMA7_hitsMA7_lag = leadlag[which.max_na(cor_deathMA7_hitsMA7)]) %>%
    mutate(cor_casesMA7_hitsMA7_zscore = (cor_casesMA7_hitsMA7_max - cor_casesMA7_hitsMA7_mean) / cor_casesMA7_hitsMA7_sd,
           cor_deathMA7_hitsMA7_zscore = (cor_deathMA7_hitsMA7_max - cor_deathMA7_hitsMA7_mean) / cor_deathMA7_hitsMA7_sd,
           
           cor_casesMA7_hitsMA7_nolag_zscore = (cor_casesMA7_hitsMA7_nolag - cor_casesMA7_hitsMA7_mean) / cor_casesMA7_hitsMA7_sd,
           cor_deathMA7_hitsMA7_nolag_zscore = (cor_deathMA7_hitsMA7_nolag - cor_deathMA7_hitsMA7_mean) / cor_deathMA7_hitsMA7_sd)
  
  gtrends_long_df$cor_casesMA7_hitsMA7_nolag[is.na(gtrends_long_df$cor_casesMA7_hitsMA7_zscore)] <- NA
  gtrends_long_df$cor_casesMA7_hitsMA7_max[is.na(gtrends_long_df$cor_casesMA7_hitsMA7_zscore)] <- NA
  gtrends_long_df$cor_casesMA7_hitsMA7_mean[is.na(gtrends_long_df$cor_casesMA7_hitsMA7_zscore)] <- NA
  gtrends_long_df$cor_casesMA7_hitsMA7_sd[is.na(gtrends_long_df$cor_casesMA7_hitsMA7_zscore)] <- NA
  gtrends_long_df$cor_casesMA7_hitsMA7_lag[is.na(gtrends_long_df$cor_casesMA7_hitsMA7_zscore)] <- NA
  
  gtrends_long_df$cor_deathMA7_hitsMA7_nolag[is.na(gtrends_long_df$cor_deathMA7_hitsMA7_zscore)] <- NA
  gtrends_long_df$cor_deathMA7_hitsMA7_max[is.na(gtrends_long_df$cor_deathMA7_hitsMA7_zscore)] <- NA
  gtrends_long_df$cor_deathMA7_hitsMA7_mean[is.na(gtrends_long_df$cor_deathMA7_hitsMA7_zscore)] <- NA
  gtrends_long_df$cor_deathMA7_hitsMA7_sd[is.na(gtrends_long_df$cor_deathMA7_hitsMA7_zscore)] <- NA
  gtrends_long_df$cor_deathMA7_hitsMA7_lag[is.na(gtrends_long_df$cor_deathMA7_hitsMA7_zscore)] <- NA
  
  #### Append together
  cor_max_df <- bind_rows(gtrends_long_df %>%
                            dplyr::rename(cor_nolag = cor_casesMA7_hitsMA7_nolag,
                                          cor = cor_casesMA7_hitsMA7_max,
                                          lag = cor_casesMA7_hitsMA7_lag,
                                          zscore = cor_casesMA7_hitsMA7_zscore,
                                          zscore_nolag = cor_casesMA7_hitsMA7_nolag_zscore) %>%
                            mutate(type = "Cases"),
                          
                          gtrends_long_df %>%
                            dplyr::rename(cor_nolag = cor_deathMA7_hitsMA7_nolag,
                                          cor = cor_deathMA7_hitsMA7_max,
                                          lag = cor_deathMA7_hitsMA7_lag,
                                          zscore = cor_deathMA7_hitsMA7_zscore,
                                          zscore_nolag = cor_deathMA7_hitsMA7_nolag_zscore) %>%
                            mutate(type = "Deaths")) %>%
    dplyr::select(geo, keyword_en, cor, cor_nolag, lag, zscore, zscore_nolag, type) %>%
    filter(!is.na(zscore))
  
  # Merge Correlations with main data --------------------------------------------
  gtrends_df <- merge(gtrends_df, gtrends_long_df, by = c("geo", "keyword_en"), all.x=T, all.y=F)
  
  # Cleanup gtrends_df -----------------------------------------------------------
  
  #### Remove lead/lags
  for(var in names(gtrends_df)){
    if(grepl("_lag_|_lead_", var)) gtrends_df[[var]] <- NULL
  }
  
  #### Add total cases/deaths 
  gtrends_df <- gtrends_df %>%
    group_by(geo) %>%
    mutate(cases_total = max(cases, na.rm = T),
           death_total = max(death, na.rm = T))
  
  # Export -----------------------------------------------------------------------
  saveRDS(gtrends_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_cor_since_",begin_day_i,".Rds")))
  
  saveRDS(cor_max_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",begin_day_i,".Rds")))
}


