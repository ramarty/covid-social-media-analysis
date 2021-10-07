# Lockdown Difference-in-Difference Analysis

# TODO: "Kiribati" has NA hits? Not being completed in 2018; no data then,
#       but should be completed.

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean_complete.Rds"))

# Add variables ----------------------------------------------------------------
## Add variables needed for regression
gtrends_df <- gtrends_df %>%
  dplyr::mutate(days_since_c_policy_yearcurrent_post_X_year2020 = 
                  days_since_c_policy_yearcurrent_post*pandemic_time) %>%
  dplyr::mutate(week = date %>% week,
                wday = date %>% wday)

## Log hits
gtrends_df$hits_ma7_log <- gtrends_df$hits_ma7 + abs(min(gtrends_df$hits_ma7, na.rm=T))
gtrends_df$hits_ma7_log <- log(gtrends_df$hits_ma7_log+1)

## Subset data -----------------------------------------------------------------

## Additional filtering
gtrends_df <- gtrends_df %>%
  dplyr::filter(
    # Filter to select keywords
    keyword_en %in% KEYWORDS_CONTAIN_USE,
    
    # Remove NA hits_ma7 values (MA creates 
    # missing values at beginning of time series)
    !is.na(hits_ma7),
    
    # Remove countries with no lockdown data
    !is.na(days_since_c_policy_yearcurrent)
    )

## Subset to near lockdown period [pandemic and pre-pandemic period]
gtrends_df <- gtrends_df %>%
  dplyr::filter(abs(days_since_c_policy_yearcurrent) <= 90)

## Remove country-keyword with no hits (only consider 30 days before/after lockdown)
gtrends_df <- gtrends_df %>%
  dplyr::mutate(hits_within_X_days = case_when(
    abs(days_since_c_policy_yearcurrent) <= 90 ~ hits
  )) %>%
  group_by(geo, keyword_en) %>%
  dplyr::mutate(N_hits_above_0 = sum(hits_within_X_days > 0, na.rm=T)) %>%
  ungroup() %>%
  dplyr::filter(N_hits_above_0 > 0) %>%
  dplyr::select(-c(N_hits_above_0, hits_within_X_days))

# Regressions ------------------------------------------------------------------
# gtrends_df <- gtrends_df %>%
#   dplyr::filter(keyword_en %in% KEYWORDS_CONTAIN_USE) %>% # KEYWORDS_CONTAIN_USE
#   dplyr::filter(!is.na(days_since_c_policy_yearcurrent),
#                 !is.na(hits_ma7)) 
# 
# keyword_i <- "social isolation"
# geo_i <- "US"
# 
# run_reg <- function(keyword_i, geo_i){
#   # Function to estimate model for keyword_i and region_i
#   
#   ## Subset to keyword and country
#   df_i <- gtrends_df %>%
#     dplyr::filter(keyword_en %in% keyword_i,
#                   geo %in% geo_i)
#   
#   have_gtrends_data <- ifelse(nrow(df_i[!is.na(df_i$hits_ma7),]) > 0, T, F)
#   have_lockdown_data <- ifelse(nrow(df_i[!is.na(df_i$days_since_c_policy_yearcurrent),]) > 0, T, F)
#   
#   ## Further subset
#   df_i <- df_i %>%
#     dplyr::filter(abs(days_since_c_policy_yearcurrent) <= 30) %>%
#     dplyr::filter(!is.na(hits_ma7),
#                   !is.na(days_since_c_policy_yearcurrent_post))
#   
#   if((nrow(df_i) > 0) & (length(unique(df_i$pandemic_time)) > 1)){
#     
#     # FE: mm_dd
#     out <- df_i
#   } else{
#     print(paste0("----", geo_i))
#     out <- data.frame(NULL)
#   }
#   
#   return(out)
# }
# 
# ## Grab list of regions
# wb_geo_all <- gtrends_df$geo %>% 
#   unique() %>% 
#   na.omit() %>% 
#   as.character()
# 
# geo_results_df <- map_df(wb_geo_all, function(geo_i){
#   print(geo_i)
#   map_df(KEYWORDS_CONTAIN_USE, run_reg, geo_i)
# })

# Export Results ---------------------------------------------------------------
saveRDS(gtrends_df,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                  "did_pooled_data.Rds"))


