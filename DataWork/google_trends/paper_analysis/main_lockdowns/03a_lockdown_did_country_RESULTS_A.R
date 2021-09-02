# Lockdown Difference-in-Difference Analysis

lm_post_confint_tidy <- function(lm){
  
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint$tvalue <- summary(lm)$coefficients[,3] %>% as.vector()
  lm_confint$pvalue <- summary(lm)$coefficients[,4] %>% as.vector()
  
  return(lm_confint)
}

# Load / Prep Data -------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

gtrends_df <- gtrends_df %>%
  dplyr::filter(year %in% c(2019, 2020)) %>%
  dplyr::mutate(year2020 = as.numeric(year >= 2020)) %>%
  dplyr::mutate(days_since_lockdown_min_yearcurrent_post_X_year2020 = 
                  days_since_lockdown_min_yearcurrent_post*year2020) %>%
  dplyr::mutate(week = date %>% week,
                wday = date %>% wday)

# https://stackoverflow.com/questions/14169620/add-a-month-to-a-date
max_lockdown_date <- gtrends_df$lockdown_date_min %>% 
  max(na.rm=T) %m+% 
  months(1) %>%
  str_replace("2020-", "")

#gtrends_df <- gtrends_df[gtrends_df$mm_dd <= max_lockdown_date,]

# Log
gtrends_df$hits_ma7_log <- gtrends_df$hits_ma7 + abs(min(gtrends_df$hits_ma7, na.rm=T))
gtrends_df$hits_ma7_log <- log(gtrends_df$hits_ma7_log+1)

# Regressions ------------------------------------------------------------------
keywords_en_use <- c("social distance", "stay at home", "boredom", "anxiety", "suicide",
                     "insomnia", "social isolation", "loneliness", "divorce",
                     "panic attack",
                     "fever",
                     #"worried health", 
                     "hysteria", "overwhelmed", "anxiety symptoms",
                     "anxiety attack", "symptoms of panic attack",
                     "depressed", "lonely", "suicidal", "abuse",
                     "therapist near me", 
                     "online therapist",
                     "deep breathing", "body scan meditation",
                     "unemployment", "unemployment insurance")

keywords_en_use <- c("social distance",
                     "stay at home",
                     "unemployment",
                     "unemployment insurance",
                     "boredom",
                     "anxiety",
                     "anxiety attack",
                     "anxiety symptoms",
                     "overwhelmed", # panic
                     "hysteria",
                     "suicide",
                     "insomnia",
                     "overwhelmed",
                     "social isolation",
                     "lonely",
                     "loneliness",
                     "divorce")

# keywords_en_use <- c("boredom",
#                      "unemployment",
#                      "social isolation",
#                      "anxiety",
#                      "anxiety attack",
#                      "suicide")

gtrends_df <- gtrends_df %>%
  dplyr::filter(keyword_en %in% keywords_en_use) %>%
  dplyr::filter(!is.na(lockdown_date_min_yearcurrent),
                !is.na(hits_ma7)) 

keyword_i <- "social isolation"
geo_i <- "US"

run_reg <- function(keyword_i, geo_i){
  # Function to estimate model for keyword_i and region_i
  
  ## Subset to keyword and country
  df_i <- gtrends_df %>%
    dplyr::filter(keyword_en %in% keyword_i,
                  geo %in% geo_i)
  
  have_gtrends_data <- ifelse(nrow(df_i[!is.na(df_i$hits_ma7),]) > 0, T, F)
  have_lockdown_data <- ifelse(nrow(df_i[!is.na(df_i$days_since_lockdown_min_yearcurrent),]) > 0, T, F)
  
  ## Further subset
  df_i <- df_i %>%
    dplyr::filter(abs(days_since_lockdown_min_yearcurrent) <= 30) %>%
    dplyr::filter(!is.na(hits_ma7),
                  !is.na(days_since_lockdown_min_yearcurrent_post))
  
  if((nrow(df_i) > 0) & (length(unique(df_i$year2020)) > 1)){
    
    # FE: mm_dd
    out <- felm(hits_ma7_log ~ year2020 + 
                  days_since_lockdown_min_yearcurrent_post + 
                  days_since_lockdown_min_yearcurrent_post_X_year2020 | week + wday | 0 | 0, 
                data = df_i) %>%
      lm_post_confint_tidy() %>%
      dplyr::mutate(keyword = keyword_i,
                    geo = geo_i,
                    have_gtrends_data = T,
                    have_lockdown_data = T,
                    wb_region = df_i$wb_region[1])
  } else{
    out <- data.frame(keyword = keyword_i,
                      geo = geo_i,
                      have_gtrends_data = have_gtrends_data,
                      have_lockdown_data = have_lockdown_data)
  }
  
  return(out)
}

## Grab list of regions
wb_geo_all <- gtrends_df$geo %>% 
  unique() %>% 
  na.omit() %>% 
  as.character()

geo_results_df <- map_df(wb_geo_all, function(geo_i){
  print(geo_i)
  map_df(keywords_en_use, run_reg, geo_i)
})

# Export Results ---------------------------------------------------------------
saveRDS(geo_results_df,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                  "did_results_country.Rds"))


