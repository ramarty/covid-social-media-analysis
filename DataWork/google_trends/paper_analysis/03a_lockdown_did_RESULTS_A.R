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
  dplyr::mutate(year2020 = as.numeric(year == 2020)) %>%
  dplyr::mutate(days_since_lockdown_min_yearcurrent_post_X_year2020 = 
                  days_since_lockdown_min_yearcurrent_post*year2020)

# Regressions ------------------------------------------------------------------
keywords_en_use <- c("social distance", "stay at home", "boredom", "anxiety", "suicide",
                     "insomnia", "social isolation", "loneliness", "divorce",
                     "panic attack",
                     "fever",
                     "worried health", "hysteria", "overwhelmed", "anxiety symptoms",
                     "anxiety attack", #"symptoms of pannic attack",
                     "depressed", "lonely", "suicidal", "abuse",
                     "therapist near me", "online therapist",
                     "deep breathing", "body scan meditation",
                     "unemployment", "unemployment insurance")

keywords_en_use <- c("stay at home", "boredom", "anxiety")

run_reg <- function(keyword_i, region_i){
  print(keyword_i)
  
  df_i <- gtrends_df[gtrends_df$keyword_en %in% keyword_i,]
  if(region_i != "Global"){
    df_i <- df_i[df_i$wb_region %in% region_i,]
  }
  
  felm(hits_ma7 ~ year2020 + 
         days_since_lockdown_min_yearcurrent_post + 
         days_since_lockdown_min_yearcurrent_post_X_year2020 | mm_dd | 0 | 0, 
       data = df_i) %>%
    lm_post_confint_tidy() %>%
    dplyr::mutate(keyword = keyword_i,
                  region = region_i)
}

wb_regions_all <- gtrends_df$wb_region %>% unique()
wb_regions_all <- wb_regions_all[!is.na(wb_regions_all)]

global_results_df <- map_df(keywords_en_use, run_reg, "Global")
region_results_df <- map_df(wb_regions_all, function(region_i){
  map_df(keywords_en_use, run_reg, region_i)
})

results_all <- bind_rows(global_results_df, region_results_df)

# Export Results ---------------------------------------------------------------
saveRDS(results_all,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                  "did_results.Rds"))


