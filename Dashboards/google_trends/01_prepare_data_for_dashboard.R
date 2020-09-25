# Prepare Data for Dashboard

library(rmapshaper)

DASHBOARD_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData", "data")

keywords <- c("Corona Symptoms", "Coronavirus", "Coronavirus Symptoms",
              "Loss of Smell", "I Can't Smell", "Loss of Taste",
              "Fever", "Tired")

# World Shapefile --------------------------------------------------------------
world_sp <- readRDS(file.path(dropbox_file_path, "Data", "world_shapefile", 
                            "FinalData",
                            "TM_WORLD_BORDERS-0.3_simplified.Rds"))

world_sp$name <- world_sp$name %>% as.character()
world_sp$continent <- NA
world_sp$continent[world_sp$region %in% 2] <- "Africa"
world_sp$continent[world_sp$region %in% 142] <- "Asia"
world_sp$continent[world_sp$region %in% 150] <- "Europe"
world_sp$continent[world_sp$region %in% 9] <- "Oceania"
world_sp$continent[world_sp$region %in% 19] <- "Americas"

world_sp <- world_sp[world_sp$name != "Antarctica",]

world_sp@data <- world_sp@data %>%
  dplyr::select(iso2, name, continent) %>%
  dplyr::rename(geo = iso2)

world_sp$geo <- world_sp$geo %>% as.character()

world_df <- world_sp@data

saveRDS(world_sp, file.path(DASHBOARD_PATH, "world.Rds"))

# Correlations -----------------------------------------------------------------
cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                            "global_with_refstate",
                            paste0("gl_gtrends_ref","US","_adj_cases_correlations.Rds")))

cor_df$keyword_en <- cor_df$keyword_en %>% tools::toTitleCase()
cor_df <- merge(cor_df, world_df, by = "geo")

cor_df$keyword_en <- cor_df$keyword_en %>% tools::toTitleCase()
cor_df$keyword_en[cor_df$keyword_en %in% "i Can't Smell"] <- "I Can't Smell"

cor_df <- cor_df[cor_df$keyword_en %in% keywords,]

saveRDS(cor_df, file.path(DASHBOARD_PATH, "correlations.Rds"))

# gTrends ----------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_cor.Rds")))

gtrends_df$keyword_en <- gtrends_df$keyword_en %>% tools::toTitleCase()
gtrends_df$keyword_en[gtrends_df$keyword_en %in% "i Can't Smell"] <- "I Can't Smell"

gtrends_df <- gtrends_df[gtrends_df$date >= "2020-02-01",]
gtrends_df <- merge(gtrends_df, world_df, by = "geo")

gtrends_df <- gtrends_df[gtrends_df$keyword_en %in% keywords,]

gtrends_df <- gtrends_df %>%
  dplyr::select(keyword_en, date, hits, hits_ma7, name, continent, cases_new, death_new,
                cor_casesMA7_hitsMA7_max, cor_casesMA7_hitsMA7_lag, cor_deathMA7_hitsMA7_max,
                cor_deathMA7_hitsMA7_lag,
                cases_total, death_total)

saveRDS(gtrends_df, file.path(DASHBOARD_PATH, "gtrends.Rds"))

# Sparkline Table --------------------------------------------------------------
gtrends_sum_df <- gtrends_df %>% 
  arrange(date) %>%
  group_by(name, continent, keyword_en,
           cases_total, death_total) %>% 
  summarize(cases_new_spark = spk_chr(cases_new,
                                      lineColor = 'orange', 
                                      fillColor = 'orange',
                                      chartRangeMin = 0,
                                      chartRangeMax = 8,
                                      width = 180,
                                      height = 100,
                                      highlightLineColor = 'orange', 
                                      highlightSpotColor = 'orange'),
            death_new_spark = spk_chr(death_new,
                                      lineColor = 'orange', 
                                      fillColor = 'orange',
                                      chartRangeMin = 0,
                                      chartRangeMax = 8,
                                      width = 180,
                                      height = 100,
                                      highlightLineColor = 'orange', 
                                      highlightSpotColor = 'orange'),
            hits_ma7_spark = spk_chr(hits_ma7,
                                     lineColor = 'forestgreen', 
                                     fillColor = 'forestgreen',
                                     chartRangeMin = 0,
                                     chartRangeMax = 8,
                                     width = 180,
                                     height = 100,
                                     highlightLineColor = 'orange', 
                                     highlightSpotColor = 'orange'),
            cor_casesMA7_hitsMA7_max = cor_casesMA7_hitsMA7_max[1], 
            cor_casesMA7_hitsMA7_lag = cor_casesMA7_hitsMA7_lag[1],
            cor_deathMA7_hitsMA7_max = cor_deathMA7_hitsMA7_max[1], 
            cor_deathMA7_hitsMA7_lag = cor_deathMA7_hitsMA7_lag[1],) 

saveRDS(gtrends_sum_df, file.path(DASHBOARD_PATH, "gtrends_spark.Rds"))

