# Prepare Data for Dashboard

DASHBOARD_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData", "data")

# gTrends ----------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases.Rds")))


gtrends_df <- gtrends_df[gtrends_df$keyword_en %in% c("i can't smell", 
                                                      "loss of smell", 
                                                      "fever", 
                                                      "cough",
                                                      "coronavirus",
                                                      "corona symptoms",
                                                      "coronavirus symptoms",
                                                      "covid symptoms",
                                                      "loss of taste",
                                                      "tired"),]

#### Create Indices
# index_all_df <- gtrends_df %>%
#   group_by(geo, Country, date, cases, death, cases_new, death_new) %>%
#   dplyr::summarise(hits = sum(hits, na.rm = T)) %>%
#   dplyr::mutate(keyword = "Index - All",
#                 keyword_en = "Index - All") %>%
#   ungroup()
# 
# gtrends_df <- bind_rows(index_all_df, gtrends_df)

#### Add variables
gtrends_df <- gtrends_df %>%
  arrange(date) %>%
  group_by(geo, keyword_en) %>%
  dplyr::mutate(cases_total = max(cases, na.rm = T),
                death_total = max(death, na.rm = T)) %>%
  dplyr::mutate(cases_new_ma7 = runMean(cases_new, n = 7),
                death_new_ma7 = runMean(death_new, n = 7),
                hits_ma7 = runMean(hits, n = 7)) %>%
  dplyr::filter(!is.na(hits_ma7)) %>%
  dplyr::mutate(cases_hits_cor = cor(cases_new, hits_ma7),
                death_hits_cor = cor(death_new, hits_ma7)) %>%
  ungroup() %>%
  mutate(keyword_en = keyword_en %>% tools::toTitleCase())

gtrends_df$keyword_en[gtrends_df$keyword_en %in% "i Can't Smell"] <- "I Can't Smell"

#gtrends_df <- gtrends_df[gtrends_df$keyword_en %in% 
#                           c("Loss of Smell", "Cough", "Fever", "I Can't Smell"),]

gtrends_df <- gtrends_df[gtrends_df$date >= "2020-02-01",]

#### Add in ISO 3 and Continent
world_df <- readRDS(file.path(dropbox_file_path, "Data", "world_shapefile", "FinalData", "world_ne.Rds"))
world_df$geometry <- NULL

gtrends_df <- merge(gtrends_df, world_df, by = "geo", all.x=F, all.y = F)

saveRDS(gtrends_df, file.path(DASHBOARD_PATH, "gtrends.Rds"))

# World Shapefile --------------------------------------------------------------
world_ne_sf <- readRDS(file.path(dropbox_file_path, "Data", "world_shapefile", "FinalData",
                                 "world_ne.Rds"))

world_ne_sf <- world_ne_sf[!(world_ne_sf$name %in% "Antarctica"),]
world_ne_sf <- world_ne_sf[world_ne_sf$continent != "Seven seas (open ocean)",]

saveRDS(world_ne_sf, file.path(DASHBOARD_PATH, "world_ne.Rds"))

# Correlations -----------------------------------------------------------------
cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                            "global_with_refstate",
                            paste0("gl_gtrends_ref","US","_adj_cases_correlations.Rds")))

cor_df <- cor_df %>%
  filter(hits_type %in% "hits") %>%
  dplyr::select(Country, geo, time_lag, keyword_en,
                cases_total, death_total,
                cor_cases_new, cor_death_new) %>%
  unique()

saveRDS(cor_df, file.path(DASHBOARD_PATH, "correlations.Rds"))

# Correlations: Time Lag Max Date ----------------------------------------------
cor_max_df <- cor_df %>%
  filter(!is.na(cor_cases_new),
         !is.na(cor_death_new)) %>%
  group_by(Country, geo, keyword_en, cases_total, death_total) %>%
  dplyr::summarise(time_lag_cases_cor_max = time_lag[which.max(cor_cases_new)],
                   time_lag_death_cor_max = time_lag[which.max(cor_death_new)],
                   
                   cor_cases_new_max = cor_cases_new[which.max(cor_cases_new)],
                   cor_death_new_max = cor_death_new[which.max(cor_death_new)]) %>%
  unique()

saveRDS(cor_max_df, file.path(DASHBOARD_PATH, "correlations_max_lag.Rds"))








