# Prepare Data for Dashboard

DASHBOARD_PATH <- file.path(github_file_path, "Dashboards", "google_trends", "data")

# gTrends ----------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases.Rds")))

#### Create Indices
index_all_df <- gtrends_df %>%
  group_by(geo, Country, date, cases, death, cases_new, death_new) %>%
  dplyr::summarise(hits = sum(hits, na.rm = T)) %>%
  dplyr::mutate(keyword = "Index - All",
                keyword_en = "Index - All") %>%
  ungroup()

gtrends_df <- bind_rows(index_all_df, gtrends_df)

#### Add variables
gtrends_df <- gtrends_df %>%
  group_by(geo, keyword_en) %>%
  mutate(cases_total = max(cases, na.rm = T),
         death_total = max(death, na.rm = T)) %>%
  mutate(cases_new_ma7 = runMean(cases_new, n = 7),
         death_new_ma7 = runMean(death_new, n = 7),
         hits_ma7 = runMean(hits, n = 7)) %>%
  filter(!is.na(hits_ma7)) %>%
  mutate(cases_hits_cor = cor(cases_new, hits_ma7),
         death_hits_cor = cor(death_new, hits_ma7)) %>%
  ungroup() %>%
  mutate(keyword_en = keyword_en %>% tools::toTitleCase())

gtrends_df$keyword_en[gtrends_df$keyword_en %in% "i Can't Smell"] <- "I Can't Smell"

gtrends_df <- gtrends_df[gtrends_df$keyword_en %in% 
                           c("Loss of Smell", "Cough", "Fever", "I Can't Smell",
                             "Index - All"),]

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




#readOGR(dsn = file.path(dropbox_file_path, "Data", "world_shapefile", "RawData"),
#        layer = "ne_50m_admin_0_countries")



