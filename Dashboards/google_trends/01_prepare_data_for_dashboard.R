# Prepare Data for Dashboard

DASHBOARD_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData", "data")

# World Shapefile --------------------------------------------------------------
world_sp <- readOGR(file.path(dropbox_file_path, "Data", "world_shapefile", 
                                 "TM_WORLD_BORDERS-0.3.shp"))

world_sp$name <- world_sp$name %>% as.character()
world_sp$continent <- NA
world_sp$continent[world_sp$region %in% 2] <- "Africa"
world_sp$continent[world_sp$region %in% 142] <- "Asia"
world_sp$continent[world_sp$region %in% 150] <- "Europe"
world_sp$continent[world_sp$region %in% 9] <- "Oceania"
world_sp$continent[world_sp$region %in% 19] <- "Americas"

world_sp@data <- world_sp@data %>%
  dplyr::select(iso2, name, continent) %>%
  dplyr::rename(geo = iso2)

world_df <- world_sp@data

saveRDS(world_sp, file.path(DASHBOARD_PATH, "world.Rds"))

# Correlations -----------------------------------------------------------------
cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                            "global_with_refstate",
                            paste0("gl_gtrends_ref","US","_adj_cases_correlations.Rds")))

cor_df$keyword_en <- cor_df$keyword_en %>% tools::toTitleCase()
cor_df <- merge(cor_df, world_df, by = "geo")

cor_df$keyword_en <- cor_df$keyword_en %>% tools::toTitleCase()

saveRDS(cor_df, file.path(DASHBOARD_PATH, "correlations.Rds"))

# gTrends ----------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_cor.Rds")))

gtrends_df$keyword_en <- gtrends_df$keyword_en %>% tools::toTitleCase()
gtrends_df <- gtrends_df[gtrends_df$date >= "2020-02-01",]
gtrends_df <- merge(gtrends_df, world_df, by = "geo")

saveRDS(gtrends_df, file.path(DASHBOARD_PATH, "gtrends.Rds"))









