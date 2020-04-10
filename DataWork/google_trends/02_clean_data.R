# Clean Google Trends Data

# Load Data --------------------------------------------------------------------
trends_df <- readRDS(file.path(dropbox_file_path, "Data/google_trends/RawData/brazil_extract.Rds"))
geo_data <- readRDS(file.path(dropbox_file_path, "Data/GADM/RawData/gadm36_BRA_1_sp.rds"))

# Clean Variables --------------------------------------------------------------

#### Hits variable
# If variable is "<1", just consider as 0.5 and convert into numeric
trends_df$hits[trends_df$hits %in% "<1"] <- "0.5"
trends_df$hits <- trends_df$hits %>% as.numeric()

#### Date
# Convert into date format
trends_df$date <- trends_df$date %>% as.Date()

# Merge with Shapefile ---------------------------------------------------------

sf_geo_data <- st_as_sf(geo_data)

sf_geo_data <- 
  sf_geo_data %>% 
  mutate(HASC_1 = str_replace(HASC_1, pattern = "[.]", replacement = "-"))

data_trends_geo <- 
  trends_df %>% 
  left_join(sf_geo_data, by = c("geo" = "HASC_1"))

# Export -----------------------------------------------------------------------
saveRDS(trends_df, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_extract_clean.Rds"))
write.csv(trends_df, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_extract_clean.csv"), row.names = F)

saveRDS(data_trends_geo, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_geo_clean.Rds"))
write.csv(data_trends_geo, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_geo_clean.csv"), row.names = F)
