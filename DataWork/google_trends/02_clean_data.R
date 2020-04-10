# Clean Google Trends Data

# Load Data --------------------------------------------------------------------
trends_df <- readRDS(file.path(dropbox_file_path, "Data/google_trends/RawData/brazil_extract.Rds"))
admin_data <- read.dta13(file.path(dropbox_file_path, "Data/brazil_admin_data/admindata.dta"))
geo_data <- readRDS(file.path(dropbox_file_path, "Data/GADM/RawData/gadm36_BRA_1_sp.rds"))

# Clean Variables --------------------------------------------------------------

#### Hits variable
# If variable is "<1", just consider as 0.5 and convert into numeric
trends_df$hits[trends_df$hits %in% "<1"] <- "0.5"
trends_df$hits <- trends_df$hits %>% as.numeric()

#### Date
# Convert into date format
trends_df$date <- trends_df$date %>% as.Date()

# Merge google trends data with admin cases and deaths---------------------------------------------------------

admin_data <- 
  admin_data %>% 
  mutate(
    state_en = stringi::stri_trans_general(state, "Latin-ASCII") %>% str_to_upper(), 
    date = as.Date(date)
  ) 

trends_admin_df <- 
  admin_data %>% 
  full_join(trends_df, by = c("state_en" = "name", "date" = "date"))


# Merge the data above with Shapefile ---------------------------------------------------------

sf_geo_data <- st_as_sf(geo_data)

sf_geo_data <- 
  sf_geo_data %>% 
  mutate(HASC_1 = str_replace(HASC_1, pattern = "[.]", replacement = "-"))

trends_df_geo <- 
  trends_admin_df %>% 
  left_join(sf_geo_data, by = c("geo" = "HASC_1"))

# Export -----------------------------------------------------------------------
saveRDS(trends_df, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_extract_clean.Rds"))
write.csv(trends_df, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_extract_clean.csv"), row.names = F)

saveRDS(trends_admin_df, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_admin_trends_clean.Rds"))
write.csv(trends_admin_df, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_admin_trends_clean.csv"), row.names = F)

saveRDS(trends_df_geo, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_extract_clean.Rds"))
write.csv(trends_df_geo, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_extract_clean.csv"), row.names = F)
