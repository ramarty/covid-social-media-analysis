# Clean Google Trends Data

# Load Data --------------------------------------------------------------------
trends_df <- readRDS(file.path(dropbox_file_path, "Data/google_trends/RawData/brazil_extract.Rds"))

# Clean Variables --------------------------------------------------------------

#### Hits variable
# If variable is "<1", just consider as 0.5 and convert into numeric
trends_df$hits[trends_df$hits %in% "<1"] <- "0.5"
trends_df$hits <- trends_df$hits %>% as.numeric()

#### Date
# Convert into date format
trends_df$date <- trends_df$date %>% as.Date()

# Merge with Shapefile ---------------------------------------------------------

# TODO

# Export -----------------------------------------------------------------------
saveRDS(trends_df, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_extract_clean.Rds"))
write.csv(trends_df, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_extract_clean.csv"), row.names = F)
