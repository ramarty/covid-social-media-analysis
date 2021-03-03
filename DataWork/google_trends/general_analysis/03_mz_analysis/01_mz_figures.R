# Scrape Data from Google Trends

ISO_SCRAPE <- "MZ"

trends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "RawData", paste0(ISO_SCRAPE, "_timeseries_raw.Rds")))


trends_df$hits

trends_df$hits
