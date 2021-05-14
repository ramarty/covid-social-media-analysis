# Append and Clean Google Trends Data

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "gtrends_otherdata_varclean.Rds"))

df$keyword_en %>% unique %>% length()
df$geo %>% unique %>% length()
