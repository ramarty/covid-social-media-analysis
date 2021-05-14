# What variables explain correlation?

# Load Data --------------------------------------------------------------------
lockdowns_df <- read.csv(file.path(dropbox_file_path, "Data", "lockdowns", "RawData",
                               "countryLockdowndates.csv"))

# Clean variables and add geo --------------------------------------------------
lockdowns_df <- lockdowns_df %>%
  mutate(Date = Date %>% dmy,
         Country.Region = Country.Region %>% str_replace_all("\\*", ""),
         geo = Country.Region %>% countryname(destination = 'iso2c')) %>%
  filter(!is.na(Date)) %>%
  dplyr::rename(date = Date,
                lockdown_type = Type)
  
lockdowns_df$geo[lockdowns_df$Country.Region %in% "Kosovo"] <- "KV"

# To country level -------------------------------------------------------------
lockdowns_df <- lockdowns_df %>%
  dplyr::filter(lockdown_type %in% "Full") %>%
  group_by(geo) %>%
  dplyr::summarise(lockdown_date_min = min(date),
                   lockdown_date_max = max(date))

# Export -----------------------------------------------------------------------
saveRDS(lockdowns_df,
        file.path(dropbox_file_path, "Data", "lockdowns", "FinalData",
                  "lockdowns_full.Rds"))












