# Applies equation to make hits comparable across time/states using a comparison
# state

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends.Rds"))

cases_df <- read.csv(file.path(dropbox_file_path, "Data", "global_admin_data", 
                               "RawData", "WHO-COVID-19-global-data.csv"),
                     stringsAsFactors = F)

wdi_df <- readRDS(file.path(dropbox_file_path, "Data", "wdi", "RawData", "wdi_data.Rds"))

# Merge Cases ------------------------------------------------------------------
gtrends_df <- gtrends_df %>%
  filter(!is.na(geo))

cases_df <- cases_df %>%
  dplyr::select(Date_reported, Country_code, New_cases, Cumulative_cases, 
                New_deaths, Cumulative_deaths, Country) %>%
  dplyr::rename(cases = Cumulative_cases,
                cases_new = New_cases,
                death = Cumulative_deaths,
                death_new = New_deaths,
                geo = Country_code,
                date = Date_reported) 

cases_df$cases_new[cases_df$cases_new < 0] <- 0
cases_df$death_new[cases_df$death_new < 0] <- 0

gtrends_cases_df <- merge(gtrends_df, cases_df, by = c("geo", "date"), all.x=F, all.y=F)

# Merge WDI --------------------------------------------------------------------
gtrends_cases_df <- merge(gtrends_cases_df, wdi_df, by = "geo", all.x=T, all.y=F)

# Export -----------------------------------------------------------------------
gtrends_cases_df <- gtrends_cases_df %>% distinct() # ?? why need

saveRDS(gtrends_cases_df,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                  "gtrends_full_timeseries", "gtrends_otherdata.Rds"))






