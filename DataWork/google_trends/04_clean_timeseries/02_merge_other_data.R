# Merge Google Trends Data with Other Data Sources

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_complete.Rds"))

cases_df <- read.csv(file.path(dropbox_file_path, "Data", "global_admin_data", 
                               "RawData", "WHO-COVID-19-global-data.csv"),
                     stringsAsFactors = F)

wdi_df <- readRDS(file.path(dropbox_file_path, "Data", "wdi", "RawData", "wdi_data.Rds"))

ox_earliest_measure_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_earliest_measure.Rds"))

ox_nat_timeseries_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_national_timeseries.Rds"))

gmobility_df <- read.csv(file.path(dropbox_file_path, "Data", "google_mobility", "RawData",
                                   "Global_Mobility_Report.csv"),
                         stringsAsFactors = F)

# Merge Cases ------------------------------------------------------------------
gtrends_df <- gtrends_df %>%
  filter(!is.na(geo))

cases_df <- cases_df %>%
  dplyr::rename(Date_reported = Date_reported) %>%
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

cases_df <- cases_df %>%
  arrange(date) %>%
  group_by(geo) %>%
  mutate(cases_new_ma7 = runMean(cases_new, n = 7),
         death_new_ma7 = runMean(death_new, n = 7))

gtrends_df <- merge(gtrends_df, cases_df, by = c("geo", "date"), all.x=T, all.y=F)

# No covid data for Jan 1 and 2
gtrends_df$cases_new[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0
gtrends_df$death_new[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0

# Merge WDI --------------------------------------------------------------------
gtrends_df <- merge(gtrends_df, wdi_df, by = "geo", all.x=T, all.y=F)

# Days Since First Lockdown ----------------------------------------------------
if(F){
  gtrends_df <- merge(gtrends_df, lockdowns_df, by = "geo", all.x = T, all.y = F)
  
  gtrends_df <- gtrends_df %>%
    group_by(geo) %>%
    mutate(days_since_lockdown_max = date - lockdown_date_max,
           days_since_lockdown_min = date - lockdown_date_min)
}

# Days Since Oxford Policies ---------------------------------------------------
gtrends_df <- merge(gtrends_df, ox_earliest_measure_df, by = "geo", all.x = T, all.y = F)

gtrends_df <- gtrends_df %>%
  mutate(days_since_c1_school_closing = date - c1_school_closing,
         days_since_c2_workplace_closing = date - c2_workplace_closing,
         days_since_c3_cancel_public_events = date - c3_cancel_public_events,
         days_since_c4_restrictions_on_gatherings = date - c4_restrictions_on_gatherings,
         days_since_c5_close_public_transport = date - c5_close_public_transport,
         days_since_c6_stay_at_home_requirements = date - c6_stay_at_home_requirements,
         days_since_c7_restrictions_on_internal_movement = date - c7_restrictions_on_internal_movement,
         days_since_c8_international_travel_controls = date - c8_international_travel_controls,
         days_since_c_policy = date - c_policy)

# Google Mobility --------------------------------------------------------------
if(T){
  gmobility_df <- gmobility_df %>%
    dplyr::filter(sub_region_1 %in% "",
                  sub_region_2 %in% "",
                  metro_area %in% "",
                  !is.na(country_region_code)) %>%
    dplyr::select(country_region_code, date,
                  retail_and_recreation_percent_change_from_baseline,
                  grocery_and_pharmacy_percent_change_from_baseline,
                  parks_percent_change_from_baseline,
                  transit_stations_percent_change_from_baseline,
                  workplaces_percent_change_from_baseline,
                  residential_percent_change_from_baseline) %>% 
    dplyr::rename(geo = country_region_code) %>%
    rename_at(vars(-geo, -date), ~ paste0("gmobility_", .)) %>%
    dplyr::mutate(date = date %>% as.Date)
  
  gtrends_df <- gtrends_df %>%
    left_join(gmobility_df,
              by = c("geo", "date"))
}

# Merge Oxford Policy Response Data --------------------------------------------
gtrends_df <- merge(gtrends_df, ox_nat_timeseries_df, by = c("geo", "date"), all.x=T, all.y=F)

# Add continent/regions --------------------------------------------------------
gtrends_df$un_regionsub_name <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "un.regionsub.name")
gtrends_df$wb_region <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "region")

# Export -----------------------------------------------------------------------
gtrends_df <- gtrends_df %>% distinct() 

saveRDS(gtrends_df,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                  "gtrends_full_timeseries", "gtrends_otherdata_complete.Rds"))





