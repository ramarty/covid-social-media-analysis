# Merge Google Trends Data with Other Data Sources

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_complete.Rds"))

## WHO Cases
cases_df <- readRDS(file.path(dropbox_file_path, "Data", "who_covid", 
                              "FinalData", "covid.Rds"))

## Oxford Policy
ox_earliest_measure_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_earliest_measure.Rds"))

ox_nat_timeseries_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_national_timeseries.Rds"))

## WDI
wdi_df <- readRDS(file.path(dropbox_file_path, "Data", "wdi", "RawData", "wdi_data.Rds"))

## Google Mobility
gmobility_df <- read.csv(file.path(dropbox_file_path, "Data", "google_mobility", "RawData",
                                   "Global_Mobility_Report.csv"),
                         stringsAsFactors = F)

# Merge Cases ------------------------------------------------------------------
cases_df <- cases_df %>%
  dplyr::select(-c(country))

gtrends_df <- gtrends_df %>%
  left_join(cases_df, by = c("geo", "date"))

# No covid data for Jan 1 and 2
#gtrends_df$cases_new[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0
#gtrends_df$death_new[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0
#gtrends_df$cases[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0
#gtrends_df$death[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0

# Merge WDI --------------------------------------------------------------------
gtrends_df <- gtrends_df %>%
  left_join(wdi_df, by = "geo")

# Merge Oxford Policy Response Data --------------------------------------------
gtrends_df <- gtrends_df %>%
  left_join(ox_nat_timeseries_df, by = c("geo", "date"))

# Days Since Oxford Policies ---------------------------------------------------
gtrends_df <- gtrends_df %>%
  left_join(ox_earliest_measure_df, by = "geo")

gtrends_df <- gtrends_df %>%
  mutate(days_since_c1_school_closing = date - c1_school_closing_first_date,
         days_since_c2_workplace_closing = date - c2_workplace_closing_first_date,
         days_since_c3_cancel_public_events = date - c3_cancel_public_events_first_date,
         days_since_c4_restrictions_on_gatherings = date - c4_restrictions_on_gatherings_first_date,
         days_since_c5_close_public_transport = date - c5_close_public_transport_first_date,
         days_since_c6_stay_at_home_requirements = date - c6_stay_at_home_requirements_first_date,
         days_since_c7_restrictions_on_internal_movement = date - c7_restrictions_on_internal_movement_first_date,
         days_since_c8_international_travel_controls = date - c8_international_travel_controls_first_date,
         days_since_c_policy = date - c_policy_first_date) %>%
  dplyr::select(-c(c1_school_closing_first_date,
                   c2_workplace_closing_first_date,
                   c3_cancel_public_events_first_date,
                   c4_restrictions_on_gatherings_first_date,
                   c5_close_public_transport_first_date,
                   c6_stay_at_home_requirements_first_date,
                   c7_restrictions_on_internal_movement_first_date,
                   c7_restrictions_on_internal_movement_first_date)) %>%
  
  # Don't analyzie these (only c_policy), so can remove for now
  dplyr::select(-c(days_since_c1_school_closing,
                   days_since_c2_workplace_closing,
                   days_since_c3_cancel_public_events,
                   days_since_c4_restrictions_on_gatherings,
                   days_since_c5_close_public_transport,
                   days_since_c6_stay_at_home_requirements,
                   days_since_c7_restrictions_on_internal_movement,
                   days_since_c8_international_travel_controls))

# Google Mobility --------------------------------------------------------------

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

# Add continent/regions --------------------------------------------------------
gtrends_df$un_regionsub_name <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "un.regionsub.name")
gtrends_df$wb_region <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "region")
gtrends_df$country <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "country.name")

# Export -----------------------------------------------------------------------
saveRDS(gtrends_df,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                  "gtrends_full_timeseries", "gtrends_otherdata_complete.Rds"))






