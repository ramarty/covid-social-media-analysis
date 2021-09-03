# Add variables to regional google trends data. 

# Add:
# (1) Vaccine data
# (2) US election data

# Load Data --------------------------------------------------------------------
## Google Data
region_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_regional",
                               "gtrends_regional.Rds"))

## Vaccine Data
doses_adm_df <- file.path(vaccine_dir, "RawData", "us-daily-covid-vaccine-doses-administered.csv")
doses_dis_df <- file.path(vaccine_dir, "RawData", "us-total-covid-vaccine-doses-distributed.csv")
share_used_df <- file.path(vaccine_dir, "RawData", "us-share-covid-19-vaccine-doses-used.csv")
share_vac_df <- file.path(vaccine_dir, "RawData", "us-covid-19-share-people-vaccinated.csv")

## US Elections
elec_df <- read_csv(file.path(dropbox_file_path, "Data", "usa_elections", "1976-2020-president.csv"))


