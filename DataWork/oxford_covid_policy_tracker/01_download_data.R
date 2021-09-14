# Download latest data from Oxford Policy Response Tracker

## Read data from OxCGRT Githib
ox_df <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")

## Export data
saveRDS(ox_df, file.path(oxpol_dir, "RawData", "OxCGRT_latest.Rds"))
