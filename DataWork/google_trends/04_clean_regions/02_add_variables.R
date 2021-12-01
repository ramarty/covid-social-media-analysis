# Add variables to regional google trends data. 

# OUTLINE
# (1) Load data
# (2) Clean vaccine data and merge with Google Trends data
# (3) Clean elections data and merge with Google Trends data
# (4) Variable construction
# (5) Export

# Load Data --------------------------------------------------------------------
#### Google Data
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_regional",
                                "gtrends_regional.Rds"))

#### Vaccine Data
#doses_adm_df <- read_csv(file.path(vaccine_dir, "RawData", "us-daily-covid-vaccine-doses-administered.csv"))
#doses_dis_df <- read_csv(file.path(vaccine_dir, "RawData", "us-total-covid-vaccine-doses-distributed.csv"))
#share_used_df <- read_csv(file.path(vaccine_dir, "RawData", "us-share-covid-19-vaccine-doses-used.csv"))
#share_vac_df <- read_csv(file.path(vaccine_dir, "RawData", "us-covid-19-share-people-vaccinated.csv"))
usa_vax_df <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv")

#### US Elections
elec_df <- read_csv(file.path(dropbox_file_path, "Data", "usa_elections", "1976-2020-president.csv"))

# Prep Vaccine Data ------------------------------------------------------------
usa_vax_df <- usa_vax_df %>%
  dplyr::mutate(location = case_when(
    location == "New York State" ~ "New York",
    TRUE ~ location
  )) %>%
  dplyr::select(date, location, people_fully_vaccinated_per_hundred) %>%
  arrange(location, date) %>%
  group_by(location) %>%
  fill(people_fully_vaccinated_per_hundred) %>%
  dplyr::filter(date %in% c(ymd("2021-05-31"),
                            ymd("2021-09-30"))) 

usa_vax_df <- usa_vax_df[usa_vax_df$location %in% gtrends_df$location,]

usa_vax_df <- usa_vax_df %>%
  dplyr::mutate(date = date %>% 
                  as.character() %>%
                  str_replace_all("-", "_")) %>%
  pivot_wider(id_cols = location,
              names_from = date,
              values_from = people_fully_vaccinated_per_hundred) %>%
  rename_at(vars(-location), ~ paste0('vax_per100_', .))

# Prep Election Data -----------------------------------------------------------
#### Clean
elec_clean_df <- elec_df %>%
  
  # Subset to 2020 and to main 2 candidates
  dplyr::filter(year == 2020,
                candidate %in% c("TRUMP, DONALD J.",
                                 "BIDEN, JOSEPH R. JR")) %>%
  
  # Cleanup variables
  dplyr::mutate(candidate = candidate %>%
                  str_replace_all(",.*", "") %>%
                  tolower() %>%
                  paste0("_prop_vote"),
                prop_vote = candidatevotes/totalvotes,
                state = state %>% 
                  tolower() %>% 
                  tools::toTitleCase()) %>%
  
  # Pivot wider (so at state level)
  dplyr::select(state, state_po, candidate, prop_vote) %>%
  pivot_wider(names_from = candidate, values_from = prop_vote) %>%
  
  # Winner variable
  dplyr::mutate(party_winner = ifelse(biden_prop_vote > trump_prop_vote, "Democrat", "Republican")) %>%
  
  # Cleanup
  dplyr::select(state, biden_prop_vote, trump_prop_vote, party_winner) %>%
  dplyr::rename(location = state)

# Merge ------------------------------------------------------------------------
gtrends_df <- gtrends_df %>%
  left_join(usa_vax_df, by = "location") %>%
  left_join(elec_clean_df, by = "location")

# Variable Construction --------------------------------------------------------
gtrends_df <- gtrends_df %>%
  dplyr::mutate(missing_search = if_else(is.na(hits), "yes", "no"))

# Export -----------------------------------------------------------------------
saveRDS(gtrends_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_regional",
                              "gtrends_regional_clean.Rds"))





#### Share of people vaccinated as of June 18
#share_vac_jun18_df <- share_vac_df %>% 
#  dplyr::filter(Day == "2021-06-18")

#### Merge datasets together and aggregate
## Merge
#vaccine_df <- 
#  doses_adm_df %>% 
#  left_join(doses_dis_df, by = c("Entity", "Day", "Code")) %>% 
#  left_join(share_used_df, by = c("Entity", "Day", "Code")) %>% 
#  left_join(share_vac_df, by = c("Entity", "Day", "Code"))

## Aggregate vaccine data
# Vaccine data is daily; search data is in 2 periods: 7 months (Dec,20-May 21) 
# and 3 months (March-May 21)

#To merge, we can: 
#  a) Create a variable of average/median doses for those 2 periods, then calculate correlations

#vac_mean_df <- 
#  vaccine_df %>% 
#  group_by(Entity) %>% 
#  summarise(
#    sum_vaccinations = sum(daily_vaccinations, na.rm = TRUE),
#    total_distributions = max(total_distributed, na.rm = TRUE), 
#    across(daily_vaccinations:share_doses_used, ~mean(., na.rm = TRUE))
#  )

#### Merge with gTrends
#gtrends_df <- 
#  gtrends_df %>% 
#  left_join(vac_mean_df, by = c("location" = "Entity")) %>% 
#  left_join(share_vac_jun18_df, by = c("location" = "Entity"))

# Merge Election Data ----------------------------------------------------------
#### Clean
# elec_clean_df <- elec_df %>%
#   
#   # Subset to 2020 and to main 2 candidates
#   dplyr::filter(year == 2020,
#                 candidate %in% c("TRUMP, DONALD J.",
#                                  "BIDEN, JOSEPH R. JR")) %>%
#   
#   # Cleanup variables
#   dplyr::mutate(candidate = candidate %>%
#                   str_replace_all(",.*", "") %>%
#                   tolower() %>%
#                   paste0("_prop_vote"),
#                 prop_vote = candidatevotes/totalvotes,
#                 state = state %>% 
#                   tolower() %>% 
#                   tools::toTitleCase()) %>%
#   
#   # Pivot wider (so at state level)
#   dplyr::select(state, state_po, candidate, prop_vote) %>%
#   pivot_wider(names_from = candidate, values_from = prop_vote) %>%
#   
#   # Winner variable
#   dplyr::mutate(party_winner = ifelse(biden_prop_vote > trump_prop_vote, "Democrat", "Republican"))
# 
# gtrends_df <- 
#   gtrends_df %>% 
#   left_join(elec_clean_df, by = c("location" = "state"))



