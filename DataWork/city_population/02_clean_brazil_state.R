city_pop_data <- readRDS(file.path(dropbox_file_path, "Data/city_population/FinalData/brazil_cities_pop_wikipedia.Rds"))

state_pop_data <- 
  city_pop_data %>%
  group_by(State) %>% 
  summarize(
    estimate_2018_state = sum(estimate_2018, na.rm = TRUE), 
    census_2010_state = sum(census_2010, na.rm = TRUE), 
    perc_change = estimate_2018_state*100/census_2010_state
  )

state_pop_data <- 
  state_pop_data %>% 
  mutate(State = str_trim(State))

write.csv(state_pop_data, file.path(dropbox_file_path, "Data/city_population/FinalData/brazil_state_pop.csv"), row.names = F)
