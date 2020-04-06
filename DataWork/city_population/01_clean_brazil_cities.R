# Clean Brazil Cities

brazil_cities <- read.csv(file.path(dropbox_file_path, "Data", "city_population", "RawData", "brazil_cities_pop_wikipedia.csv"),
                          stringsAsFactors=F)

brazil_cities <- brazil_cities %>%
  mutate(census_2010 = census_2010 %>% str_replace_all(",", "") %>% as.numeric(),
         estimate_2018 = estimate_2018 %>% str_replace_all(",", "") %>% as.numeric())

saveRDS(brazil_cities, file.path(dropbox_file_path, "Data", "city_population", "FinalData", "brazil_cities_pop_wikipedia.Rds"))
