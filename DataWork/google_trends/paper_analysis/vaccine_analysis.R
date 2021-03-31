# What variables explain correlation?

# Load Data --------------------------------------------------------------------
google_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

google_df$geo_name <- google_df$geo %>% countrycode(origin = "iso2c", destination = "iso.name.en")

google_df <- google_df %>%
  mutate(geo_name = case_when(geo_name %in% "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                              geo_name %in% "Bahamas (the)" ~ "Bahamas",
                              geo_name %in% "Cayman Islands (the)" ~ "Cayman Islands",
                              geo_name %in% "Bolivia (Plurinational State of)" ~ "Bolivia",
                              geo_name %in% "Russian Federation (the)" ~ "Russia",
                              geo_name %in% "Iran (Islamic Republic of)" ~ "Iran",
                              geo_name %in% "Dominican Republic (the)" ~ "Dominican Republic",
                              geo_name %in% "United States of America (the)" ~ "USA",
                              TRUE ~ geo_name)) 

# Figures ----------------------------------------------------------------------
google_df %>%
  filter(keyword_en %in% "covid vaccine cause infertility") %>%
  ggplot() +
  geom_line(aes(x = date,
                y = hits_ma7),
            color = "forestgreen") +
  labs(x = NULL,
       y = "Search Interest") +
  facet_wrap(~geo_name,
             scales = "free_y") + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold",
                                  size = 8)) +
  ggsave(filename = file.path("~/Desktop","vx",
                              "infert.png"),
         height = 1.75, width = 4)


google_df %>%
  filter(keyword_en %in% "does covid vaccine change dna") %>%
  ggplot() +
  geom_line(aes(x = date,
                y = hits_ma7),
            color = "forestgreen") +
  labs(x = NULL,
       y = "Search Interest") +
  facet_wrap(~geo_name,
             scales = "free_y") + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold",
                                  size = 8)) +
  ggsave(filename = file.path("~/Desktop","vx",
                              "changedna.png"),
         height = 3.5, width = 4)


google_df %>%
  filter(keyword_en %in% "covid vaccine side effects") %>%
  ggplot() +
  geom_line(aes(x = date,
                y = hits_ma7),
            color = "forestgreen") +
  labs(x = NULL,
       y = "Search Interest") +
  facet_wrap(~geo_name,
             scales = "free_y") + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold",
                                  size = 8)) +
  ggsave(filename = file.path("~/Desktop","vx",
                              "sideffects.png"),
         height = 8, width = 11.3)
