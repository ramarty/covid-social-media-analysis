# Append and Clean Google Trends Data

# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

# Load Data --------------------------------------------------------------------
region_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "gtrends_regional.Rds"))

region_df <- region_df %>%
  filter(!is.na(hits))

us_df <- region_df %>%
  filter(geo %in% "US")

states <- get_urbn_map("states", sf = F)

##
states_data <- merge(states, us_df, by.x = "state_name", by.y = "location", all=T)

for(keyword_i in unique(states_data$keyword_en)){
  print(keyword_i)
  
  states_data %>%
    filter(keyword_en %in% keyword_i) %>%
    ggplot() +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     fill = hits)) +
    scale_fill_viridis(na.value = "gray50") +
    facet_wrap(~time_span) +
    theme_void() +
    coord_quickmap() +
    labs(title = keyword_i) +
    ggsave(filename = file.path(paper_figures, paste0("us_regional_",keyword_i,".png")),
           height = 4, width = 8)
}


