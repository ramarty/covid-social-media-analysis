# What variables explain correlation?

library(usdata)
library(urbnmapr)
library(usmap)
library(geofacet)

# Prep Google -------------------------------------------------------------------
google_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "gtrends_regional.Rds"))
# does covid vaccine change dna
# covid vaccine mark of beast
# can covid vaccine cause infertility
google_df_dna <- google_df %>%
  filter(geo %in% "US",
         keyword %in% "does covid vaccine change dna") %>%
  mutate(month = time_span %>% substring(1,10) %>% ymd()) 

google_df_beast <- google_df %>%
  filter(geo %in% "US",
         keyword %in% "covid vaccine mark of the beast") %>%
  mutate(month = time_span %>% substring(1,10) %>% ymd()) 

# google_df$keyword_en %>% unique() %>% str_subset("inferti")
google_df_inf <- google_df %>%
  filter(geo %in% "US",
         keyword %in% "covid vaccine cause infertility") %>%
  mutate(month = time_span %>% substring(1,10) %>% ymd()) 

states <- get_urbn_map("states", sf = T) 
states <- states %>%
  dplyr::rename(location = state_name)

for(month in c("2020-09-01",
               "2020-10-01",
               "2020-11-01",
               "2020-12-01",
               "2021-01-01",
               "2021-02-01")){
  
  month_date <- as.Date(month)
  
  month_name <- case_when(month %in% "2020-09-01" ~ "September, 2020",
                          month %in% "2020-10-01" ~ "October, 2020",
                          month %in% "2020-11-01" ~ "November, 2020",
                          month %in% "2020-12-01" ~ "December, 2020",
                          month %in% "2021-01-01" ~ "January, 2021",
                          month %in% "2021-02-01" ~ "February, 2021")
  
  google_df_dna_i <- google_df_dna %>%
    filter(month %in% !!month_date) %>%
    dplyr::select(hits, location) %>%
    dplyr::rename(hits_dna = hits)
  
  google_df_beast_i <- google_df_beast %>%
    filter(month %in% !!month_date) %>%
    dplyr::select(hits, location) %>%
    dplyr::rename(hits_beast = hits)
  
  google_df_inf_i <- google_df_inf %>%
    filter(month %in% !!month_date) %>%
    dplyr::select(hits, location) %>%
    dplyr::rename(hits_inf = hits)
  
  states_i <- merge(states,   google_df_dna_i,   by = "location", all.x=T, all.y=F)
  states_i <- merge(states_i, google_df_beast_i, by = "location", all.x=T, all.y=F)
  states_i <- merge(states_i, google_df_inf_i,   by = "location", all.x=T, all.y=F)
  
  p_dna <- states_i %>%
    ggplot() +
    geom_sf(aes(fill = hits_dna)) +
    scale_fill_viridis(na.value = "gray50") +
    theme_void() +
    labs(fill = "Search Interest",
         title = "Does COVID Vaccine Change DNA") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5))
  
  p_beast <- states_i %>%
    ggplot() +
    geom_sf(aes(fill = hits_beast)) +
    scale_fill_viridis(na.value = "gray50") +
    theme_void() +
    labs(fill = "Search Interest",
         title = "COVID Vaccine Mark of the Beast") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5))
  
  p_inf <- states_i %>%
    ggplot() +
    geom_sf(aes(fill = hits_inf)) +
    scale_fill_viridis(na.value = "gray50") +
    theme_void() +
    labs(fill = "Search Interest",
         title = "COVID Vaccine Cause Infertility") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5))
  
  p <- ggarrange(p_dna, p_beast, p_inf,
                 nrow = 1,
                 common.legend = T,
                 legend = "bottom")
  p <- annotate_figure(p,
      top = text_grob(month_name, color = "black", face = "bold", size = 14)
    )
  
  ggsave(p, filename = file.path("~/Desktop",
                                 "vxmap",
                                 paste0(as.character(month), ".png")),
         height = 4, width = 12)
}

