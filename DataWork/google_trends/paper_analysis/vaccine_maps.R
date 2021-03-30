# What variables explain correlation?

library(usdata)
library(urbnmapr)
library(usmap)
library(geofacet)

# Prep Cases -------------------------------------------------------------------
data(statepop)
statepop <- statepop %>%
  dplyr::select(full, pop_2015) %>%
  dplyr::rename(location = full)

cases_df <- read.csv(file.path(dropbox_file_path, "Data", "usa_case_data", "RawData",
                               "covid_confirmed_usafacts.csv"))

cases_df <- cases_df %>%
  dplyr::select(-c(countyFIPS)) %>%
  pivot_longer(cols = -c(County.Name,
                         State,
                         StateFIPS)) %>%
  mutate(name = name %>% str_replace_all("X", "") %>% ymd(),
         month = name %>% round_date(unit = "month")) %>%
  group_by(State, month) %>%
  dplyr::summarise(value = max(value)) %>%
  mutate(location = State %>% abbr2state) %>%
  ungroup() %>%
  dplyr::select(location, value, month) %>%
  dplyr::rename(cases = value) %>%
  arrange(month) %>%
  ungroup() 

cases_df$cases[cases_df$location %in% "West Virginia" & cases_df$month %in% as.Date("2021-01-01")] <- 10069

cases_df <- cases_df %>%
  dplyr::group_by(location) %>%
  dplyr::mutate(cases_new = c(NA, diff(cases))) %>%
  dplyr::mutate(cases_new = cases_new %>% replace_na(0)) %>%
  left_join(statepop, by = "location") %>%
  mutate(cases_new_pop = cases_new / pop_2015)

# Prep Google -------------------------------------------------------------------
google_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "gtrends_regional.Rds"))
# does covid vaccine change dna
# covid vaccine mark of beast
# can covid vaccine cause infertility
google_df_dna <- google_df %>%
  filter(geo %in% "US",
         keyword %in% "covid vaccine cause infertility") %>%
  mutate(month = time_span %>% substring(1,10) %>% ymd()) %>%
  left_join(cases_df, by = c("location", "month")) 

google_df_beast <- google_df %>%
  filter(geo %in% "US",
         keyword %in% "covid vaccine mark of beast") %>%
  mutate(month = time_span %>% substring(1,10) %>% ymd()) %>%
  left_join(cases_df, by = c("location", "month")) 

google_df_inf <- google_df %>%
  filter(geo %in% "US",
         keyword %in% "can covid vaccine cause infertility") %>%
  mutate(month = time_span %>% substring(1,10) %>% ymd()) %>%
  left_join(cases_df, by = c("location", "month")) 

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
    labs(fill = "Search Interest") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5))
  
  p_beast <- states_i %>%
    ggplot() +
    geom_sf(aes(fill = hits_beast)) +
    scale_fill_viridis(na.value = "gray50") +
    theme_void() +
    labs(fill = "Search Interest") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5))
  
  p_inf <- states_i %>%
    ggplot() +
    geom_sf(aes(fill = hits_inf)) +
    scale_fill_viridis(na.value = "gray50") +
    theme_void() +
    labs(fill = "Search Interest") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5))
  
  p <- ggarrange(p_dna, p_beast, p_inf,
                 nrow = 1,
                 common.legend = T)
  p <- annotate_figure(p,
      top = text_grob(month, color = "black", face = "bold", size = 14)
    )
  
  ggsave(p, filename = file.path("~/Desktop",
                                 "vxmap",
                                 paste0(as.character(month), ".png")),
         height = 4, width = 12)
}

