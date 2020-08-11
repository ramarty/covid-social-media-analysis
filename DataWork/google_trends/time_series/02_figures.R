# Initial Figures of Trends

# Load Data --------------------------------------------------------------------
trends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", 
                               "brazil_with_refstate_analysis_long.Rds"))

trends_df$ma7_hits_perda_de_olfato_bin <- trends_df$ma7_hits_perda_de_olfato_bin * 7
trends_df$ma7_hits_perdi_o_olfato_bin  <- trends_df$ma7_hits_perdi_o_olfato_bin  * 7

# Scaled -----------------------------------------------------------------------
trends_df_lng <- trends_df %>%
  group_by(state) %>%
  mutate(ma7_hits_perda_de_olfato_scale = ma7_hits_perda_de_olfato / max(ma7_hits_perda_de_olfato, na.rm=T),
         ma7_hits_perda_de_olfato_bin_scale = ma7_hits_perda_de_olfato_bin / max(ma7_hits_perda_de_olfato_bin, na.rm=T),
         
         ma7_hits_perdi_o_olfato_scale = ma7_hits_perdi_o_olfato / max(ma7_hits_perdi_o_olfato, na.rm=T),
         ma7_hits_perdi_o_olfato_bin_scale = ma7_hits_perdi_o_olfato_bin / max(ma7_hits_perdi_o_olfato_bin, na.rm=T),
         
         cases_new_scale = cases_new / max(cases_new),
         deaths_new_scale = deaths_new / max(deaths_new)) %>%
  ungroup() %>%
  dplyr::select(state, date, 
                cases_new, 
                deaths_new, 
                ma7_hits_perda_de_olfato_bin,
                ma7_hits_perda_de_olfato,
                ma7_hits_perdi_o_olfato_bin,
                ma7_hits_perdi_o_olfato,
                
                cases_new_scale, 
                deaths_new_scale, 
                ma7_hits_perda_de_olfato_bin_scale,
                ma7_hits_perda_de_olfato_scale,
                ma7_hits_perdi_o_olfato_bin_scale,
                ma7_hits_perdi_o_olfato_scale) %>%
  pivot_longer(-c(state, date))

for(covid_var in c("cases_new_scale", "deaths_new_scale")){
  for(google_var in c("ma7_hits_perda_de_olfato_bin_scale", "ma7_hits_perda_de_olfato_scale",
                      "ma7_hits_perdi_o_olfato_bin_scale", "ma7_hits_perdi_o_olfato_scale")){
    
    trends_df_lng %>%
      filter(name %in% c(covid_var, google_var)) %>%
      ggplot(aes(x=date, y = value, group = name, color = name)) +
      geom_line() +
      theme_minimal() +
      labs(color = "",
           title = "",
           subtitle= "For each state, cases/deaths and google hits are scaled between 0 and 1") +
      scale_color_manual(values = c("black", "green3"),
                         labels = c("New Cases", "Google Hits")) +
      facet_wrap(~state, 
                 scale = "free_y") +
      ggsave(file.path(google_figures_path, 
                       paste0(covid_var,"_",google_var, "_scaled01.png")),
             height = 9, width =12)
    
  }
}

# Hits -------------------------------------------------------------------------

trends_df_lng %>%
  filter(name == "ma7_hits_perda_de_olfato_bin") %>%
  ggplot() +
  geom_line(aes(x = date, y = value)) +
  labs(title = "N Times\nPerda de Olfato\nAppears Past Week") +
  theme_minimal() +
  facet_wrap(~state, ncol = 2) +
  ggsave(file.path(google_figures_path, "perda_de_olfato_hits_7days.png"),
         height = 12, width = 3)

trends_df_lng %>%
  filter(name == "ma7_hits_perdi_o_olfato_bin") %>%
  ggplot() +
  geom_line(aes(x = date, y = value)) +
  labs(title = "N Times\nPerdi o Olfato\nAppears Past Week") +
  theme_minimal() +
  facet_wrap(~state, ncol = 2) +
  ggsave(file.path(google_figures_path, "perdi_o_olfato_hits_7days.png"),
         height = 12, width = 3)

trends_df_lng %>%
  filter(name == "cases_new") %>%
  ggplot() +
  geom_line(aes(x = date, y = value)) +
  labs(title = " \n \nNew Cases") +
  theme_minimal() +
  facet_wrap(~state, ncol = 2, scales = "free_y") +
  ggsave(file.path(google_figures_path, "cases_new.png"),
         height = 12, width = 3)

trends_df_lng %>%
  filter(name == "deaths_new") %>%
  ggplot() +
  geom_line(aes(x = date, y = value)) +
  labs(title = " \n \nNew Deaths") +
  theme_minimal() +
  facet_wrap(~state, ncol = 2, scales = "free_y") +
  ggsave(file.path(google_figures_path, "deaths_new.png"),
         height = 12, width = 3)



ggplot() +
  geom_histogram(data = trends_df[!is.na(trends_df$ma7_hits_perda_de_olfato_bin) &
                                    !(trends_df$ma7_hits_perda_de_olfato_bin %in% 0),],
                 aes(x = cases_new),
                 fill="red") +
  theme_minimal() +
  labs(title = "Distribution of Cases when I cant smell appears\nX times in previous week") +
  facet_wrap(~ma7_hits_perda_de_olfato_bin) +
  ggsave(file.path(google_figures_path, "cases_new_hist.png"),
         height = 7, width = 7)

ggplot() +
  geom_histogram(data = trends_df[!is.na(trends_df$ma7_hits_perda_de_olfato_bin) &
                                    !(trends_df$ma7_hits_perda_de_olfato_bin %in% 0),],
                 aes(x = deaths_new),
                 fill="red") +
  theme_minimal() +
  labs(title = "Distribution of Deaths when I cant smell appears\nX times in previous week") +
  facet_wrap(~ma7_hits_perda_de_olfato_bin) +
  ggsave(file.path(google_figures_path, "deaths_new_hist.png"),
         height = 7, width = 7)

# First X Cases ----------------------------------------------------------------
trends_df_1000cases <- trends_df %>%
  group_by(state) %>%
  mutate(cases_new_max = max(cases_new, na.rm=T)) %>%
  ungroup() %>%
  filter(cases_new_max >= 1000) %>%
  group_by(state) %>%
  mutate(cases_1000_wk = min(date[cases_new >= 1000])) %>%
  mutate(wks_since_1000 = date - cases_1000_wk) %>%
  filter(wks_since_1000 < 0)

trends_df_1000deaths <- trends_df %>%
  group_by(state) %>%
  mutate(deaths_new_max = max(deaths_new, na.rm=T)) %>%
  ungroup() %>%
  filter(deaths_new_max >= 20) %>%
  group_by(state) %>%
  mutate(deaths_1000_wk = min(date[deaths_new >= 20])) %>%
  mutate(wks_since_1000 = date - deaths_1000_wk) %>%
  filter(wks_since_1000 < 0)


coeff <- 7/max(trends_df_1000cases$cases_new)

trends_df_1000cases %>%
  dplyr::select(state, wks_since_1000, cases_new, ma7_hits_perda_de_olfato_bin) %>%
  mutate(ma7_hits_perda_de_olfato_bin = ma7_hits_perda_de_olfato_bin / coeff) %>%
  pivot_longer(-c(state, wks_since_1000)) %>%
  ggplot(aes(x=wks_since_1000, y = value, group = name, color = name)) +
  geom_line() +
  geom_point(size=.1) +
  theme_minimal() +
  scale_y_continuous(
    
    # Features of the first axis
    name = "New Cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Number of Days 'Perda de Olfato' Appears in Search in Past Week")
  ) + 
  labs(color = "",
       title = "New Daily Cases vs Number Days Perda de Olfato Appears in Search in Past Week",
       x = "Days Since 1000 New Cases") +
  scale_color_manual(values = c("black", "green3"),
                     labels = c("New Cases", "Google Hits")) +
  facet_wrap(~state) +
  ggsave(file.path(google_figures_path, "cases_perdaolfato_bin_1000cases.png"),
         height = 9, width =12)

### Deaths
coeff <- 7/max(trends_df_1000deaths$deaths_new, na.rm=T)

trends_df_1000deaths %>%
  dplyr::select(state, wks_since_1000, deaths_new, ma7_hits_perda_de_olfato_bin) %>%
  mutate(ma7_hits_perda_de_olfato_bin = ma7_hits_perda_de_olfato_bin / coeff) %>%
  pivot_longer(-c(state, wks_since_1000)) %>%
  ggplot(aes(x=wks_since_1000, y = value, group = name, color = name)) +
  geom_line() +
  geom_point(size=.1) +
  theme_minimal() +
  scale_y_continuous(
    
    # Features of the first axis
    name = "New Deaths",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Number of Days 'Perda de Olfato' Appears in Search in Past Week")
  ) + 
  labs(color = "",
       title = "New Daily Deaths vs Number Days Perda de Olfato Appears in Search in Past Week",
       x = "Days Since 20 New Deaths") +
  scale_color_manual(values = c("black", "green3"),
                     labels = c("New Cases", "Google Hits")) +
  facet_wrap(~state) +
  ggsave(file.path(google_figures_path, "deaths_perdaolfato_bin_1000cases.png"),
         height = 9, width =12)






