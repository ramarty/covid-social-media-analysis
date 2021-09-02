# Example Trends

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                "gtrends_since2020-01-01_until2021-07-31.Rds"))

gtrends_df$cases_new <- gtrends_df$cases_new_ma7

# Prep Data --------------------------------------------------------------------
# (1) Filter and (2) Scale between 0 and 1 and (3) Rename
gtrends_df <- gtrends_df %>%
  filter(keyword_en %in% "loss of smell",
         !is.na(Country)) %>%
  group_by(geo, keyword_en) %>%
  mutate(hits_ma7 = hits_ma7 / max(hits_ma7, na.rm=T),
         cases_new = cases_new / max(cases_new, na.rm=T)) 

# Figure: Top Countries --------------------------------------------------------
gtrends_df %>%
  dplyr::filter(geo %in% "IN") %>%
  ggplot() +
  geom_col(aes(x = date, y = cases_new),
           fill = "#ffc266", # orange3
           color = "#ffc266") +
  geom_line(aes(x = date, y = hits_ma7),
            color = "#3AA959", # green4
            size=0.5)


p_top <- gtrends_df %>%
  dplyr::filter(cases_total > 0,
                cor_casesMA7_hitsMA7_nolag > 0.505) %>%
  ggplot() +
  geom_col(aes(x = date, y = cases_new),
           fill = "#ffc266", # orange3
           color = "#ffc266") +
  geom_line(aes(x = date, y = hits_ma7),
            color = "#3AA959", # green4
            size=0.5) + # .75 olivedrab3 deepskyblue
  labs(x = "",
       y = "",
       title ="<span style='font-size:18pt'><span style='color:#000000;'>Trends in Google Search Interest in</span> 
               <span style='color:#3AA959;'>'Loss of Smell'</span> 
               <span style='color:#000000;'>and</span>
    <span style='color:#ff9900;'>COVID-19 Cases</span>
    <br>
    </span>") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=12),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=5)) +
  facet_wrap(~Country, 
             ncol = 4,
             scales = "free") 

ggsave(p_top, filename = file.path(paper_figures, "cases_vs_loss_of_smell_trends_topcountries.png"),
       heigh = 20, width=14.2)