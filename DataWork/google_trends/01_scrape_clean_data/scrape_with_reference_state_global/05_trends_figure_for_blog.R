# Merge Correlation Data with Other Data

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_cor_since_","2020-02-01",".Rds")))

gtrends_df <- gtrends_df %>%
  group_by(geo, keyword_en) %>%
  mutate(hits_ma7 = hits_ma7 / max(hits_ma7, na.rm=T),
         cases_new = cases_new / max(cases_new, na.rm=T))

gtrends_df <- gtrends_df %>%
  filter(keyword_en %in% "loss of smell")

gtrends_df$Country[gtrends_df$Country %in% "occupied Palestinian territory, including east Jerusalem"] <- "Palestine"
gtrends_df$Country[gtrends_df$Country %in% "Iran (Islamic Republic of)"] <- "Iran"
gtrends_df$Country[gtrends_df$Country %in% "Bolivia (Plurinational State of)"] <- "Bolivia"
gtrends_df$Country[gtrends_df$Country %in% "United Republic of Tanzania"] <- "Tanzania"
gtrends_df$Country[gtrends_df$Country %in% "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
gtrends_df$Country[gtrends_df$Country %in% "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"

# gtrends_df <- gtrends_df %>%
#   filter(Country %in% c("United States of America", "Australia", "Brazil"))

library(ggtext)

gtrends_df %>%
  filter(cases_total > 0) %>%
  ggplot() +
  geom_col(aes(x = date, y = cases_new),
           fill = "orange3", # orange3
           color = "orange3") +
  geom_line(aes(x = date, y = hits_ma7),
            color = "green4",
            size=0.75) + # olivedrab3 deepskyblue
  labs(x = "",
       y = "",
       title ="<span style='font-size:18pt'><span style='color:#ffffff;'>Trends in</span> 
               <span style='color:green4;'>'Loss of Smell'</span> 
               <span style='color:#ffffff;'>and</span>
    <span style='color:orange2;'>COVID-19 Cases</span>
    <br>
    </span>") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold")) +
  facet_wrap(~Country, 
             ncol = 9,
             scales = "free") +
  ggsave(filename = file.path(dropbox_file_path,
                              "Data",
                              "google_trends",
                              "Outputs",
                              "figures",
                              "trend_i_cant_smell.png"),
         heigh = 12, width=14.1)

# #f9ba2d
# #3aa757

#gtrends_df$Country[nchar(gtrends_df$Country) > 10] %>% unique()
