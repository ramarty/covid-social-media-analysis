# Vaccine figures

# RELEVANT LIT
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0251605

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_regional",
                                "gtrends_regional_clean.Rds"))

## Useful for thinking about figures
cor_df <- gtrends_df %>%
  dplyr::filter(time_span == "2020-12-01_2021-05-31",
                !is.na(hits),
                !is.na(people_vaccinated_per_hundred)) %>%
  group_by(keyword) %>%
  dplyr::summarise(cor = cor(hits, people_vaccinated_per_hundred),
                   N = n())

# A. Vaccine -------------------------------------------------------------------
p_vaccine <- gtrends_df %>%
  dplyr::filter(keyword %in% c("vaccine", "covid vaccine side effects",
                               "covid vaccine safety", "covid-19"),
                time_span == "2020-12-01_2021-05-31") %>%
  group_by(keyword) %>%
  dplyr::mutate(cor = cor.test(people_vaccinated_per_hundred, hits)$estimate) %>%
  ungroup() %>%
  mutate(title = paste0("Search interest in: ", keyword %>% tools::toTitleCase(),
                        "\nCorrelation Coefficient: ", cor %>% round(2))) %>%
  mutate(title = fct_reorder(title, cor) %>% fct_rev()) %>%
  ggplot(aes(y = people_vaccinated_per_hundred,
             x = hits)) +
  geom_smooth(method = "lm",
              color = "wheat4",
              fill = "wheat1",
              size = 0.5) +
  geom_point(aes(color = party_winner)) +
  geom_text_repel(aes(label = location), size = 2) +
  labs(x = 'Search Interest',
       y = '% Vaccinated',
       color = '2020 Election\nWinner',
       title = "A. Association between vaccine search interest and vaccination rates") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(#axis.title.y = element_text(angle = 0, vjust = 0.5),
    strip.text = element_text(hjust = 0, face = "bold"),
    plot.title = element_text(hjust = 0, face = "bold")) + 
  facet_wrap(~title, nrow = 2)

# B. Vaccine conspiracies ------------------------------------------------------
p_conspiracy <- gtrends_df %>%
  # Subset
  dplyr::filter(keyword %in% c("is the covid vaccine the mark of the beast",
                               "covid microchip",
                               "does covid vaccine change dna",
                               "covid vaccine cause infertility"),
                time_span == "2020-12-01_2021-05-31") %>%
  
  # Cleanup title
  dplyr::mutate(keyword = case_when(
    keyword == "is the covid vaccine the mark of the beast" ~ 
      "is the covid vaccine\nthe mark of the beast",
    TRUE ~ keyword),
    keyword = keyword %>% tools::toTitleCase(),
    title = paste0("Search interest in:\n", keyword)) %>%
  
  # Define low/high (below/above median)
  group_by(keyword) %>%
  dplyr::mutate(hits_median = median(hits, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(cat = case_when(
    is.na(hits) ~ "None",
    hits <= hits_median ~ "Low",
    hits > hits_median ~ "High"
  ) %>%
    factor(levels = c("None", "Low", "High"))) %>%
  
  # Figure
  ggplot(aes(x = cat, y = people_vaccinated_per_hundred)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width=0.11),
             aes(color = party_winner)) +
  labs(x = "Search Interest",
       y = "% Vaccinated",
       color = '2020 Election\nWinner',
       title = "B. Search interest in vaccine missinformation and vaccination rates") +
  theme_minimal() +
  theme(strip.text = element_text(hjust = 0, face = "bold"),
        plot.title = element_text(hjust = 0, face = "bold")) +
  scale_color_manual(values = c("blue", "red")) +
  facet_wrap(~title, nrow = 1)

# C. Vaccine conspiracies popularity -------------------------------------------
us_misinfo_df <- readRDS(file.path(gtrends_dir, "RawData", "search_interest_across_terms_us", 
                                   "gtrends_missinfo_2020-12-01_2021-07-31.Rds"))

us_misinfo_int_df <- us_misinfo_df$interest_over_time

us_misinfo_int_popularity_df <- us_misinfo_int_df %>%
  group_by(keyword) %>%
  dplyr::summarise(hits = mean(hits)) %>%
  ungroup() %>%
  dplyr::mutate(hits = hits/max(hits)*100,
                keyword = keyword %>% tools::toTitleCase(),
                keyword = case_when(keyword %in% "Covid Vaccine Change Dna" ~
                                      "COVID Vaccine\nChange DNA",
                                    keyword %in% "Covid Vaccine Cause Infertility" ~
                                      "COVID Vaccine\nCause Infertility",
                                    keyword %in% "Covid Microchip" ~
                                      "COVID Microchip",
                                    keyword %in% "Is the Covid Vaccine the Mark of the Beast" ~
                                      "Is the COVID Vaccine\nthe Mark of the Beast"),
  ) 

p_search_pop <- us_misinfo_int_popularity_df %>%
  ggplot() +
  geom_col(aes(y = reorder(keyword, hits), 
               x = hits),
           fill = "dodgerblue4") +
  labs(x = "Search Interest",
       y = NULL,
       title = "C. Relative interest\nin missinformation\nsearch terms") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.y = element_text(face = "bold", color = "black"),
        plot.title = element_text(hjust = 0, face = "bold")) 

# Arrange ----------------------------------------------------------------------
p_bottom <- ggarrange(p_conspiracy + theme(legend.position = "none"), 
                      p_search_pop,
                      widths = c(0.66, 0.37),
                      ncol = 2)

p <- ggarrange(p_vaccine, p_bottom,
               heights = c(0.66, 0.37),
               ncol = 1)

ggsave(p, filename = file.path(paper_figures, "vaccine_panels.png"),
       height = 13, width = 11)

#common.legend = TRUE,
#legend = "right"




