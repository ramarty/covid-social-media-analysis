# Vaccine figures

# RELEVANT LIT
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0251605

BEGIN_DATE <- "2020-12-01"
END_DATE <- "2021-09-30"

BEGIN_DATE <- "2020-12-01"
END_DATE <- "2021-05-31"

for(END_DATE in c("2021-05-31", "2021-09-30")){
  print(END_DATE)
  
  # Load data --------------------------------------------------------------------
  gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                  "gtrends_regional",
                                  "gtrends_regional_clean.Rds"))
  
  ## Define var
  TIME_SPAN_END <- paste0(BEGIN_DATE, " ", END_DATE)
  gtrends_df <- gtrends_df %>%
    dplyr::filter(time_span == TIME_SPAN_END) 
  
  gtrends_df$people_vaccinated_per_hundred <- gtrends_df[[paste0("vax_per100_",
                                                                 END_DATE %>% str_replace_all("-", "_"))]]
  
  
  ## Useful for thinking about figures
  cor_df <- gtrends_df %>%
    dplyr::filter(!is.na(hits),
                  !is.na(people_vaccinated_per_hundred)) %>%
    group_by(keyword) %>%
    dplyr::summarise(cor = cor(hits, people_vaccinated_per_hundred),
                     N = n())
  
  # A. Vaccine -------------------------------------------------------------------
  p_vaccine <- gtrends_df %>%
    dplyr::filter(keyword %in% c("vaccine", "covid vaccine side effects",
                                 "covid vaccine safety", "covid-19")) %>%
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
                                 "covid vaccine cause infertility",
                                 "ivermectin")) %>%
    
    # Cleanup title
    dplyr::mutate(keyword = case_when(
      keyword == "is the covid vaccine the mark of the beast" ~ 
        "is the covid\nvaccine the\nmark of the beast",
      
      keyword == "does covid vaccine change dna" ~
        "does covid\nvaccine\nchange dna",
      
      keyword == "covid vaccine cause infertility" ~
        "covid vaccine\ncause infertility",
      
      keyword == "covid microchip" ~
        "covid\nmicrochip",
      
      TRUE ~ keyword),
      title = keyword %>% tools::toTitleCase()) %>%
    #       title = paste0("Search interest in:\n", keyword)
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
    #theme_minimal() +
    theme_clean() +
    theme(strip.text = element_text(hjust = 0, face = "bold"),
          plot.title = element_text(hjust = 0, face = "bold"),
          plot.background=element_blank()) +
    scale_color_manual(values = c("blue", "red")) +
    facet_wrap(~title, nrow = 1, scales = "free_y")
  
  # C. Vaccine conspiracies popularity -------------------------------------------
  file.path(gtrends_dir, "RawData", "search_interest_across_terms_us") %>% list.files()
  
  us_misinfo_df <- readRDS(file.path(gtrends_dir, "RawData", "search_interest_across_terms_us", 
                                     paste0("gtrends_missinfo_",
                                            BEGIN_DATE,
                                            "_",
                                            END_DATE,
                                            ".Rds")))
  
  us_misinfo_int_df <- us_misinfo_df$interest_over_time
  us_misinfo_int_df <- us_misinfo_int_df %>%
    dplyr::mutate(hits = hits %>% as.character(),
                  hits = case_when(hits %in% "<1" ~ "0.5",
                                   TRUE ~ hits),
                  hits = hits %>%
                    as.character() %>%
                    as.numeric())
  
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
                                        "Is the COVID Vaccine\nthe Mark of the Beast",
                                      TRUE ~ keyword),
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
                        widths = c(0.66, 0.32), # 0.66, 0.37
                        ncol = 2)
  
  p <- ggarrange(p_vaccine, p_bottom,
                 heights = c(0.66, 0.37),
                 ncol = 1)
  
  ggsave(p, filename = file.path(paper_figures, paste0("vaccine_panels_",
                                                       BEGIN_DATE %>% str_replace_all("-", "_"),
                                                       END_DATE %>% str_replace_all("-", "_")
                                                       ,".png")),
         height = 13, width = 11)
  
}
#common.legend = TRUE,
#legend = "right"




