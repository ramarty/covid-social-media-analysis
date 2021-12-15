
#dateranges <- file.path(gtrends_dir, "RawData", "search_interest_refivermectin_across_terms_us") %>%
#  list.files() %>%
#  str_sub(-25,-5) %>%
#  unique()

#daterange_i <- dateranges[1]

google_df <- file.path(gtrends_dir, "RawData", "search_interest_refivermectin_across_terms_us") %>%
  list.files(full.names = T) %>%
  #str_subset(daterange_i) %>%
  map_df(function(file_i){
    
    df <- readRDS(file_i)
    
    df_sum <- df$interest_over_time %>%
      dplyr::mutate(hits = hits %>% as.character(),
                    hits = case_when(hits %in% "<1" ~ "0.5",
                                     TRUE ~ hits),
                    hits = hits %>%
                      as.character() %>%
                      as.numeric()) %>%
      group_by(keyword) %>%
      dplyr::summarise(hits = mean(hits)) 
    
    value_notiver <- df_sum$hits[df_sum$keyword != "ivermectin"]
    value_iver    <- df_sum$hits[df_sum$keyword == "ivermectin"]
    
    value <- value_notiver/value_iver
    
    df_out <- data.frame(hits_rel_iver = value,
                         keyword = df_sum$keyword[df_sum$keyword != "ivermectin"],
                         time = df$interest_over_time$time[1])
    
    return(df_out)
  })

## Add in ivermectin (reference)
iver_df <- google_df %>%
  distinct(time, .keep_all = T)
iver_df$hits_rel_iver <- 1
iver_df$keyword <- "ivermectin"

google_df <- bind_rows(google_df, iver_df)
google_df$keyword_en <- google_df$keyword

# Categorize -------------------------------------------------------------------
google_df <- google_df %>%
  dplyr::mutate(keyword_cat = case_when(
    # Searches specifically related to appointments
    keyword_en %in% c("can i get the covid vaccine",
                      "covid vaccine appointment",
                      "vaccine appointment",
                      "covid vaccine center") ~ "Vaccine Appointment",
    
    keyword_en %in% c("covid vaccine",
                      "covid vaccine priority",
                      "covid vaccine priority list",
                      "covid vaccine approved",
                      "is covid vaccine approved", ## COULD DELETE
                      "covid vaccine second dose",
                      "vaccine near me", ## COULD DELETE
                      "where to get covid vaccine",
                      "vaccine") ~ "Vaccine General",
    
    keyword_en %in% c("covid vaccine blood clots",
                      "covid vaccine safety",
                      "covid vaccine sick", ## COULD DELETE
                      "covid vaccine side effects",
                      "safety of covid vaccine",
                      "vaccine allergy",
                      "long term effects of covid vaccine",
                      "vaccine reaction",
                      "fear of needles",
                      "needle phobia",
                      "trypanophobia") ~ "Side Effects & Safety",
    
    keyword_en %in% c("covid microchip",
                      "covid vaccine microchip",
                      "covid vaccine cause infertility",
                      "covid vaccine infertility",
                      "covid vaccine change dna",
                      "does covid vaccine change dna",
                      "covid vaccine dangerous",
                      "is the covid vaccine the mark of the beast",
                      "covid vaccine mercury",
                      "ivermectin") ~ "Misinformation"
  ))

# Cleanup ----------------------------------------------------------------------
google_df <- google_df %>%
  dplyr::mutate(keyword_en = keyword_en %>%
                  tools::toTitleCase() %>%
                  str_replace_all("\\bi\\b", "I") %>%
                  str_replace_all("^where\\b", "Where") %>%
                  str_replace_all("^can\\b", "Can"))

# Export -----------------------------------------------------------------------
saveRDS(google_df, file.path(gtrends_dir, "FinalData", "gtrends_usa_ref_ivermectin", 
                             "gtrends_usa_ref_ivermectin.Rds"))




