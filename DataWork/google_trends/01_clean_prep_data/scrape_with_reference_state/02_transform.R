# Applies equation to make hits comparable across time/states using a comparison
# state

comparison_iso <- "BR-SP"

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "RawData", 
                                paste0("br_gtrends_ref",comparison_iso,".Rds")))


# Using formulat on page 32 
# http://documents1.worldbank.org/curated/en/821821591104924698/pdf/Winners-and-Losers-from-COVID-19-Global-Evidence-from-Google-Search.pdf

gtrends_df <- gtrends_df %>%
  
  mutate(hits = hits %>% as.numeric(),
         hits_with_compstate = hits_with_compstate %>% as.numeric(),
         hits_compstate = hits_compstate %>% as.numeric(),
         date = date %>% as.Date()) %>%
  
  group_by(geo, keyword) %>%
  mutate(hits_adj = hits_with_compstate / max(hits_compstate))

# Save Data --------------------------------------------------------------------
saveRDS(gtrends_df, file.path(dropbox_file_path, "Data", "google_trends", "RawData", 
                  paste0("br_gtrends_ref",comparison_iso,"_adj.Rds")))



