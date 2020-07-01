# Scrape Data from Google Trends
# Loop through states and terms. For each state-term combination, also search
# for the comparison iso/state. Consequently, for each search, we'll get hits
# for 1 term and 2 states.

comparison_iso <- "BR-RJ"

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "RawData", 
                                paste0("br_gtrends_ref",comparison_iso,".Rds")))

# For the comparison state, add hits_comparison_state that is just hits
gtrends_df$hits_comparison_state[gtrends_df$geo == comparison_iso] <-
  gtrends_df$hits[gtrends_df$geo == comparison_iso]

# Using formulat on page 32 
# http://documents1.worldbank.org/curated/en/821821591104924698/pdf/Winners-and-Losers-from-COVID-19-Global-Evidence-from-Google-Search.pdf

gtrends_df <- gtrends_df %>%
  
  mutate(hits = hits %>% as.numeric(),
         hits_comparison_state = hits_comparison_state %>% as.numeric(),
         date = date %>% as.Date()) %>%
  
  group_by(geo, keyword) %>%
  mutate(hits_adj = hits / max(hits_comparison_state))

# Save Data --------------------------------------------------------------------
saveRDS(gtrends_df, file.path(dropbox_file_path, "Data", "google_trends", "RawData", 
                  paste0("br_gtrends_ref",comparison_iso,"_adj.Rds")))


# Some Quick Checks - - - - - - - - - - - - - -
gtrends_df %>%
  filter(keyword %in% "febre") %>%
  ggplot(aes(x = date, y = hits)) +
  geom_line() +
  labs(title = "Hits") +
  facet_wrap(~geo)

gtrends_df %>%
  filter(keyword %in% "febre") %>%
  ggplot(aes(x = date, y = hits_comparison_state)) +
  geom_line() +
  labs(title = "Hits Comparison State") +
  facet_wrap(~geo)

gtrends_df %>%
  filter(keyword %in% "febre") %>%
  ggplot(aes(x = date, y = hits_adj)) +
  geom_line() +
  labs(title = "Hits Adjusted") +
  facet_wrap(~geo)
