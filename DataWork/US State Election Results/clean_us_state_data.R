# US State Election Results

# Load Data --------------------------------------------------------------------
us_df <- read.csv(file.path(dropbox_file_path, "Data", "US State Election Results", "RawData",
                               "1976-2020-president.csv"))

us_df <- us_df %>%
  dplyr::filter(year %in% c(2016, 2020)) %>%
  dplyr::mutate(prop_vote = candidatevotes / totalvotes) %>%
  dplyr::filter(candidate %in% c("TRUMP, DONALD J.")) %>%
  # Some states separate by write-in/not write in; so sum those
  group_by(state_po, year) %>%
  dplyr::summarise(prop_vote = sum(prop_vote)) %>%
  pivot_wider(id_cols = state_po,
              values_from = prop_vote,
              names_from = year) %>%
  dplyr::rename(election_2016_trump = "2016",
                election_2020_trump = "2020")

# Clean variables and add geo --------------------------------------------------
saveRDS(us_df,
        file.path(dropbox_file_path, "Data", "US State Election Results", "FinalData",
                  "us_election_results.Rds"))
write.csv(us_df,
        file.path(dropbox_file_path, "Data", "US State Election Results", "FinalData",
                  "us_election_results.csv"), row.names = F)











