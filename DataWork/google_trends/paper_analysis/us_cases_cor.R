# What variables explain correlation?

# Prep Cases -------------------------------------------------------------------
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
  dplyr::summarise(value = max(value))
  
