# Clean Europe Vaccine Data

eeuu <- read.csv(file.path(data_dir, "eeuu_vaccine", "RawData", "data.csv"), stringsAsFactors = F)

eeuu_clean <- eeuu %>%
  dplyr::filter(TargetGroup %in% "ALL",
                Region == ReportingCountry) %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(FirstDose = sum(FirstDose),
                   Denominator = max(Denominator)) %>%
  dplyr::mutate(prop = FirstDose / Denominator)

eeuu_clean$YearWeekISO %>% max()


eeuu_clean$ReportingCountry %>% unique()
