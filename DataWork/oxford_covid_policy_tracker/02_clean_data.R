# Clean Oxford Policy Response Data

# Create a dataset that indicates the first date of lockdown/closures for each
# country

# Load data --------------------------------------------------------------------
ox_df <- readRDS(file.path(oxpol_dir, "RawData", "OxCGRT_latest.Rds"))

# Helper Functions -------------------------------------------------------------
is_greater_zero <- function(x) as.numeric(x > 0)

min_ignore_0_and_na <- function(x){
  x_clean <- x[!is.na(x)]
  out <- min(x_clean[x_clean > 0])
  return(out)
}

# Clean data -------------------------------------------------------------------
ox_clean_df <- ox_df %>%
  dplyr::select(CountryCode, Date, 
                "C1_School closing",
                "C2_Workplace closing",
                "C3_Cancel public events",
                "C4_Restrictions on gatherings",
                "C5_Close public transport",
                "C6_Stay at home requirements",
                "C7_Restrictions on internal movement",
                "C8_International travel controls") %>%
  dplyr::mutate(C_policy = 
                  `C1_School closing` +
                  `C2_Workplace closing` +
                  `C3_Cancel public events` +
                  `C4_Restrictions on gatherings` +
                  `C5_Close public transport` +
                  `C6_Stay at home requirements` +
                  `C7_Restrictions on internal movement` +
                  `C8_International travel controls`) %>%
  dplyr::mutate_at(vars(c("C1_School closing",
                          "C2_Workplace closing",
                          "C3_Cancel public events",
                          "C4_Restrictions on gatherings",
                          "C5_Close public transport",
                          "C6_Stay at home requirements",
                          "C7_Restrictions on internal movement",
                          "C8_International travel controls",
                          "C_policy")), 
                   ~is_greater_zero(.) * Date) %>%
  dplyr::select(-Date) %>%
  dplyr::group_by(CountryCode) %>%
  dplyr::summarise_all(min_ignore_0_and_na) %>%
  dplyr::mutate_if(is.numeric, . %>% as.character() %>% ymd()) %>%
  dplyr::mutate(geo = CountryCode %>% countrycode(origin = "iso3c",
                                                  destination = "iso2c")) %>%
  dplyr::mutate(geo = case_when(CountryCode == "RKS" ~ "XK",
                                TRUE ~ geo)) %>%
  dplyr::select(-CountryCode)

names(ox_clean_df) <- names(ox_clean_df) %>%
  tolower() %>%
  str_replace_all(" ", "_")

# Export data ------------------------------------------------------------------
saveRDS(ox_clean_df, 
        file.path(oxpol_dir, "FinalData", "OxCGRT_earliest_measure.Rds"))







