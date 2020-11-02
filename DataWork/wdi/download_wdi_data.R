# Download WDI data

download_wdi_indicator <- function(indicator){
  df <- WDI(country = "all",
            indicator = indicator,
            start = 2018,
            end = 2018,
            extra = TRUE)
  
  #df$iso2c <- NULL
  df$capital <- NULL
  df$longitude <- NULL 
  df$latitude <- NULL
  
  return(df)
}

indicator_list <- c("SP.POP.TOTL", "NY.GDP.PCAP.KD", "IT.NET.USER.ZS")

wdi_data <- lapply(indicator_list, download_wdi_indicator) %>%
  purrr::reduce(left_join, by = c("iso3c", "iso2c", "country", "year", "region", "income", "lending")) %>%
  dplyr::select(iso3c, country, year, region, income, lending, everything()) %>%
  filter(lending != "Aggregates") %>%
  dplyr::rename(iso = iso3c) %>%
  dplyr::mutate(iso = iso %>% tolower)

saveRDS(wdi_data, file.path(dropbox_file_path, "Data", "wdi", "RawData", "wdi_data.Rds"))
write.csv(wdi_data, file.path(dropbox_file_path, "Data", "wdi", "RawData", "wdi_data.csv"),
          row.names = F)



