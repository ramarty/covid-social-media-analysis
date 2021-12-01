# Clean Global Data

# Load gTrends data ------------------------------------------------------------
# FILL MISSING HITS WITH ZERO

dirs_all <- file.path(data_dir, "google_trends", "RawData") %>%
  list.files(full.names = T) %>%
  str_subset("timeseriesALL_")

readRDS_addname <- function(path){
  df <- readRDS(path)
  df <- df$interest_by_country
  df$file_path <- path
  df$hits <- df$hits %>% as.character()
  return(df)
}

gdata_df <- map_df(dirs_all, function(dir_i){
  list.files(dir_i, full.names = T) %>%
    map_df(readRDS_addname)
})

gdata_df$timespan <- gdata_df$file_path %>%
  str_replace_all(".*timeseriesALL_", "") %>%
  str_replace_all("/.*", "")

gdata_df$keyword <- gdata_df$file_path %>%
  str_replace_all(".*_term", "") %>%
  str_replace_all("_language.*", "")

gdata_df$geo <- countrycode(gdata_df$location,origin = "country.name", destination = "iso2c")
gdata_df <- gdata_df[!is.na(gdata_df$geo),]
gdata_df$location <- NULL

# Load Vaccination Data --------------------------------------------------------
vac_df <- read.csv(file.path(data_dir, "global_vaccine", "RawData", "owid-covid-data.csv"), stringsAsFactors = F)
vac_df$geo <- countrycode(vac_df$iso_code, origin = "iso3c", destination = "iso2c")

vac_df <- vac_df %>%
  dplyr::select(geo, location, continent, population, people_fully_vaccinated_per_hundred) %>%
  group_by(geo, location, continent, population) %>%
  dplyr::summarise(people_fully_vaccinated_per_hundred = 
                     max(people_fully_vaccinated_per_hundred, na.rm = T)) %>%
  dplyr::mutate(prop = (people_fully_vaccinated_per_hundred*100)/population)

# Merge and Clean --------------------------------------------------------------
data <- merge(gdata_df, vac_df, by = "geo")
data$hits[data$hits %in% "<1"] <- "1"
#data$hits[data$hits %in% ""] <- "1"
data$hits <- data$hits %>% as.numeric()

data$location[data$hits < 50] <- NA
data %>%
  dplyr::filter(keyword %in% c("vaccine", "covid vaccine"),
                timespan == "2020-12-01_2021-08-31",
                people_fully_vaccinated_per_hundred >= 0,
                population >= 100000) %>%
  ggplot(aes(x = hits,
             y = people_fully_vaccinated_per_hundred,
             label = location)) +
  geom_point() +
  geom_text_repel() +
  facet_grid(~keyword)


