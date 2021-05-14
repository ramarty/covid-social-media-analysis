# Append and Clean Google Trends Data




# Load Data --------------------------------------------------------------------
gtrends_1_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                          "_archive",
                          "global_with_ref_state_by_keyword_20201031") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  map_df(readRDS) %>%
  distinct(date, keyword, geo, time, language, .keep_all = T) %>%
  filter(!is.na(date))

# gtrends_1_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
#                           "timeseries_2020-03-05_2020-11-29") %>%
#   list.files(pattern = "*.Rds", full.names = T) %>%
#   map_df(readRDS) %>%
#   distinct(date, keyword, geo, time, language, .keep_all = T)

gtrends_2_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                          "timeseries_2020-06-01_2021-01-31") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  map_df(readRDS) %>%
  distinct(date, keyword, geo, time, language, .keep_all = T)

# Hits to Numeric --------------------------------------------------------------
gtrends_1_df$hits[gtrends_1_df$hits %in% "<1"] <- "1"
gtrends_1_df$hits <- gtrends_1_df$hits %>% as.numeric()

gtrends_2_df$hits[gtrends_2_df$hits %in% "<1"] <- "1"
gtrends_2_df$hits <- gtrends_2_df$hits %>% as.numeric()

# Merge ------------------------------------------------------------------------
## Rename/Select Variables
gtrends_1_df <- gtrends_1_df %>% 
  dplyr::rename(hits_1 = hits) %>%
  dplyr::select(hits_1, date, keyword, geo, language)

gtrends_2_df <- gtrends_2_df %>% 
  dplyr::rename(hits_2 = hits) %>%
  dplyr::select(hits_2, date, keyword, geo, language)

## Keep common keywords, languages, geo
if(F){
  gtrends_1_df$klg <- paste(gtrends_1_df$keyword, gtrends_1_df$language, gtrends_1_df$geo)
  gtrends_2_df$klg <- paste(gtrends_2_df$keyword, gtrends_2_df$language, gtrends_2_df$geo) 
  klg <- intersect(gtrends_1_df$klg %>% unique(), 
                   gtrends_2_df$klg %>% unique())
  
  gtrends_1_df <- gtrends_1_df[gtrends_1_df$klg %in% klg,]
  gtrends_2_df <- gtrends_2_df[gtrends_2_df$klg %in% klg,]
  
  gtrends_1_df$klg <- NULL
  gtrends_2_df$klg <- NULL
}

## Merge
gtrends_df <- merge(gtrends_1_df, gtrends_2_df, by = c("date", "keyword", "geo", "language"), all = T)

# Blend hits -------------------------------------------------------------------
# https://stats.stackexchange.com/questions/281162/scale-a-number-between-a-range

## Scale hits_2 to be like hits_1
gtrends_df <- gtrends_df %>%
  mutate(overlap = !is.na(hits_1) & !is.na(hits_2)) %>%
  group_by(keyword, geo, language) %>%
  mutate(hits_1_o_max = max(hits_1[overlap %in% T]),
         hits_1_o_min = min(hits_1[overlap %in% T]),
         hits_2_o_max = max(hits_2[overlap %in% T]),
         hits_2_o_min = min(hits_2[overlap %in% T])) %>%
  ungroup() %>%
  mutate(hits_2_scaled_like_hits1 = 
           ((hits_2 - hits_2_o_min) / (hits_2_o_max - hits_2_o_min)) *
           (hits_1_o_max - hits_1_o_min) +
           hits_1_o_min)

## Do certain keywords not overlap for any dates?
gtrends_df <- gtrends_df %>%
  group_by(keyword, geo, language) %>%
  mutate(overlap_anytime = max(overlap)) %>%
  ungroup()

## Create fill time series of hits value
# Use hits_1 versus scaled version
gtrends_df$hits <- gtrends_df$hits_1
gtrends_df$hits[is.na(gtrends_df$hits)] <- gtrends_df$hits_2_scaled_like_hits1[is.na(gtrends_df$hits)]

# If no overlap, use the original value
nooverlap_time1 <- (gtrends_df$overlap_anytime %in% 0) & !is.na(gtrends_df$hits_1)
nooverlap_time2 <- (gtrends_df$overlap_anytime %in% 0) & !is.na(gtrends_df$hits_2)

gtrends_df$hits[nooverlap_time1] <- gtrends_df$hits_1[nooverlap_time1]
gtrends_df$hits[nooverlap_time2] <- gtrends_df$hits_2[nooverlap_time2]

## Cleanup Variables
gtrends_df <- gtrends_df %>%
  mutate(hits = hits %>% as.numeric(),
         date = date %>% as.Date()) 

# Choose One Language per Country ----------------------------------------------
#### Merge in Language
languages <- read.csv(file.path(dropbox_file_path, 
                                "Data", "country_primary_language", "countries_lang.csv"),
                      stringsAsFactors = F) 

languages <- languages %>%
  dplyr::select(Code, Language_code_main) %>%
  dplyr::rename(geo = Code,
                state_language = Language_code_main) %>%
  filter(!(state_language == ""),
         !is.na(languages)) 

gtrends_df <- merge(gtrends_df, languages, by = "geo", all.x = T, all.y = F)

#### Only keep if google hits language matches state language
gtrends_df <- gtrends_df %>%
  filter(!is.na(state_language)) %>%
  filter(language == state_language)

# Merge in English Version of Keyword ------------------------------------------
keywords <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                              "keywords", "FinalData","covid_keywords_alllanguages_clean.Rds"))

#### Dataset with english and translated word
keywords <- keywords %>%
  dplyr::select(names(keywords) %>% str_subset("keyword")) %>%
  dplyr::mutate(keyword = keyword_en) %>%
  pivot_longer(cols = -c(keyword)) %>%
  dplyr::rename(keyword_en = keyword) %>%
  dplyr::rename(keyword = value) %>%
  dplyr::select(keyword_en, keyword) %>%
  
  mutate(keyword = keyword %>% tolower(),
         keyword_en = keyword_en %>% tolower())

#### Merge
gtrends_df <- merge(gtrends_df, keywords, by = "keyword", all.x=T, all.y=F)
# TODO: Some keywords in different languages are mapped to the same english keyword


# Cleanup ----------------------------------------------------------------------
gtrends_df <- gtrends_df %>%
  dplyr::select(keyword, keyword_en, geo, date, language, 
                hits, hits_1, hits_2, hits_2_scaled_like_hits1) %>%
  mutate(date = date %>% as.Date())

gtrends_df$hits[gtrends_df$hits %in% Inf] <- NA

# Save Data --------------------------------------------------------------------
## TODO: check why need
gtrends_df <- gtrends_df %>%
  distinct(keyword, keyword_en, geo, date, language, .keep_all = T)

saveRDS(gtrends_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries", "gtrends.Rds"))



