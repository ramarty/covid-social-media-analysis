# Append and Clean Google Trends Data

# Load Data --------------------------------------------------------------------
gtrends_1_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                          "global_with_ref_state_by_keyword") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  unique()

gtrends_2_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                          "global_with_ref_state_by_keyword_20200601_20210131") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  unique()

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
gtrends_1_df$klg <- paste(gtrends_1_df$keyword, gtrends_1_df$language, gtrends_1_df$geo)
gtrends_2_df$klg <- paste(gtrends_2_df$keyword, gtrends_2_df$language, gtrends_2_df$geo) 
klg <- intersect(gtrends_1_df$klg %>% unique(), 
                 gtrends_2_df$klg %>% unique())

gtrends_1_df <- gtrends_1_df[gtrends_1_df$klg %in% klg,]
gtrends_2_df <- gtrends_2_df[gtrends_2_df$klg %in% klg,]

gtrends_1_df$klg <- NULL
gtrends_2_df$klg <- NULL

## Merge
gtrends_df <- merge(gtrends_1_df, gtrends_2_df, by = c("date", "keyword", "geo", "language"), all = T)

# Blend hits -------------------------------------------------------------------
# https://stats.stackexchange.com/questions/281162/scale-a-number-between-a-range
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

gtrends_df$hits <- gtrends_df$hits_1
gtrends_df$hits[is.na(gtrends_df$hits)] <- gtrends_df$hits_2_scaled_like_hits1[is.na(gtrends_df$hits)]


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
  filter(!(state_language == ""))

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

# Cleanup ----------------------------------------------------------------------
gtrends_df <- gtrends_df %>%
  dplyr::select(keyword, keyword_en, geo, date, language, 
                hits, hits_1, hits_2, hits_2_scaled_like_hits1) %>%
  mutate(date = date %>% as.Date())

# Save Data --------------------------------------------------------------------
saveRDS(gtrends_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries", "gtrends.Rds"))





