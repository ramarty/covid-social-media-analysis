# Initial Figures of Trends

library(TTR)

# Load Data --------------------------------------------------------------------
trends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", 
                               "brazil_with_refstate_analysis.Rds"))

trends_df <- trends_df %>%
  dplyr::rename(state = name)

## Grab keyword lists
fst_per_keywrds <- trends_df$keyword[trends_df$categories %in% "in_1st_person"] %>% unique() %>%
  str_replace_all(" ", "_")

# NAs --------------------------------------------------------------------------
#### Remove by NA
trends_df <- trends_df %>%
  filter(!is.na(keyword),
         !is.na(date),
         !is.na(state)) 

#### NA as 0
trends_df$hits[is.na(trends_df$hits)] <- 0
trends_df$hits_adj[is.na(trends_df$hits_adj)] <- 0
trends_df$cases[is.na(trends_df$cases)] <- 0

# Lags and first differences ---------------------------------------------------
trends_df <- trends_df %>%
  dplyr::arrange(date) %>%
  dplyr::group_by(state, keyword) %>%
  dplyr::mutate(cases_new = cases - lag(cases)) %>%
  dplyr::mutate(deaths_new = deaths - lag(deaths)) 

# Long to Wide -----------------------------------------------------------------
make_long_to_wide <- function(trends_df, var){
  
  constant_vars <- c("date", "state")
  
  trends_df$var <- trends_df[[var]]
  
  trends_wide <- trends_df %>%
    dplyr::select(var, keyword, date, state) %>%
    pivot_wider(names_from = keyword,
                values_from = var) 
  
  names(trends_wide) <- names(trends_wide) %>% str_replace_all(" ", "_")
  
  names(trends_wide)[!(names(trends_wide) %in% constant_vars)] <- 
    paste0(var,"_", names(trends_wide)[!(names(trends_wide) %in% constant_vars)])
  
  return(trends_wide)
}

trends_wide_all <- trends_df %>%
  dplyr::select(hits, keyword, date, state,
                cases, cases_new,
                deaths, deaths_new) %>%
  pivot_wider(names_from = keyword,
              values_from = hits) %>%
  dplyr::select(date, state, 
                cases, cases_new,
                deaths, deaths_new)

trends_wide_all <- trends_wide_all %>%
  left_join(make_long_to_wide(trends_df, "hits"), by = c("state", "date")) %>%
  left_join(make_long_to_wide(trends_df, "hits_adj"), by = c("state", "date")) 

## NAs as 0
for(var in names(trends_wide_all)[!names(trends_wide_all) %in% c("state", "date")]){
  trends_wide_all[[var]][is.na(trends_wide_all[[var]])] <- 0
}

# Create Binary Variables ------------------------------------------------------
## Any hits
keywrds_to_bin <- c("perda_de_olfato", "perdi_o_olfato") %>% paste(collapse = "|")
vars_to_bin <- names(trends_wide_all)[grepl(keywrds_to_bin, names(trends_wide_all))]

for(var in vars_to_bin){
  trends_wide_all[[paste0(var, "_bin")]] <- 
    as.numeric(trends_wide_all[[var]] > 0)
}

# Moving Average ---------------------------------------------------------------
trends_wide_all <- trends_wide_all %>%
  arrange(date)

ma7 <- trends_wide_all %>%
  group_by(state) %>%
  mutate_if(is.numeric, function(x) runMean(x , n=7)) %>%
  rename_if(is.numeric, ~paste0("ma7_", . ))

ma14 <- trends_wide_all %>%
  group_by(state) %>%
  mutate_if(is.numeric, function(x) runMean(x , n=14)) %>%
  rename_if(is.numeric, ~paste0("ma14_", . ))

trends_wide_all <- trends_wide_all %>%
  left_join(ma7, by = c("date", "state")) %>%
  left_join(ma14, by = c("date", "state"))

# Lag Variables ----------------------------------------------------------------
trends_wide_all <- trends_wide_all %>%
  arrange(date)

keywrds_to_lag <- c("perda_de_olfato") %>% paste(collapse = "|")
vars_to_lag <- names(trends_wide_all)[grepl(keywrds_to_bin, names(trends_wide_all))]

vars_to_lag <- "ma7_hits_perda_de_olfato_bin"

trends_wide_all <- lapply(1:25, function(lag_i){
  print(lag_i)
  
  lag_df <- trends_wide_all %>%
    group_by(state) %>%
    mutate_at(vars_to_lag, function(x) lag(x, lag_i)) %>%
    rename_if(is.numeric, ~paste0("lag", lag_i, "_", . ))
  
  return(lag_df)
}) %>%
  reduce(full_join, by = c("date", "state")) %>%
  right_join(trends_wide_all, by = c("date", "state"))
  
# Export -----------------------------------------------------------------------
saveRDS(trends_wide_all, file.path(dropbox_file_path, "Data", "google_trends", "FinalData", 
                                   "brazil_with_refstate_analysis_long.Rds"))

