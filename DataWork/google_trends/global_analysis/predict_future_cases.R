# Predict Future Cases

comparison_iso <- "US"

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref",comparison_iso,"_adj_cases.Rds")))

gtrends_df <- gtrends_df[gtrends_df$keyword_en %in% "loss of smell",]

# Prep Variables ---------------------------------------------------------------

#### Moving Averages
gtrends_df <- gtrends_df %>%
  arrange(date) %>%
  group_by(geo) %>%
  mutate(hits_ma7 = runMean(hits, n = 7),
         cases_ma7 = runMean(cases, n = 7)) %>%
  mutate(hits_ma7_lag7 = lag(hits_ma7, 7)) %>%
  mutate(cases_lag7 = lag(cases, 7)) %>%
  ungroup() %>%
  mutate(hits_ma7_7dayinc = hits_ma7 - hits_ma7_lag7,
         cases_7dayinc = cases - cases_lag7) %>%
  dplyr::select(-hits_ma7_lag7)

#### Lag
vars_to_lag <- c("hits_ma7_7dayinc",
                 "hits_ma7",
                 "hits")

gtrends_df <- lapply(1:25, function(lag_i){
  print(lag_i)
  
  lag_df <- gtrends_df %>%
    dplyr::select(c("date", "geo", vars_to_lag)) %>%
    arrange(date) %>%
    group_by(geo) %>%
    mutate_at(vars_to_lag, function(x) lag(x, lag_i)) %>%
    rename_if(is.numeric, ~paste0( . , "_lag", lag_i ))
  
  return(lag_df)
}) %>%
  reduce(full_join, by = c("date", "geo")) %>%
  right_join(gtrends_df, by = c("date", "geo"))

gtrends_df <- lapply(1:25, function(lag_i){
  print(lag_i)
  
  lag_df <- gtrends_df %>%
    dplyr::select(c("date", "geo", vars_to_lag)) %>%
    arrange(date) %>%
    group_by(geo) %>%
    mutate_at(vars_to_lag, function(x) lead(x, lag_i)) %>%
    rename_if(is.numeric, ~paste0( . , "_lead", lag_i ))
  
  return(lag_df)
}) %>%
  reduce(full_join, by = c("date", "geo")) %>%
  right_join(gtrends_df, by = c("date", "geo"))

gtrends_df <- gtrends_df %>%
  dplyr::rename(hits_ma7_7dayinc_lead0 = hits_ma7_7dayinc,
                hits_ma7_lead0 = hits_ma7,
                hits_lead0 = hits)

hits_vars <- names(gtrends_df) %>% str_subset("hits_") %>%
  str_subset("lead|lag")

gtrends_long_df <- gtrends_df %>%
  dplyr::select(c("Country", "geo", "date", "keyword_en", "cases_new", "cases_7dayinc", hits_vars)) %>%
  pivot_longer(cols = -c(Country, geo, date, keyword_en, cases_7dayinc, cases_new)) %>%
  mutate(time_lag = name %>%
           str_replace_all("hits_ma7_7dayinc_", "") %>%
           str_replace_all("hits_ma7_", "") %>%
           str_replace_all("hits_", "") %>%
           str_replace_all("lag", "-") %>%
           str_replace_all("lead", "") %>%
           as.numeric(),
         hits_type = name %>%
           str_replace_all("_lead.*", "") %>%
           str_replace_all("_lag.*", "")) %>%
  dplyr::rename(hits_value = value)

cor_df <- gtrends_long_df %>%
  filter(!is.na(cases_7dayinc),
         !is.na(hits_value)) %>%
  group_by(keyword_en, time_lag, geo, hits_type) %>%
  mutate(cor_level = cor(cases_new, hits_value),
         cor_7dayinc = cor(cases_7dayinc, hits_value))

p <- cor_df %>%
  filter(hits_type %in% "hits") %>%
  ggplot() +
  geom_col(aes(x = time_lag, y = cor_level, fill = cor_level)) + 
  geom_vline(xintercept = 0,
             color = "black") +
  scale_fill_gradient2(low =  "#1A9850",
                       mid = "#FFFFBF",
                       high = "#D73027",
                       midpoint = 0) +
  labs(x = "Time Lag (Days)",
       y = "Correlation") +
  theme_ipsum() +
  facet_wrap(~Country,
             ncol = 2)
ggsave(p, filename = file.path("~/Desktop/hits.png"),
       height = 30, width=10)

p <- cor_df %>%
  filter(hits_type %in% "hits_ma7") %>%
  ggplot() +
  geom_col(aes(x = time_lag, y = cor_level, fill = cor_level)) + 
  geom_vline(xintercept = 0,
             color = "black") +
  scale_fill_gradient2(low =  "#1A9850",
                       mid = "#FFFFBF",
                       high = "#D73027",
                       midpoint = 0) +
  labs(x = "Time Lag (Days)",
       y = "Correlation") +
  theme_ipsum() +
  facet_wrap(~Country,
             ncol = 2)
ggsave(p, filename = file.path("~/Desktop/hits_ma7.png"),
       height = 30, width=10)

p <- cor_df %>%
  filter(hits_type %in% "hits_ma7_7dayinc") %>%
  ggplot() +
  geom_col(aes(x = time_lag, y = cor_7dayinc, fill = cor_7dayinc)) + 
  geom_vline(xintercept = 0,
             color = "black") +
  scale_fill_gradient2(low =  "#1A9850",
                       mid = "#FFFFBF",
                       high = "#D73027",
                       midpoint = 0) +
  labs(x = "Time Lag (Days)",
       y = "Correlation") +
  theme_ipsum() +
  facet_wrap(~Country,
             ncol = 2)
ggsave(p, filename = file.path("~/Desktop/hits_ma7_7dayinc.png"),
       height = 30, width=10)




cor_max_df <- cor_df %>%
  filter(!is.na(cor_level)) %>%
  filter(hits_type == "hits") %>%
  group_by(Country) %>%
  summarise(time_lag_max_cor = time_lag[which.max(cor_level)],
            max_cor = cor_level[which.max(cor_level)])

cor_max_df %>%
  filter(max_cor > 0.2) %>%
  ggplot() +
  geom_histogram(aes(x = time_lag_max_cor),
                 binwidth=4) +
  geom_vline(xintercept = 0)


cor_max_df$time_lag_max_cor %>% hist()

