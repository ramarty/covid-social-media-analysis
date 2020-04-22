# Determine how well spikes in some words act as alarms

trends_df <- readRDS(file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_admin_trends_clean.Rds"))
trends_df <- trends_df[!is.na(trends_df$region),]
trends_df$date_num <- trends_df$date %>% as.numeric()

#### Function to determine breakpoints
determine_breakpoint <- function(data){
  out <- maxstat_test(hits ~ date_num, data = data, dist = "approx")
  
  break_day <- out@estimates[[1]][1] %>% as.numeric()
  break_day_tstat <- out@statistic@teststatistic
  
  cases10_date <- min(data$date_num[data$hits >= 10], na.rm=T)
  
  return(data.frame(break_day=break_day,
                    break_day_tstat=break_day_tstat,
                    cases10_date = cases10_date))
}

#### Determine breakpoints by state and keyword
hits_breakpoints <- trends_df %>%
  filter(keyword %in% c("febre", "tosse")) %>%
  mutate(date_num = date %>% as.numeric) %>%
  arrange(date_num) %>%
  group_by(state, keyword) %>%
  group_modify(~ determine_breakpoint(.x)) %>%
  mutate(hits_date_cases10_lag = break_day - cases10_date)
  

hits_breakpoints[hits_breakpoints$state %in% "São Paulo",]

trends_df %>%
  filter(state == "São Paulo") %>%
  filter(keyword == "febre") %>%
  ggplot() +
  #geom_line(aes(x=date, y=cases)) +
  geom_line(aes(x=date_num, y=hits), color = "green") + 
  geom_vline(aes(xintercept = 18291))

  y <- a$hits
breakpoints(y ~ 1)
b <- breakpoints( y ~ 1 )
f <- Fstats( y ~ 1 )
plot(b)  # 2 breakpoints
plot(f)  # Only 1 F-statistic above the threshold
lines(b)
  library(strucchange)
  



days_bfr_df
a <- days_bfr_df %>%
  filter(keyword == "febre") %>%
  filter(state == "Distrito Federal")



a$date[a$zscore_high_lag_avg >= 3 & a$month > 2] %>% min(na.rm=T)
days_bfr_df$state %>% unique()


#### Only Case about Z-score in March
dplyr::mutate(hits_zscore = ifelse(month == 2, 0, hits_zscore)) %>%
  
  
  
  dplyr::summarise(date_cases = min(date[cases > 30], na.rm=T),
                   date_keyword = min(date[hits > 90], na.rm=T)) %>%
  
  #### Number of days keyword spikes compared to coronavirus
  mutate(diff_days = difftime(date_keyword, date_cases, units="days") %>% as.numeric()) %>%
  filter(!is.na(diff_days)) %>%
  filter(!(diff_days %in% c(-Inf, Inf)))

days_bfr_sum <- days_bfr_df %>%
  group_by(keyword) %>%
  summarise(min = min(diff_days),
            mean = mean(diff_days),
            median = median(diff_days),
            max = max(diff_days),
            sd = sd(diff_days),
            prop_neg = mean(diff_days < 0),
            N = n())



trends_df %>%
  filter(state == "São Paulo") %>%
  filter(keyword == "tosse") %>%
  ggplot() +
  #geom_line(aes(x=date, y=cases)) +
  geom_line(aes(x=date_num, y=hits), color = "green")

trends_df$keyword %>% table %>% View()
trends_df$state %>% unique()


days_bfr_sum


trends_sum_df[trends_sum_df$keyword %in% "febre",]


a <- trends_df[trends_df$state %in% "Alagoas",]



a <- trends_df %>%
  group_by(region, state) %>%
  summarise(case_max = max(cases))

trends_df$cases %>% summary()


trends_df$date %>% str()





head(trends_df)


