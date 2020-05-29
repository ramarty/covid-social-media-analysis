---
title: "06_Time_Lags"
author: Manuel Ramos
output: 
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true
    code_folding: hide
    toc_fold: TRUE
    highlight: tango
    keep_md: yes
    theme: cosmo
    number_sections: true
---






```r
trends_df <- trends_df %>% mutate(date = as.Date(date))
```

# Goal

The goal of this analysis is to understand whether Google searches of specific keyords can help predict or anticipate the spike in number of cases or deaths due to covid-19. 

# Overall picture across keyword

Hits for all keywords across all states


```r
trends_df %>% 
  filter(!is.na(state), !is.na(keyword), !is.na(hits)) %>% 
  group_by(date, keyword, state) %>% 
  summarize(
    weighted_mean_hits = weighted.mean(hits, w = estimate_2018_state)
  ) %>% 
  ggplot() +
  geom_line(aes(date, weighted_mean_hits, group = state, color = state)) +
  labs(
    y = "Weighted Average Number of Hits",
    title = "Average Number of Hits by Keyword Over Time (Symptoms)", 
    subtitle = "The average is weighted by the number of people in each state"
  ) +
  facet_wrap(vars(keyword))
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

# Selecting main keywords 

Based on the graph above, we also select the main keywords. We will focus on these keywords in the analysis. 

We also want to exclude words which have missing values for multiple states. In order to do so, we restrict to keywords with at least 1500 observations. 



```r
# we select a group of keywords
selected_keywords <- 
  c("ajuda do coronavírus", "cloroquina", "como tratar o coronavírus", 
    "coronavirus", "covid", 
    "Estou com falta de ar", "estou com febre", "febre", 
    "fique em casa", "medicos", "quais são os sintomas do coronavírus", 
    "sintomas do coronavirus", "tosse")

main_words_df <- 
  trends_df %>% 
  filter(keyword %in% selected_keywords)

# we now filter for those keywords which appear in a majority of states

main_keywords <- 
  main_words_df %>% 
  count(keyword) %>% 
  filter(n > 1500) %>% pull(keyword)

main_words_df <- 
  trends_df %>% 
  filter(keyword %in% main_keywords)
```

The words "sintomas do coronavirus" and "tosse" seem to have sufficient variation yet a clear, increasing trend as well,.

# Computing growth rates of cases and deaths

We compute the growth rate for cases and deaths of coronavirus


```r
trends_df <- 
  trends_df %>% 
  group_by(state, keyword) %>% 
  mutate(
    diff_date = as.numeric(date - lag(date)), 
    diff_growth_cases = cases - lag(cases), 
    diff_growth_deaths = deaths - lag(deaths), 
    diff_growth_hits = hits - lag(hits), 
    growth_rate_cases = (diff_growth_cases/diff_date)*100/lag(cases),
    growth_rate_deaths = (diff_growth_deaths/diff_date)*100/lag(deaths),
    growth_rate_hits = (diff_growth_hits/diff_date)*100/lag(hits), 
    growth_rate_cases = if_else(is.na(growth_rate_cases), 0, growth_rate_cases), 
    growth_rate_deaths = if_else(is.na(growth_rate_deaths), 0, growth_rate_deaths), 
    growth_rate_cases = if_else(is.infinite(growth_rate_cases), 100, growth_rate_cases), 
    growth_rate_deaths = if_else(is.infinite(growth_rate_deaths), 100, growth_rate_deaths), 
    growth_rate_hits = if_else(is.na(growth_rate_hits), 0, growth_rate_hits), 
    growth_rate_hits = if_else(is.infinite(growth_rate_hits), 100, growth_rate_hits)
  ) %>% 
  ungroup()
```

We check how the case rate looks like


```r
trends_df %>% 
  filter(!is.na(categories), keyword %in% main_keywords, !is.na(state)) %>% 
  group_by(categories, state, date) %>% 
  mutate(
    mean_hits = mean(hits, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = categories, color = fct_reorder2(categories, date, mean_hits))) +
  geom_line(aes(date, growth_rate_cases)) + 
  labs(color = "Category") +
  coord_cartesian(ylim = c(0, 100)) + 
  facet_wrap(vars(state)) +
  labs(
    y = "Average hits per category", 
    title = "Growth rate of cases (in black) per state over time in comparison to Google Trends"
  )
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

We check how the death growth rate looks like


```r
trends_df %>% 
  filter(!is.na(categories), keyword %in% main_keywords, !is.na(state)) %>% 
  group_by(categories, state, date) %>% 
  mutate(
    mean_hits = mean(hits, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = categories, color = fct_reorder2(categories, date, mean_hits))) +
  geom_line(aes(date, growth_rate_deaths)) + 
  labs(color = "Category") +
  coord_cartesian(ylim = c(0, 100)) + 
  facet_wrap(vars(state)) +
  labs(
    y = "Average hits per category", 
    title = "Growth rate of deaths (in black) per state over time in comparison to Google Trends"
  )
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Reducing number of states (1:10)


```r
first_10_states <- 
  trends_df %>% 
  count(state) %>% 
  head(10) %>% pull(state)

trends_df %>% 
  filter(!is.na(categories), keyword %in% main_keywords, !is.na(state), state %in% first_10_states, categories %in% c("virus", "symptoms", "in_1st_person")) %>% 
  group_by(categories, state, date) %>% 
  mutate(
    mean_hits = mean(hits, na.rm = TRUE), 
    mean_growth_rate_cases = mean(growth_rate_cases, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = categories, color = fct_reorder2(categories, date, mean_hits))) +
  geom_line(aes(date, mean_growth_rate_cases)) + 
  labs(color = "Category") +
  coord_cartesian(ylim = c(0, 100)) + 
  facet_wrap(vars(state)) +
  labs(
    y = "Average hits per category", 
    title = "Growth rate of deaths (in black) per state over time in comparison to Google Trends"
  ) + 
  theme_light()
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
trends_df %>% 
  filter(!is.na(categories), keyword %in% main_keywords, !is.na(state), state %in% first_10_states, categories %in% c("resources", "prevention")) %>% 
  group_by(categories, state, date) %>% 
  mutate(
    mean_hits = mean(hits, na.rm = TRUE), 
    mean_growth_rate_cases = mean(growth_rate_cases, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = categories, color = fct_reorder2(categories, date, mean_hits))) +
  geom_line(aes(date, mean_growth_rate_cases)) + 
  labs(color = "Category") +
  coord_cartesian(ylim = c(0, 100)) + 
  facet_wrap(vars(state)) +
  labs(
    y = "Average hits per category", 
    title = "Growth rate of deaths (in black) per state over time in comparison to Google Trends"
  ) + 
  theme_light()
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


## Average of all states

### For symptoms, virus and 1st person


```r
trends_df %>% 
  filter(!is.na(categories), categories %in% c("virus", "symptoms", "in_1st_person"), keyword %in% main_keywords, !is.na(state)) %>% 
  group_by(categories, date) %>% 
  summarize(
    mean_hits = mean(hits, na.rm = TRUE), 
    mean_growth_rate_cases = mean(growth_rate_cases)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = categories, color = fct_reorder2(categories, date, mean_hits)), size = 1.2) +
  geom_line(data = . %>% filter(categories == "in_1st_person"), aes(date, mean_growth_rate_cases, group = 1), size = 1.1) + 
  labs(
    y = "Average hits per category", 
    x = "Date",
    title = "Average growth rate of cases (in black)\nin comparison to Google Trends over time"
  ) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_color_discrete(name = "Search Category", labels = c("Coronavirus", "Symptoms", "1st Person Search")) +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.55)) 
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


## For prevention and resources

```r
trends_df %>% 
  filter(!is.na(categories), categories %in% c("prevention", "resources"), keyword %in% main_keywords, !is.na(state)) %>% 
  group_by(categories, date) %>% 
  summarize(
    mean_hits = mean(hits, na.rm = TRUE), 
    mean_growth_rate_cases = mean(growth_rate_cases)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = categories, color = fct_reorder2(categories, date, mean_hits))) +
  geom_line(aes(date, mean_growth_rate_cases, group = 1)) + 
  labs(color = "Category") +
  labs(
    y = "Average hits per category", 
    title = "Average growth rate of cases (in black) in comparison to Google Trends over time"
  ) + 
  coord_cartesian(ylim = c(0, 100)) +
  theme_light()
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

## Using cases rather than growth rate of cases


```r
trends_df %>% 
  filter(!is.na(categories), categories %in% c("virus", "symptoms", "in_1st_person"), keyword %in% main_keywords, !is.na(state)) %>% 
  group_by(categories, date) %>% 
  summarize(
    mean_hits = mean(hits, na.rm = TRUE), 
    mean_cases = mean(cases)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = categories, color = fct_reorder2(categories, date, mean_hits))) +
  geom_line(data = . %>% filter(categories == "in_1st_person"), aes(date, mean_cases, group = 1)) + 
  labs(color = "Category") +
  labs(
    y = "Average hits per category", 
    title = "Average number of new cases (in black) in comparison to Google Trends over time"
  ) + 
  theme_light()
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


# Time lags

We now compute the difference between these keywords and the maximum in searches

We first create variables for time periods  

```r
trends_df <- 
  trends_df %>% 
  mutate(
    month = month(date), 
    wday = wday(date, label = TRUE), 
    week_number = week(date)
  )
```

We compute the z-score for hits

```r
trends_df <- 
  trends_df %>%
   group_by(state, keyword) %>%
   mutate(
     hits_feb = mean(hits[month == 2], na.rm=T),
     hits_feb_sd = sd(hits[month == 2], na.rm=T)
   ) %>%
   ungroup()
    
min_sd <- min(trends_df$hits_feb_sd[trends_df$hits_feb_sd > 0], na.rm=T)

trends_df$hits_feb_sd[trends_df$hits_feb_sd < min_sd] <- min_sd
    
trends_df$zscore <- (trends_df$hits - trends_df$hits_feb) / trends_df$hits_feb_sd
```

Compute weekly average of zscore for hits, then find the week with the maximum z-score

```r
trends_df <- 
  trends_df %>% 
  group_by(week_number, state, keyword) %>% 
  mutate(week_zscore = mean(zscore, na.rm = TRUE)) %>% 
  ungroup()

trends_df <- 
  trends_df %>% 
  group_by(state, keyword) %>% 
  mutate(
    max_week_zscore = max(week_zscore), 
    max_week_zscore_bin = if_else(max_week_zscore == week_zscore, 1, 0)
  ) 

# need to review later if this worked well
```

Once we have the maximum z-score, a key question is how to calculate the period of maximum or enough number of cases. 

## Using max growth rate in the number of cases

we calculate the days that pass between when the z-score and the maximum growth rate of cases for the main keyword per state. We compute the period of maximum growth rate for each state and keyword


```r
trends_df <- 
  trends_df %>% 
  group_by(state, keyword) %>% 
  mutate(
    max_growth_cases = max(growth_rate_cases), 
    max_growth_cases_bin = if_else(growth_rate_cases == max_growth_cases, 1, 0)
  )
```

We now compute the difference between the week of maximum z-score and the week of maximum case growth rate. However, this might not be very reliable because the first number of cases tend to have the largest growth rate due to the fact that the baseline of cases is 0. 


```r
#trends_df <- 
#  trends_df %>% 
#  group_by(keyword, state) %>% 
#  mutate(days_since_max_zscore = date - date[max_week_zscore_bin == 1]) %>% 
#  ungroup()
```

We plot a histogram of the difference in number of days between large increase in hits v. covid-19 cases


```r
trends_df %>% 
  filter(keyword %in% main_keywords) %>% 
  group_by(state, keyword) %>% 
  summarize(
    date_max_week_zscore = mean(date[max_week_zscore_bin == 1]), 
    date_max_growth_cases = mean(date[max_growth_cases_bin == 1]), 
    date_cases_minus_hits = date_max_week_zscore - date_max_growth_cases 
  ) %>% 
  ggplot() + 
  geom_histogram(aes(date_cases_minus_hits)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") + 
  facet_wrap(vars(keyword)) +
  labs(
    x = "Date of max hits - date of max growth in cases", 
    y = "Number of states in Brazil",
    title = "Difference in number of days between sudden increase in hits and covid-19 cases", 
    subtitle = "In most states, the peak in Google searches comes around \na similar time or even later than the increase in cases", 
    caption = "In this case, we used an early date for the increase in number of cases"
  )
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


We now switch the approach to a simpler one, and potentially more reliable one. 

We create a variable for the date when states hit more than 50 cases

```r
beyond_50_dates <- 
  trends_df %>% 
  mutate(more_50_cases = if_else(cases >= 50L, 1, 0)) %>%
  filter(more_50_cases == 1L) %>% 
  group_by(state) %>% 
  slice(1) %>% 
  mutate(date_beyond_50 = "Just_Beyond_50") %>% 
  ungroup()

beyond_50_dates <- 
  beyond_50_dates %>% 
  dplyr::select(date, region, state, cases, date_beyond_50)

beyond_100_dates <- 
  trends_df %>% 
  mutate(more_100_cases = if_else(cases >= 100L, 1, 0)) %>%
  filter(more_100_cases == 1L) %>% 
  group_by(state) %>% 
  slice(1) %>% 
  mutate(date_beyond_100 = "Just_Beyond_100") %>% 
  ungroup()

beyond_100_dates <- 
  beyond_100_dates %>% 
  dplyr::select(date, region, state, cases, date_beyond_100)

beyond_10_deaths_dates <- 
  trends_df %>% 
  mutate(more_10_deaths = if_else(deaths >= 10L, 1, 0)) %>%
  filter(more_10_deaths == 1L) %>% 
  group_by(state) %>% 
  slice(1) %>% 
  mutate(date_beyond_10_deaths = "Just_Beyond_10_deaths") %>% 
  ungroup()

beyond_10_deaths_dates <- 
  beyond_10_deaths_dates %>% 
  dplyr::select(date, region, state, cases, date_beyond_10_deaths)
```

## Graph of hits v. 50 cases 

We compute the difference in days between the date with the largest zscore (for hits) and the date with more than 50 cases


```r
trends_df %>% 
  filter(keyword %in% main_keywords) %>% 
  group_by(state, keyword) %>% 
  summarize(
    date_max_week_zscore = mean(date[max_week_zscore_bin == 1])
  ) %>% 
  left_join(beyond_50_dates %>% rename(date_50_cases = date), by = "state") %>% 
  mutate(date_cases_minus_hits = date_max_week_zscore - date_50_cases) %>% 
  ggplot() + 
  geom_histogram(aes(date_cases_minus_hits)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") + 
  facet_wrap(vars(keyword)) +
  labs(
    x = "Date of max hits - date after surpassing 50 cases", 
    y = "Number of states in Brazil",
    title = "Difference in number of days between sudden increase in hits and covid-19 cases", 
    subtitle = "In most states, the peak in Google searches comes around \na similar time or even later than the increase in cases", 
    caption = "In this case, we used an early date for the increase in number of cases"
  )
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

## Graph of hits v. 100 cases 

We compute the difference in days between the date with the largest zscore (for hits) and the date with more than 100 cases


```r
trends_df %>% 
  filter(keyword %in% main_keywords) %>% 
  group_by(state, keyword) %>% 
  summarize(
    date_max_week_zscore = mean(date[max_week_zscore_bin == 1])
  ) %>% 
  left_join(beyond_100_dates %>% rename(date_100_cases = date), by = "state") %>% 
  mutate(date_cases_minus_hits = date_max_week_zscore - date_100_cases) %>% 
  ggplot() + 
  geom_histogram(aes(date_cases_minus_hits)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") + 
  facet_wrap(vars(keyword)) +
  labs(
    x = "Date of max hits - date after surpassing 100 cases", 
    y = "Number of states in Brazil",
    title = "Difference in number of days between sudden increase in hits and covid-19 cases", 
    subtitle = "In most states, the peak in Google searches comes around \na similar time or right before the increase in cases"
  )
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-21-1.png)<!-- -->


With a focus on key words


```r
trends_df %>% 
  filter(keyword %in% c("febre", "tosse", "como tratar o coronavírus", "Estou com falta de ar", "eu tenho coronavírus")) %>% 
  group_by(state, keyword) %>% 
  summarize(
    date_max_week_zscore = mean(date[max_week_zscore_bin == 1])
  ) %>% 
  left_join(beyond_100_dates %>% rename(date_100_cases = date), by = "state") %>% 
  mutate(date_cases_minus_hits = date_max_week_zscore - date_100_cases) %>% 
  ggplot() + 
  geom_histogram(aes(date_cases_minus_hits)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") + 
  facet_wrap(vars(keyword)) +
  labs(
    x = "Date of max hits - date after surpassing 100 cases", 
    y = "Number of states in Brazil",
    title = "Difference in number of days between sudden increase in hits and covid-19 cases", 
    subtitle = "In most states, the peak in Google searches comes around \na similar time or right before the increase in cases"
  )
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

## Graph of hits v. 10 deaths 

We compute the difference in days between the date with the largest zscore (for hits) and the date with more than 10 deaths


```r
trends_df %>% 
  filter(keyword %in% c("febre", "tosse", "como tratar o coronavírus", "Perdi o olfato")) %>% 
  group_by(state, keyword) %>% 
  summarize(
    date_max_week_zscore = mean(date[max_week_zscore_bin == 1])
  ) %>% 
  left_join(beyond_10_deaths_dates %>% rename(date_beyond_10 = date), by = "state") %>% 
  mutate(date_cases_minus_hits = date_max_week_zscore - date_beyond_10) %>% 
  ggplot() + 
  geom_histogram(aes(date_cases_minus_hits)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") + 
  facet_wrap(vars(keyword)) +
  labs(
    x = "Date of max hits - date after surpassing 10 deaths", 
    y = "Number of states in Brazil",
    title = "Difference in number of days between sudden increase in hits and covid-19 deaths"   )
```

![](06_Time_Lags_files/figure-html/unnamed-chunk-23-1.png)<!-- -->
