---
title: "05_Analysis_With_Refstate"
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




## GOAL

This script performs analysis of the google trends data for Brazil to develop an alarm system for covid19 cases at the state level.


```r
#user's file path
if(Sys.info()[["user"]] == "wb537287") dropbox_file_path <- "/Users/wb537287/Dropbox/COVID Social Media Analysis/"

#import data
trends_df <- 
  readRDS(file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_with_refstate_analysis.Rds"))

library(ggrepel)
library(tidyverse)
library(parallel)
library(pbmcapply)
library(ggplot2)
library(jsonlite)
library(stringr)
library(raster)
library(ISOcodes)
library(stringi)
library(lubridate)
library(purrr)
library(tidytext)
library(quanteda)
library(SentimentAnalysis)
library(sentimentr)
library(tm)
library(tokenizers)
library(wordcloud)
library(ggwordcloud)
library(ggpubr)
library(dplyr)
library(sf)
library(readstata13)
library(forcats)
library(tidyr)
library(tidylog)
```

Adjusting the dataset

```r
#renaming state variable name to "state" so that it matches previous code
trends_df <- 
  trends_df %>% 
  rename(state = name)
```


# Covid-19 per state

Total number of cases across states

```r
trends_df %>% 
  filter(!is.na(state), !is.na(cases)) %>% 
  count(cases, date, state) %>% 
  ggplot() +
  geom_line(aes(date, cases, group = state, color = fct_reorder2(state, date, cases))) +
  labs(
    title = "Cases per State",
    color = "State"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Total number of deaths across states


```r
trends_df %>% 
  filter(!is.na(state), !is.na(cases)) %>% 
  count(deaths, date, state) %>% 
  ggplot() +
  geom_line(aes(date, deaths, group = state, color = fct_reorder2(state, date, deaths))) +
  labs(
    title = "Deaths per State",
    color = "State"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Case rate per state

```r
trends_df %>% 
  filter(!is.na(state), !is.na(case_rate)) %>% 
  count(case_rate, date, state) %>% 
  ggplot() +
  geom_line(aes(date, case_rate, group = state, color = fct_reorder2(state, date, case_rate))) +
  labs(
    title = "Case rate per State",
    color = "State"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Death rate per state

```r
trends_df %>% 
  filter(!is.na(state), !is.na(death_rate)) %>% 
  count(death_rate, date, state) %>% 
  ggplot() +
  geom_line(aes(date, death_rate, group = state, color = fct_reorder2(state, date, death_rate))) +
  labs(
    title = "Death rate per State",
    color = "State"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

# Overall evolution of searches v. trends

```r
main_keywords <- c("como tratar o coronavírus", "febre", "tosse")

trends_df %>% 
  filter(!is.na(categories), categories %in% c("virus", "symptoms", "in_1st_person"), !is.na(state)) %>% 
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
    title = "Average growth rate of cases (in black)\nin comparison to Google Trends over time", 
    caption = "Using hits - not relative to another state"
  ) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_color_discrete(name = "Search Category", labels = c("Coronavirus", "Symptoms", "1st Person Search")) +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.55)) 
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

# Correlation between average of main keywords and case rate

We now explore if the average of tosse, febre and como tratar o coronavirus is a better predictor of cases


```r
main_keywords <- c("como tratar o coronavírus", "febre", "tosse")

trends_df %>% 
  filter(categories %in% c("in_1st_person", "symptoms"), date > "2020-02-29") %>% 
  group_by(state, week_number) %>% 
  summarize(
    mean_hits = mean(hits_adj, na.rm = TRUE), 
    case_rate = mean(case_rate, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_point(aes(case_rate, mean_hits)) + 
  geom_smooth(aes(case_rate, mean_hits), method = "lm") + 
  geom_text_repel(
    data = . %>% filter(mean_hits > 75), 
    aes(case_rate, mean_hits, label = state), 
    hjust=0.5, vjust=0.4
  ) + 
  facet_wrap(vars(week_number), scales = "free_x") +
  labs(
    caption = "Keywords used: 'febre', 'tosse', and 'como tratar o coronavirus'; Using hits_adj"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Correlation between average of 3 main keywords and case rate


```r
main_keywords <- c("como tratar o coronavírus", "febre", "tosse")

trends_df %>%
  filter(!is.na(hits), keyword %in% main_keywords) %>% 
  group_by(state) %>%
  summarize(
    average_hits = mean(hits_adj, na.rm = TRUE), 
    case_rate = mean(case_rate, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_point(aes(case_rate, average_hits)) +
  geom_smooth(aes(case_rate, average_hits), method = "lm") +
  geom_label_repel(
    data = . %>% filter(case_rate > 500 | average_hits > 70), 
    aes(case_rate, average_hits, label = state), 
    hjust=0.5, vjust=0.4
  ) + 
  theme_light()
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


# Focus on "I can't smell" = "Perdi o olfato"  

## Case rate against searches for I can't smell


```r
trends_df %>% 
  filter(keyword == "perdi o olfato", date > "2020-02-29") %>% 
  group_by(state, week_number) %>% 
  summarize(
    mean_hits = mean(hits_adj, na.rm = TRUE), 
    case_rate = mean(case_rate, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_point(aes(case_rate, mean_hits)) + 
  geom_smooth(aes(case_rate, mean_hits), method = "lm") + 
  geom_text_repel(
    data = . %>% filter(mean_hits > 1), 
    aes(case_rate, mean_hits, label = state), 
    hjust=0.5, vjust=0.4
  ) + 
  facet_wrap(vars(week_number), scales = "free_x") +
  labs(
    caption = "Keywords used: 'I can't smell'; Using hits_adj"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Death rate against searches for I can't smell


```r
trends_df %>% 
  filter(!is.na(hits), keyword == "Perdi o olfato") %>% 
  ggplot() + 
  geom_point(aes(death_rate, hits)) +
  geom_smooth(aes(death_rate, hits), method = "lm") + 
  ggrepel::geom_label_repel(aes(death_rate, hits, label = state), hjust=0.5, vjust=0.4) +
  coord_cartesian(ylim = c(0, 100)) + 
  theme_light()
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



# Case rate and death rate per state

Find states that show up under “I can’t smell”


```r
states_cant_smell <-  
  trends_df %>% 
  filter(keyword == "perdi o olfato", hits_adj > 0) %>% 
  filter(!is.na(hits_adj)) %>% 
  count(state) %>%
  pull(state)
```

Find the date in which the state first shows under “I can’t smell”


```r
states_cant_smell_date <- 
  trends_df %>% 
  filter(keyword == "perdi o olfato", state %in% states_cant_smell, hits_adj > 0) %>% 
  filter(!is.na(hits_adj), date> "2020-03-01") %>% 
  group_by(state) %>% 
  summarize(
    first_date_cant_smell = min(date)
  )
```


## Case rate: Highlighting the states that show up under I can't smell at any point

```r
trends_df %>% 
  filter(!is.na(state), !is.na(case_rate)) %>% 
  count(case_rate, date, state) %>% 
  ggplot() +
  geom_line(aes(date, case_rate, group = state, color = fct_reorder2(state, date, case_rate))) +
  geom_line(
    data = . %>% filter(state %in% states_cant_smell),
    aes(date, case_rate, group = state, color = fct_reorder2(state, date, case_rate)), size = 2) +
  labs(
    title = "Case rate per State",
    color = "State"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

## Death rate: Highlighting the states that show up under I can't smell at any point

```r
trends_df %>% 
  filter(!is.na(state), !is.na(death_rate)) %>% 
  count(death_rate, date, state) %>% 
  ggplot() +
  geom_line(aes(date, death_rate, group = state, color = fct_reorder2(state, date, death_rate))) +
  geom_line(
    data = . %>% filter(state %in% states_cant_smell),
    aes(date, death_rate, group = state, color = fct_reorder2(state, date, death_rate)), size = 2) +
  labs(
    title = "Death rate per State",
    color = "State"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

## Cases: Highlighting the states that show up under I can’t smell at any point

```r
trends_df %>% 
  filter(!is.na(state), !is.na(cases)) %>% 
  count(cases, date, state) %>% 
  ggplot() +
  geom_line(aes(date, cases, group = state, color = fct_reorder2(state, date, cases))) +
  geom_line(
    data = . %>% filter(state %in% states_cant_smell),
    aes(date, cases, group = state, color = fct_reorder2(state, date, cases)), size = 2) +
  labs(
    title = "Cases per State",
    color = "State"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


## Date of showing up "I can't smell"


```r
trends_df %>% 
  filter(!is.na(state), !is.na(death_rate), state %in% states_cant_smell) %>% 
  count(death_rate, date, state) %>% 
  ggplot() +
  geom_line(aes(date, death_rate, group = state, color = fct_reorder2(state, date, death_rate))) +
  geom_vline(
    data = states_cant_smell_date, 
    aes(xintercept = first_date_cant_smell, group = state, color = state)
  ) +
  geom_label_repel(
    data = states_cant_smell_date, 
    aes(first_date_cant_smell, y = 80, label = state), 
    hjust=0.5, vjust=0.4
  ) + 
  labs(
    title = "Death rate per State",
    color = "State"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


## Graph of current death rate distinguishing by date of I can’t smell


```r
trends_df %>% 
  left_join(states_cant_smell_date, by = "state") %>% 
  mutate(cant_smell_appears = if_else(!is.na(first_date_cant_smell), 1L, 0L) %>% as.character()) %>% 
  filter(date == "2020-06-18", !is.na(death_rate)) %>% 
  count(deaths, state, cant_smell_appears, first_date_cant_smell) %>% 
  arrange(desc(deaths)) %>% 
  ggplot()+ 
  geom_col(aes(fct_reorder(state, deaths), deaths, fill = cant_smell_appears)) +
  geom_label_repel(
    aes(
      x = as.character(state), 
      y = deaths, 
      label = as.character(first_date_cant_smell)
    ), 
    position = position_fill(vjust = 0)
  ) + 
  labs(
    color = "State category"
  ) + 
  coord_flip() + 
  labs(
    title = "States ordered by deaths on June 18th, 2020", 
    x = "State", 
    y = "Deaths"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
trends_df %>% 
  left_join(states_cant_smell_date, by = "state") %>% 
  mutate(cant_smell_appears = if_else(!is.na(first_date_cant_smell), 1L, 0L) %>% as.character()) %>% 
  filter(date == "2020-06-18", !is.na(death_rate)) %>% 
  count(death_rate, state, cant_smell_appears, first_date_cant_smell) %>% 
  arrange(desc(death_rate)) %>% 
  ggplot()+ 
  geom_col(aes(fct_reorder(state, death_rate), death_rate, fill = cant_smell_appears)) +
  geom_label_repel(
    aes(
      x = as.character(state), 
      y = death_rate, 
      label = as.character(first_date_cant_smell)
    ),  
    position = position_fill(vjust = 0.5)
  ) + 
  coord_flip() + 
  labs(
    title = "States ordered by death rate on June 18th, 2020", 
    x = "State", 
    y = "Deaths per 100,000 people"
  )
```

![](05_analysis_with_restat_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

