---
title: "07_Cross_state_Analysis"
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
library(tidyverse)
library(gridExtra)
library(lubridate)

if(Sys.info()[["user"]] == "wb537287") dropbox_file_path <- "/Users/wb537287/Dropbox/COVID Social Media Analysis"

states_df <- read.csv(file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_crosstates_clean.csv"))
```

We focus on the latest date of cases that we have, April 18. The trends data was extracted in April 20, so this is close enough. 


```r
states_df_0418 <- 
  states_df %>% 
  filter(date == "2020-04-18") 

selected_keywords <- 
  c("ajuda do coronavírus", "cloroquina", "como tratar o coronavírus", 
    "coronavirus", "covid", 
    "Estou com falta de ar", "estou com febre", "febre", 
    "fique em casa", "medicos", "quais são os sintomas do coronavírus", 
    "sintomas do coronavirus", "tosse")


states_df_0418 %>%
  filter(!is.na(hits), keyword %in% selected_keywords) %>% 
  ggplot() + 
  geom_point(aes(case_rate, hits)) +
  geom_smooth(aes(case_rate, hits), method = "lm") + 
  facet_wrap(vars(keyword)) + 
  coord_cartesian(ylim = c(0, 100))
```

![](07_Cross_State_Analysis_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



```r
states_df_0418 %>%
  filter(!is.na(hits), keyword %in% selected_keywords) %>% 
  ggplot() + 
  geom_point(aes(death_rate, hits)) +
  geom_smooth(aes(death_rate, hits), method = "lm") + 
  facet_wrap(vars(keyword)) +
  coord_cartesian(ylim = c(0, 100)) 
```

![](07_Cross_State_Analysis_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

