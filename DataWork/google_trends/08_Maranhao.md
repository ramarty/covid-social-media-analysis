---
title: "08_Maranhao_Searches"
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





We use two different datasets to evaluate the evolution of searches for the state of Maranhao. 

- The first has searches for all states relative to Rio de Janeiro
- The second has searches for all states relative to their own (within a period of 3 months)

# We start by using the data that compares the states relative to Rio de Janeiro

## Maranhao in comparison to others


```r
search_df <-   search_df %>% mutate(date = as.Date(date))
```



```r
search_df %>% 
  group_by(geo, keyword, date) %>% 
  summarize(mean_hits = mean(hits, na.rm = TRUE)) %>% 
  ggplot(aes(date, mean_hits)) + 
  geom_point(aes(group = geo, color = geo)) + 
  geom_line(aes(group = geo, color = geo)) + 
  geom_point(data = . %>% filter(geo == "BR-MA"), aes(date, mean_hits, group = 1), color = "black", size = 1.5) +
  geom_line(data = . %>% filter(geo == "BR-MA"), aes(date, mean_hits, group = 1), color = "black", size = 1.5) +
  facet_wrap(vars(keyword)) + 
  labs(
    subtitle = "Maranhao is highlighted in black"
  )
```

![](08_Maranhao_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Maranhao in comparison to the average of all states


```r
search_df %>% 
  group_by(keyword, date) %>% 
  summarize(mean_hits = mean(hits, na.rm = TRUE)) %>% 
  ggplot(aes(date, mean_hits, group = 1, color = "Average of all")) + 
  geom_point() + 
  geom_line() + 
  geom_point(data = search_df %>% filter(geo == "BR-MA"), aes(date, hits, group = 1, color = "Maranhao")) +
  geom_line(data = search_df %>% filter(geo == "BR-MA"), aes(date, hits, group = 1, color = "Maranhao")) +
  facet_wrap(vars(keyword)) + 
  labs(color = "State")
```

![](08_Maranhao_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## Maranhao only


```r
search_df %>% 
  filter(geo == "BR-MA", !is.na(hits)) %>% 
  group_by(keyword, date) %>% 
  summarize(mean_hits = mean(hits, na.rm = TRUE)) %>% 
  ggplot(aes(date, mean_hits, group = 1)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(vars(keyword))
```

![](08_Maranhao_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Comparing Maranhao to Sao Paulo, Rio de Janeiro 


```r
search_df %>% 
  filter(geo %in% c("BR-MA", "BR-RJ", "BR-SP"), !is.na(hits)) %>% 
  group_by(keyword, geo, date) %>% 
  summarize(mean_hits = mean(hits, na.rm = TRUE)) %>% 
  ggplot(aes(date, mean_hits, group = 1)) + 
  geom_point(aes(group = geo, color = geo)) + 
  geom_line(aes(group = geo, color = geo)) + 
  facet_wrap(vars(keyword))
```

![](08_Maranhao_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

# Same analysis but reducing the number of words



```r
reduced_keywords <- c("como tratar o coronavírus", "Estou com falta de ar", "febre", "eu tenho coronavírus", "tosse")

search_df %>% 
  filter(keyword %in% reduced_keywords) %>% 
  group_by(geo, keyword, date) %>% 
  summarize(mean_hits = mean(hits, na.rm = TRUE)) %>% 
  ggplot(aes(date, mean_hits)) + 
  geom_point(aes(group = geo, color = geo)) + 
  geom_line(aes(group = geo, color = geo)) + 
  geom_point(data = . %>% filter(geo == "BR-MA"), aes(date, mean_hits, group = 1), color = "black", size = 1.5) +
  geom_line(data = . %>% filter(geo == "BR-MA"), aes(date, mean_hits, group = 1), color = "black", size = 1.5) +
  facet_wrap(vars(keyword)) + 
  labs(
    subtitle = "Maranhao is highlighted in black"
  )
```

![](08_Maranhao_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Maranhao in comparison to the average of all states


```r
reduced_keywords <- c("como tratar o coronavírus", "Estou com falta de ar", "febre", "eu tenho coronavírus", "tosse")

search_df %>% 
  filter(keyword %in% reduced_keywords) %>% 
  group_by(keyword, date) %>% 
  summarize(mean_hits = mean(hits, na.rm = TRUE)) %>% 
  ggplot(aes(date, mean_hits, group = 1, color = "Average of all")) + 
  geom_point() + 
  geom_line() + 
  geom_point(data = search_df %>% filter(geo == "BR-MA", keyword %in% reduced_keywords), aes(date, hits, group = 1, color = "Maranhao")) +
  geom_line(data = search_df %>% filter(geo == "BR-MA", keyword %in% reduced_keywords), aes(date, hits, group = 1, color = "Maranhao")) +
  facet_wrap(vars(keyword)) + 
  labs(color = "State")
```

![](08_Maranhao_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


## Maranhao only


```r
search_df %>% 
  filter(geo %in% c("BR-MA", "BR-RJ", "BR-SP"), !is.na(hits), keyword %in% reduced_keywords) %>% 
  group_by(keyword, geo, date) %>% 
  summarize(mean_hits = mean(hits, na.rm = TRUE)) %>% 
  ggplot(aes(date, mean_hits, group = 1)) + 
  geom_point(aes(group = geo, color = geo)) + 
  geom_line(aes(group = geo, color = geo)) + 
  facet_wrap(vars(keyword))
```

![](08_Maranhao_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## Comparing Maranhao to Sao Paulo, Rio de Janeiro 


```r
search_df %>% 
  filter(geo %in% c("BR-MA", "BR-RJ", "BR-SP"), !is.na(hits)) %>% 
  group_by(keyword, geo, date) %>% 
  summarize(mean_hits = mean(hits, na.rm = TRUE)) %>% 
  ggplot(aes(date, mean_hits, group = 1)) + 
  geom_point(aes(group = geo, color = geo)) + 
  geom_line(aes(group = geo, color = geo)) + 
  facet_wrap(vars(keyword))
```

![](08_Maranhao_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



# We now use the data for Maranhao relative to its own time trends (instead of relative to Rio de Janeiro)

Cleaning and creation of new variables

```r
trends_df <- trends_df %>% mutate(date = as.Date(date))

# We create categories for key words
trends_df <- 
  trends_df %>% 
  mutate(
    categories = case_when(
      keyword %in% 
        c("abuso", "abuso sexual", "desemprego",  "dívida", "Educação online",
          "psicologia", "terapia" ) ~ "consequences",
      keyword %in% 
        c("cama de hospital", "desinfetantes", "hidroxicloroquina", "cloroquina",
          "máscara facial", "termômetros", "ventiladores", "medicos", 
          "profissionais de saúde") ~ "resources",
      keyword %in% 
        c( "dificuldade ao respirar", "dor nos olhos", "falta de cheiro", 
           "febre", "febre alta valor","perda de olfato", "tosse",
           "sintomas do coronavirus", "teste de coronavírus") ~ "symptoms",
      keyword %in% 
        c("distância social","fique em casa", "lavar as mãos", "Isolamento social", 
          "isolamento vertical") ~ "prevention", 
      keyword %in% c("coronavirus", "covid", "ajuda do coronavírus") ~ "virus", 
      keyword %in% 
        c("como tratar o coronavírus", "Estou com falta de ar",
        "estou com febre", "Eu fico em casa", "eu tenho coronavírus",
        "Perdi o olfato", "quais são os sintomas do coronavírus", "volta brasil"
        ) ~ "in_1st_person"
      )
  )
```

## Evolution of key terms for Maranhao  

```r
trends_df %>% 
  filter(
    !is.na(state), !is.na(keyword), !is.na(hits), state == "Maranhão", 
    !(categories %in% c("consequences", "prevention"))
  ) %>% 
  group_by(date, keyword, state) %>% 
  summarize(
    mean_hits = mean(hits, na.rm = TRUE)
  ) %>% 
  ggplot() +
  geom_line(aes(date, mean_hits, group = 1)) +
  labs(
    y = "Average Number of Hits",
    title = "Average Number of Hits by Keyword Over Time (All)"
  ) +
  facet_wrap(vars(keyword)) 
```

![](08_Maranhao_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Evolution of key categories v. cases for Maranhao  


```r
trends_df %>% 
  filter(!is.na(categories), categories != "consequences", !is.na(state), state == "Maranhão") %>% 
  group_by(categories, state, date) %>% 
  mutate(
    mean_hits = mean(hits, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = categories, color = fct_reorder2(categories, date, mean_hits))) +
  geom_line(aes(date, cases)) +
  labs(color = "Category") +
  coord_cartesian(ylim = c(0, 100)) + 
  labs(subtitle = "The black line represents overall cases")
```

![](08_Maranhao_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
 
# Comparison of Maranhao with other states with a high case rate

We calculate the growth rates

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

We select key words and 8 states with largest case rate (which includes Maranhao)

```r
main_keywords <- 
  c("ajuda do coronavírus", "como tratar o coronavírus", "eu tenho coronavírus",
    "Estou com falta de ar", "estou com febre", "febre", 
    "quais são os sintomas do coronavírus", "Perdi o olfato", 
    "sintomas do coronavirus", "tosse")

top_8_case_rate <- 
  trends_df %>% 
  filter(date == "2020-04-15", is.na(keyword) | keyword == "coronavirus") %>% 
  arrange(desc(case_rate)) %>% 
  head(8) %>% pull(state)
```



```r
trends_df %>% 
  filter(!is.na(categories), keyword %in% main_keywords, !is.na(state), state %in% top_8_case_rate) %>% 
  group_by(keyword, state, week = date) %>% 
  mutate(
    mean_hits = mean(hits, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = keyword, color = fct_reorder2(keyword, date, mean_hits))) +
  geom_line(aes(date, growth_rate_cases)) + 
  labs(color = "Keyword") +
  coord_cartesian(ylim = c(0, 100)) + 
  facet_wrap(vars(state)) +
  labs(
    y = "Average hits per category", 
    title = "Growth rate of cases (in black) per state over time in comparison to Google Trends"
  )
```

![](08_Maranhao_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



```r
trends_df %>% 
  filter(!is.na(categories), keyword %in% main_keywords, !is.na(state), state == "Maranhão") %>% 
  group_by(keyword, state, date) %>% 
  mutate(
    mean_hits = mean(hits, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = keyword, color = fct_reorder2(keyword, date, mean_hits))) +
  geom_line(aes(date, growth_rate_cases)) + 
  labs(color = "Keyword") +
  coord_cartesian(ylim = c(0, 100)) + 
  facet_wrap(vars(state)) +
  labs(
    y = "Average hits per category", 
    title = "Growth rate of cases (in black) per state over time in comparison to Google Trends"
  )
```

![](08_Maranhao_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

At the weekly level


```r
trends_df %>% 
  filter(!is.na(categories), keyword %in% main_keywords, !is.na(state), state %in% top_8_case_rate) %>% 
  mutate(week_number = week(date)) %>% 
  group_by(keyword, state, week_number) %>% 
  mutate(
    mean_hits = mean(hits, na.rm = TRUE)
  ) %>% 
  ggplot() + 
  geom_line(aes(date, mean_hits, group = keyword, color = fct_reorder2(keyword, date, mean_hits))) +
  geom_line(aes(date, growth_rate_cases)) + 
  labs(color = "Keyword") +
  coord_cartesian(ylim = c(0, 100)) + 
  facet_wrap(vars(state)) +
  labs(
    y = "Average hits per category", 
    title = "Growth rate of cases (in black) per state over time in comparison to Google Trends"
  )
```

![](08_Maranhao_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
