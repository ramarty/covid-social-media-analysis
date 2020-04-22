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

![](08_Maranhao_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

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

![](08_Maranhao_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
 