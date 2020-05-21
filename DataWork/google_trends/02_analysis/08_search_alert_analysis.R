# Determine how well spikes in some words act as alarms

trends_df <- readRDS(file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_admin_trends_clean.Rds"))
trends_df <- trends_df[!is.na(trends_df$region),]

#### Function to determine breakpoints
determine_breakpoint <- function(data){
  
  data <- data %>%
    arrange(date) %>%
    mutate(id = 1:n())
  
  nonfeb_ids <- data$id[data$date >= "2020-03-01"]
  
  #### Determine breakpoints, restrict to March + , take first one
  break_result <- breakpoints(hits ~ id, data=data)
  break_points <- break_result$breakpoints
  
  if(is.na(break_points)) break_points <- 1 # dummy removed in next step
  break_points <- break_points[break_points %in% nonfeb_ids] %>% min()
  
  #### Date of breakpoint
  if(is.na(break_points) | break_points %in% Inf){
    hits_break_date <- NA
    hits_break_F <- NA
    hits_break_pvalue <- NA
  } else{
    hits_break_date <- data$date[data$id %in% break_points]
    
    # F-stat and p-value of break
    chow_result <- sctest(hits ~ id, data = data,
                          type = "Chow", point = break_points)
    hits_break_F <- chow_result$statistic %>% as.numeric()
    hits_break_pvalue <- chow_result$p.value %>% as.numeric()
  }
  
  #### Cases Date
  data$cases_prop <- data$cases / max(data$cases, na.rm=T)
  
  cases1_date <- data$date[data$cases >= 1] %>% min()
  cases5_date <- data$date[data$cases >= 5] %>% min()
  cases10_date <- data$date[data$cases >= 10] %>% min()
  cases20_date <- data$date[data$cases >= 20] %>% min()
  cases50_date <- data$date[data$cases >= 50] %>% min()
  
  cases05perc_date <- data$date[data$cases_prop >= 0.05] %>% min()
  cases10perc_date <- data$date[data$cases_prop >= 0.1] %>% min()
  cases20perc_date <- data$date[data$cases_prop >= 0.2] %>% min()
  
  deaths1_date <- data$date[data$deaths >= 1] %>% min()
  deaths5_date <- data$date[data$deaths >= 5] %>% min()
  
  return(data.frame(hits_break_date = hits_break_date,
                    hits_break_F = hits_break_F,
                    hits_break_pvalue = hits_break_pvalue,
                    cases05perc_date = cases05perc_date,
                    cases10perc_date = cases10perc_date,
                    cases20perc_date = cases20perc_date,
                    
                    cases1_date = cases1_date,
                    cases5_date = cases5_date,
                    cases10_date = cases10_date,
                    cases20_date = cases20_date, 
                    cases50_date = cases50_date,
                    deaths1_date = deaths1_date, 
                    deaths5_date = deaths5_date,
                    deaths_max = max(data$deaths, na.rm=T),
                    cases_max = max(data$cases, na.rm=T)))
}

#### Determine breakpoints by state and keyword
hits_breakpoints <- trends_df %>%
  filter(keyword %in% c("febre", 
                        "tosse",
                        #"cloroquina",
                        "como tratar o coronavírus",
                        "coronavirus",
                        "estou com febre",
                        #"perda de olfato",
                        #"dor nos olhos",
                        "sintomas do coronavirus")) %>%
  group_by(state, keyword) %>%
  group_modify(~ determine_breakpoint(.x)) %>%
  mutate(hits_date_cases05perc_lag = difftime(hits_break_date, cases05perc_date, units="days") %>% as.numeric(),
         hits_date_cases10perc_lag = difftime(hits_break_date, cases10perc_date, units="days") %>% as.numeric(),
         hits_date_cases20perc_lag = difftime(hits_break_date, cases20perc_date, units="days") %>% as.numeric(),
         
         hits_date_cases1_lag = difftime(hits_break_date, cases1_date, units="days") %>% as.numeric(),
         hits_date_cases5_lag = difftime(hits_break_date, cases5_date, units="days") %>% as.numeric(),
         hits_date_cases10_lag = difftime(hits_break_date, cases10_date, units="days") %>% as.numeric(),
         hits_date_cases20_lag = difftime(hits_break_date, cases20_date, units="days") %>% as.numeric(),
         hits_date_cases50_lag = difftime(hits_break_date, cases50_date, units="days") %>% as.numeric(),
         hits_date_deaths1_lag = difftime(hits_break_date, deaths1_date, units="days") %>% as.numeric(),
         hits_date_deaths5_lag = difftime(hits_break_date, deaths5_date, units="days") %>% as.numeric())

make_figure <- function(df, keyword){
  df <- df[df$keyword %in% keyword,]
  
  df <- df %>%
    dplyr::mutate(cases_group = ifelse(cases_max > 250, "Total Cases > 250", "Total Cases < 250"))
  
  df$cases_group <- df$cases_group %>% as_factor() %>% fct_rev
  
  nobreaks_above <- sum(is.na(df$hits_break_date[df$cases_group %in% "Total Cases > 250"]))
  nobreaks_below <- sum(is.na(df$hits_break_date[df$cases_group %in% "Total Cases < 250"]))
  
  ggplot(df) + 
    geom_bar(aes(x=hits_date_cases50_lag), color="white", fill = "dodgerblue2", stat = "count") + 
    facet_wrap( ~ cases_group, ncol=1) +
    theme_minimal() +
    labs(x = "", y="Count", 
         title = keyword,
         caption = paste0("Among states with > 250 cases, there were ",
                          nobreaks_above, " states without a Google Trends breakpoint.\n",
                          "Among states with < 250 cases, there were ",
                          nobreaks_below,
                          " states without a Google Trend breakpoint."
         )) +
    geom_vline(xintercept = 0, color="red") + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
}

febre_fig <- make_figure(hits_breakpoints, "febre")
tosse_fig <- make_figure(hits_breakpoints, "tosse")
sintomas_fig <- make_figure(hits_breakpoints, "sintomas do coronavirus")
#coronavirus_fig <- make_figure(hits_breakpoints, "coronavirus")
coronavirus_fig <- make_figure(hits_breakpoints, "estou com febre")

p <- ggarrange(febre_fig,
               tosse_fig,
               sintomas_fig,
               coronavirus_fig) %>%
  annotate_figure("Days Between Google Trend Increase and State Observing 10 Cases")
ggsave(p, 
       filename = file.path(dropbox_file_path, "Data", "google_trends", "Outputs", "figures", "hitsbreak_cases_daylag.png"),
       height =10, width = 11)


#### Other figure
hits_breakpoints_lim <- hits_breakpoints %>%
  select(state, keyword, hits_break_date, cases10_date, cases50_date, cases10perc_date)

trends_fig_df <- trends_df %>%
  left_join(hits_breakpoints_lim, by=c("state", "keyword")) %>%
  group_by(state) %>%
  mutate(max_cases = max(cases, na.rm=T)) %>%
  ungroup() %>%
  mutate(state = paste0(state, "\nCases: ", max_cases))

trends_fig_df$state %>% unique()
p2 <- trends_fig_df %>%
  filter(keyword == "sintomas do coronavirus") %>%
  filter(grepl("Maranhão|Espírito Santo|Amazonas|Acre|Amapá|Ceará|Rio de Janeiro|Goiás|Paraná|Paraíba|Piauí|Rio Grande do Sul|Mato Grosso|Rondônia", state)) %>%
  group_by(state) %>%
  mutate(cases = cases/max(cases, na.rm=T)*100) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x=date, y=cases), alpha = 0.6) +
  geom_line(aes(x=date, y=hits), color="forestgreen", alpha = 0.6) +
  #geom_vline(aes(xintercept = hits_break_date), color="forestgreen",size=2,alpha=0.1) +
  geom_vline(aes(xintercept = hits_break_date), color="red",size=.8) +
  
  geom_vline(aes(xintercept = cases10_date), color="black",size=2,alpha=0.1) +
  geom_vline(aes(xintercept = cases10_date), color="black",size=.8) +
  facet_wrap( ~ state) +
  theme_minimal() +
  labs(x="", y="")

ggsave(p2, 
       filename = file.path(dropbox_file_path, "Data", "google_trends", "Outputs", "figures", "hitsbreak_cases_daylag_trend_estoucomfebre.png"),
       height =10, width = 14)


####
p2 <- trends_fig_df %>%
  filter(keyword == "febre") %>%
  filter(grepl("Maranhão|Espírito Santo|Amazonas|Acre|Amapá|Ceará|Rio de Janeiro|Goiás|Paraná|Paraíba|Piauí|Rio Grande do Sul|Mato Grosso|Rondônia", state)) %>%
  group_by(state) %>%
  mutate(cases = cases/max(cases, na.rm=T)*100) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x=date, y=cases), alpha = 0.6) +
  geom_line(aes(x=date, y=hits), color="forestgreen", alpha = 0.6) +
  #geom_vline(aes(xintercept = hits_break_date), color="forestgreen",size=2,alpha=0.1) +
  geom_vline(aes(xintercept = hits_break_date), color="red",size=.8) +
  
  geom_vline(aes(xintercept = cases10_date), color="black",size=2,alpha=0.1) +
  geom_vline(aes(xintercept = cases10_date), color="black",size=.8) +
  facet_wrap( ~ state) +
  theme_minimal() +
  labs(x="", y="")

ggsave(p2, 
       filename = file.path(dropbox_file_path, "Data", "google_trends", "Outputs", "figures", "hitsbreak_cases_daylag_trend_febre.png"),
       height =10, width = 14)
