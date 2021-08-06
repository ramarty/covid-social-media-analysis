# What variables explain correlation?

# Load Data --------------------------------------------------------------------
cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "gtrends_full_timeseries",
                               "correlation_datasets",
                               "correlations_gtrends_since2020-01-01_until2021-07-31.Rds")) %>%
  dplyr::mutate(date_since = "2020-01-01")

# Prep Data --------------------------------------------------------------------
cor_df <- cor_df %>%
  dplyr::filter(type %in% "Cases") %>%
  mutate(cases_pc = cases_total / population) %>%
  dplyr::mutate(cor_nolag_5 = cor_nolag > 0.5)

cor_df$lag_adj <- cor_df$lag
cor_df$lag_adj[cor_df$lag_adj == -21] <- NA
cor_df$lag_adj[cor_df$lag_adj >= 0] <- 0
cor_df$lag_adj <- abs(cor_df$lag_adj)

# Cor Regs ---------------------------------------------------------------------
cor_smell_df <- cor_df[cor_df$keyword_en %in% "loss of smell",]
cor_taste_df <- cor_df[cor_df$keyword_en %in% "loss of taste",]

lm_1 <- lm(cor_nolag ~ log(cases_total) , data = cor_smell_df)
lm_2 <- lm(cor_nolag ~ log(cases_pc) , data = cor_smell_df) 
lm_3 <- lm(cor_nolag ~ log(population), data = cor_smell_df) 
lm_4 <- lm(cor_nolag ~ log(per_pop_using_internet), data = cor_smell_df) 
lm_5 <- lm(cor_nolag ~ log(mobile_cell_sub_per100), data = cor_smell_df) 
lm_6 <- lm(cor_nolag ~ log(gdp_pc), data = cor_smell_df) 
lm_7 <- lm(cor_nolag ~ log(population) + log(cases_pc) + log(per_pop_using_internet) + log(mobile_cell_sub_per100) + log(gdp_pc), data = cor_smell_df) 
lm_8 <- lm(cor_nolag ~ log(population) + log(cases_total) + log(per_pop_using_internet) + log(mobile_cell_sub_per100) + log(gdp_pc), data = cor_smell_df) 

stargazer(lm_1,
          lm_2,
          lm_3,
          lm_4,
          lm_5,
          lm_6,
          lm_7,
          lm_8,
          dep.var.labels.include = T,
          dep.var.labels = "Correlation between Loss of Smell and COVID Cases",
          dep.var.caption = "",
          covariate.labels = c("Total Cases",
                               "Cases Per Cap",
                               "Population",
                               "\\% Using Internet",
                               "Mobile Cell Users per 100",
                               "GDP Per Cap"),
          omit.stat = c("f","ser", "rsq"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          omit.table.layout = "n",
          out = file.path(paper_tables, 
                          "cor_lm_loss_of_smell.tex"))

# Lag Regs ---------------------------------------------------------------------
lm_1 <- lm(lag_adj ~ log(cases_total) , data = cor_smell_df)
lm_2 <- lm(lag_adj ~ log(cases_pc) , data = cor_smell_df) 
lm_3 <- lm(lag_adj ~ log(population), data = cor_smell_df) 
lm_4 <- lm(lag_adj ~ log(per_pop_using_internet), data = cor_smell_df) 
lm_5 <- lm(lag_adj ~ log(mobile_cell_sub_per100), data = cor_smell_df) 
lm_6 <- lm(lag_adj ~ log(gdp_pc), data = cor_smell_df) 
lm_7 <- lm(lag_adj ~ log(population) + log(cases_pc) + log(per_pop_using_internet) + log(mobile_cell_sub_per100) + log(gdp_pc), data = cor_smell_df) 
lm_8 <- lm(lag_adj ~ log(population) + log(cases_total) + log(per_pop_using_internet) + log(mobile_cell_sub_per100) + log(gdp_pc), data = cor_smell_df) 

summary(lm_8)

stargazer(lm_1,
          lm_2,
          lm_3,
          lm_4,
          lm_5,
          lm_6,
          lm_7,
          lm_8,
          dep.var.labels.include = T,
          dep.var.labels = "Number of days in advance that search interest in loss of smell best correlations with cases",
          dep.var.caption = "",
          covariate.labels = c("Total Cases",
                               "Cases Per Cap",
                               "Population",
                               "\\% Using Internet",
                               "Mobile Cell Users per 100",
                               "GDP Per Cap"),
          omit.stat = c("f","ser", "rsq"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          omit.table.layout = "n",
          out = file.path(paper_tables, 
                          "lag_lm_loss_of_smell.tex"))



