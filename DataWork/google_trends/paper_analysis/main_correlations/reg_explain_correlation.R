# Example Trends

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                "correlations_gtrends_since2020-01-01_until2021-07-31.Rds"))

gtrends_df$income %>% table()
gtrends_losssmell_df <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "loss of smell",
                type %in% "Cases") %>%
  mutate(income = income %>% factor(levels = c("Low income",
                                               "Lower middle income",
                                               "Upper middle income",
                                               "High income")))

lm1 <- lm(cor_nolag ~ log(cases_total), data = gtrends_losssmell_df)
lm2 <- lm(cor_nolag ~ per_pop_using_internet, data = gtrends_losssmell_df)
lm3 <- lm(cor_nolag ~ mobile_cell_sub_per100, data = gtrends_losssmell_df)
lm4 <- lm(cor_nolag ~ factor(income), data = gtrends_losssmell_df)
lm5 <- lm(cor_nolag ~ log(cases_total) + per_pop_using_internet + mobile_cell_sub_per100 + factor(income), data = gtrends_losssmell_df)

stargazer(lm1,
          lm2,
          lm3,
          lm4,
          lm5,
          dep.var.labels.include = F,
          dep.var.caption = "Correlation between loss of smell search interest and COVID-19",
          covariate.labels = c("Total COVID-19 Cases, log",
                               "Per Pop. Using Internet",
                               "Mobile Cell Sub. per 100",
                               "Lower middle income",
                               "Upper middle income",
                               "High income"),
          omit.stat = c("f","ser", "rsq"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          omit.table.layout = "n",
          out = file.path(paper_tables, 
                          "lm_cor_loss_of_smell.tex"))

