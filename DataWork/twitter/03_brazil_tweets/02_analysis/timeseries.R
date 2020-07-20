# Time Series Analysis

# brazil_with_refstate
data <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "brazil_with_refstate_covid_pop.Rds"))

data$cases[is.na(data$cases)] <- 0

data <- data[data$keyword %in% "febre",]

lm(cases ~ hits, data = data) %>% summary()
lm(cases ~ hits_adj, data = data) %>% summary()

ggplot(data) +
  geom_line(aes(x = date, y = hits)) +
  facet_wrap(~geo)

ggplot(data) +
  geom_line(aes(x = date, y = cases)) +
  facet_wrap(~geo)

head(data)
