

FIGURES_PATH <- file.path(github_file_path, "Dashboards", "google_trends", "precomputed_figures")
DASHBOARD_PATH <- file.path(github_file_path, "Dashboards", "google_trends", "data")

keyword <- "Loss of Smell"
cases_deaths <- "Cases"
continent <- "All"
sort_by <- "Name"

for(keyword in c("Loss of Smell",
                 "I Can't Smell",
                 "Fever",
                 "Cough")){
  for(cases_deaths in c("Cases",
                        "Deaths")){
    for(continent in c("All",
                       "Asia",
                       "Africa",
                       "Europe",
                       "South America",
                       "Oceania",
                       "North America")){
      for(sort_by in c("Name",
                       "Cases",
                       "Deaths",
                       "Correlation")){
        
        # Load Data ------------------------------------------------------------
        gtrends_df <- readRDS(file.path(DASHBOARD_PATH, "gtrends.Rds"))
        cor_df     <- readRDS(file.path(DASHBOARD_PATH, "correlations.Rds"))
        cor_max_df <- readRDS(file.path(DASHBOARD_PATH, "correlations_max_lag.Rds"))
        world_sf   <- readRDS(file.path(DASHBOARD_PATH, "world_ne.Rds"))
        
        GEO <- c("US", "BR")
        gtrends_df <- gtrends_df[gtrends_df$geo %in% GEO,]
        cor_df     <- cor_df[cor_df$geo %in% GEO,]
        cor_max_df <- cor_max_df[cor_max_df$geo %in% GEO,]
        
        # Subset ---------------------------------------------------------------
        gtrends_df <- gtrends_df[gtrends_df$keyword_en %in% keyword,]
        cor_df     <- cor_df[cor_df$keyword_en %in% keyword,]
        cor_max_df <- cor_max_df[cor_max_df$keyword_en %in% keyword,]
        
        if(continent != "All"){
          world_df <- world_df[world_df$continent %in% continent,]
          
          gtrends_df <- gtrends_df[gtrends_df$geo %in% world_sf$geo,]
          cor_df     <- cor_df[cor_df$geo %in% world_sf$geo,]
          cor_max_df <- cor_max_df[cor_max_df$geo %in% world_sf$geo,]
        }
        
        if(cases_deaths %in% c("Cases")){
          gtrends_df$covid_new <- gtrends_df$cases_new
          cor_df$cor_covid_new <- cor_df$cor_cases_new
          cor_max_df$time_lag_covid_cor_max <- cor_max_df$time_lag_cases_cor_max
          cor_max_df$cor_covid_new_max <- cor_max_df$cor_cases_new_max
        }
        
        if(cases_deaths %in% c("Deaths")){
          gtrends_df$covid_new <- gtrends_df$death_new
          cor_df$cor_covid_new <- cor_df$cor_death_new
          cor_max_df$time_lag_covid_cor_max <- cor_max_df$time_lag_death_cor_max
          cor_max_df$cor_covid_new_max <- cor_max_df$cor_death_new_max
        }
        
        # 1. Line and Cor Figure -----------------------------------------------
        gtrends_sub_df <- gtrends_df %>%
          mutate(hits = hits_ma7) %>%
          group_by(geo) %>%
          mutate(hits = hits / max(hits, na.rm = T)) %>% # ensure max is 1 (for eg, for moving avg)
          mutate(hits = hits * max(covid_new)) %>%
          ungroup() 
        
        if(sort_by %in% c("Cases", "Deaths")){
          gtrends_sub_df$Country <- gtrends_sub_df$Country %>% 
            as.factor() %>% 
            reorder(-gtrends_sub_df$covid_new)
        }
        
        if(sort_by %in% "Correlation"){
          gtrends_sub_df$Country <- gtrends_sub_df$Country %>% 
            as.factor() %>% 
            reorder(-gtrends_sub_df$covid_hits_cor)
        }
        
        p_line <- ggplot(aes(x = date)) +
          geom_col(aes(y = covid_new, fill = paste("COVID-19", cases_deaths))) +
          geom_line(aes(y = hits, color = paste0("Search Popularity of ", keyword_en))) +
          facet_wrap(~Country,
                     scales = "free_y",
                     ncol = 1) +
          scale_fill_manual(values = "orange1") +
          scale_color_manual(values = "green4") +
          labs(x = "", y = paste("COVID-19", cases_deaths),
               fill = "", color = "") +
          theme_ipsum() + 
          theme(legend.position="top",
                legend.text = element_text(size=14))
        
        p_cor <- cor_df %>%
          ggplot() +
          geom_col(aes(x = time_lag, y = cor_covid_new, fill = cor_covid_new)) + 
          geom_vline(xintercept = 0,
                     color = "black") +
          scale_fill_gradient2(low =  "#1A9850",
                               mid = "#FFFFBF",
                               high = "#D73027",
                               midpoint = 0) +
          labs(x = "Time Lag (Days)",
               y = "Correlation") +
          theme_ipsum() +
          facet_wrap(~Country,
                     ncol = 1) +
          theme(legend.position="top",
                legend.text = element_text(size=14))
        
        p_all <- ggarrange(p_line,
                           p_cor,
                           ncol = 2)
        
        saveRDS(p_all, file.pth(FIGURES_PATH, paste0("fig_line_cor",
                                                     "_keyword", keyword,
                                                     "_cases_deaths", cases_deaths,
                                                     "_continent", continent,
                                                     "_sort_by", sort_by,
                                                     ".Rds")))
        
        # 2. Histogram of Correlation ------------------------------------------
        time_lag_best <- cor_max_df$time_lag_cases_cor_max %>% mean() %>% round()
        
        df <- cor_df %>%
          filter(time_lag == time_lag_best) %>%
          filter(!is.na(cor_covid_new)) %>%
          dplyr::mutate(bins = round(cor_covid_new*100, digits=-1) / 100) %>%
          distinct(geo, bins) %>%
          dplyr::group_by(bins) %>%
          dplyr::summarise(N = n()) %>%
          ungroup() %>%
          mutate(text = paste0("Correlation: ", bins, "\nN countries: ", N)) 
        
        df_m <- seq(from = min(df$bins),
                    to = max(df$bins),
                    by = .1) %>%
          as.data.frame() %>%
          dplyr::rename(bins = ".") %>%
          mutate(bins = bins %>% round(1))
        
        df <- merge(df, df_m, by = "bins")  
        
        p <- ggplot(df) +
          geom_col(aes(x = bins %>% as.factor(), 
                       y = N, 
                       fill = bins,
                       text = text), color = "black") +
          labs(x = "Correlation",
               y = "Number of Countries") +
          scale_fill_gradient(low = "white",
                              high = muted("red")) +
          theme_ipsum() +
          theme(legend.position = "none")
        
        saveRDS(p, file.pth(FIGURES_PATH, paste0("fig_cor_hist",
                                                 "_keyword", keyword,
                                                 "_cases_deaths", cases_deaths,
                                                 "_continent", continent,
                                                 ".Rds")))
        
        saveRDS(time_lag_best, file.pth(FIGURES_PATH, paste0("stat_time_lag_best",
                                                             "_keyword", keyword,
                                                             "_cases_deaths", cases_deaths,
                                                             "_continent", continent,
                                                             ".Rds")))
        
        # 3. Time Lag Hist -----------------------------------------------------
        p <- cor_max_df %>%
          ggplot() +
          geom_histogram(aes(x = time_lag_covid_cor_max),
                         fill = "deepskyblue2",
                         color = "black",
                         binwidth = 5) +
          geom_vline(aes(xintercept = 0), color = "red") +
          labs(x = "Time Lag (Days)",
               y = "Number of Countries") +
          theme_ipsum()
        
        saveRDS(p, file.pth(FIGURES_PATH, paste0("fig_time_lag_hist",
                                                 "_keyword", keyword,
                                                 "_cases_deaths", cases_deaths,
                                                 "_continent", continent,
                                                 ".Rds")))
        
        # 4. Correlation Map ---------------------------------------------------
        cor_sub_df <- cor_df %>%
          filter(time_lag == time_lag_best) %>%
          distinct(geo, cor_covid_new)
        
        world_data_sf <- merge(world_sf, cor_sub_df, all.x = T, all.y = F)
        world_data_sf$text <- paste0(world_data_sf$name, "\n", world_data_sf$cor_covid_new %>% round(2))
        
        p <- ggplot() +
          geom_sf(data = world_data_sf,
                  aes(fill = cor_covid_new,
                      text = text),
                  color = NA) +
          scale_fill_gradient(low = "white",
                              high = muted("red")) +
          theme_void() +
          theme(legend.position = "none") +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.line = element_blank()) #+
          #coord_map(
          #  projection = "mercator")
        
        saveRDS(p, file.pth(FIGURES_PATH, paste0("fig_cor_map",
                                                 "_keyword", keyword,
                                                 "_cases_deaths", cases_deaths,
                                                 "_continent", continent,
                                                 ".Rds")))
        
        
        
      }
    }
  }
}


