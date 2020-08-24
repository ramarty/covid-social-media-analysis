
DASHBOARD_PATH <- file.path(dropbox_file_path, "Data", "google_trends", 
                            "DashboardData", "data")

FIGURES_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData",
                          "precomputed_figures")

keyword <- "Loss of Smell" # "Loss of Smell"
cases_deaths <- "Cases"
continent <- "All" # "Asia" # 
sort_by <- "Name"

gtrends_df <- readRDS(file.path(DASHBOARD_PATH, "gtrends.Rds"))
keyword_list <- unique(gtrends_df$keyword_en) %>% sort()

saveRDS(keyword_list, file.path(FIGURES_PATH, paste0("keyword_list",
                                                   ".Rds")))

for(keyword in keyword_list){
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
        
        print(paste(keyword, "-",
                    cases_deaths, "-",
                    continent, "-",
                    sort_by))
        
        # Load Data ------------------------------------------------------------
        gtrends_df <- readRDS(file.path(DASHBOARD_PATH, "gtrends.Rds"))
        cor_df     <- readRDS(file.path(DASHBOARD_PATH, "correlations.Rds"))
        cor_max_df <- readRDS(file.path(DASHBOARD_PATH, "correlations_max_lag.Rds"))
        world_sf   <- readRDS(file.path(DASHBOARD_PATH, "world_ne.Rds"))
        
        #GEO <- c("US", "BR", "FR", "ES", "MX", "PT")
        #gtrends_df <- gtrends_df[gtrends_df$geo %in% GEO,]
        #cor_df     <- cor_df[cor_df$geo %in% GEO,]
        #cor_max_df <- cor_max_df[cor_max_df$geo %in% GEO,]
        
        # Subset ---------------------------------------------------------------
        gtrends_df <- gtrends_df[gtrends_df$keyword_en %in% keyword,]
        cor_df     <- cor_df[cor_df$keyword_en %in% keyword,]
        cor_max_df <- cor_max_df[cor_max_df$keyword_en %in% keyword,]
        
        if(continent != "All"){
          world_sf <- world_sf[world_sf$continent %in% continent,]
          
          gtrends_df <- gtrends_df[gtrends_df$geo %in% world_sf$geo,]
          cor_df     <- cor_df[cor_df$geo %in% world_sf$geo,]
          cor_max_df <- cor_max_df[cor_max_df$geo %in% world_sf$geo,]
        }
        
        if(cases_deaths %in% c("Cases")){
          gtrends_df$covid_new <- gtrends_df$cases_new
          cor_df$cor_covid_new <- cor_df$cor_cases_new
          
          gtrends_df$covid_total <- gtrends_df$cases_total
          cor_df$covid_total <- cor_df$cases_total
          
          cor_max_df$time_lag_covid_cor_max <- cor_max_df$time_lag_cases_cor_max
          cor_max_df$cor_covid_new_max <- cor_max_df$cor_cases_new_max
        }
        
        if(cases_deaths %in% c("Deaths")){
          gtrends_df$covid_new <- gtrends_df$death_new
          cor_df$cor_covid_new <- cor_df$cor_death_new
          
          gtrends_df$covid_total <- gtrends_df$death_total
          cor_df$covid_total <- cor_df$death_total
          
          cor_max_df$time_lag_covid_cor_max <- cor_max_df$time_lag_death_cor_max
          cor_max_df$cor_covid_new_max <- cor_max_df$cor_death_new_max
        }
        
        # Prep Increase Data ---------------------------------------------------
        last_14_days <- gtrends_df$date %>% 
          unique %>% 
          sort() %>% 
          tail(14)
        
        first_week  <- last_14_days %>% head(7)
        second_week <- last_14_days %>% tail(7)
        
        gtrends_df$week <- NA
        gtrends_df$week[gtrends_df$date %in% first_week] <- 1
        gtrends_df$week[gtrends_df$date %in% second_week] <- 2
        
        gtrends_df <- gtrends_df %>%
          group_by(geo) %>%
          mutate(week_1_hits = mean(hits[week %in% 1]),
                 week_2_hits = mean(hits[week %in% 2]),
                 
                 week_1_hits_ma7 = mean(hits_ma7[week %in% 1]),
                 week_2_hits_ma7 = mean(hits_ma7[week %in% 2])) %>%
          ungroup() %>%
          mutate(hits_change     = week_2_hits     - week_1_hits,
                 hits_ma7_change = week_2_hits_ma7 - week_1_hits_ma7) 
        
        # 0. Best Time Lag -----------------------------------------------------
        #### Stat
        time_lag_best <- cor_max_df$time_lag_cases_cor_max %>% mean() %>% round()
        
        saveRDS(time_lag_best, file.path(FIGURES_PATH, paste0("stat_time_lag_best",
                                                              "_keyword", keyword,
                                                              "_cases_deaths", cases_deaths,
                                                              "_continent", continent,
                                                              ".Rds")))
        
        
        #### Merge with gTrends and cor_df
        
        gtrends_df$cases_hits_cor <- NULL
        gtrends_df$death_hits_cor <- NULL
        
        cor_sub_df <- cor_df[cor_df$time_lag %in% time_lag_best,]
        cor_sub_varlim_df <- cor_sub_df %>%
          distinct(geo, cor_covid_new) %>%
          dplyr::rename(cor_covid_new_best = cor_covid_new)
        gtrends_df <- merge(gtrends_df, cor_sub_varlim_df, by = "geo")
        cor_df <- merge(cor_df, cor_sub_varlim_df, by = "geo")
        
        ## Save dataframe (for using for text)
        cor_sub_df <- cor_df[cor_df$time_lag %in% time_lag_best,]
        cor_sub_df <- cor_sub_df %>%
          distinct(Country, cor_covid_new) %>%
          filter(!is.na(cor_covid_new))
        
        saveRDS(cor_sub_df, file.path(FIGURES_PATH, paste0("stat_cor_countries",
                                                              "_keyword", keyword,
                                                              "_cases_deaths", cases_deaths,
                                                              "_continent", continent,
                                                              ".Rds")))
        
        # 1. Line and Cor Figure -----------------------------------------------
        gtrends_sub_df <- gtrends_df %>%
          mutate(hits = hits_ma7) %>%
          group_by(geo) %>%
          mutate(hits = hits / max(hits, na.rm = T)) %>% # ensure max is 1 (for eg, for moving avg)
          mutate(hits = hits * max(covid_new)) %>%
          ungroup() %>%
          unique()
        
        if(nrow(gtrends_sub_df) %in% 0) next
        
        if(sort_by %in% c("Cases", "Deaths")){
          gtrends_sub_df$Country <- gtrends_sub_df$Country %>% 
            as.factor() %>% 
            reorder(-gtrends_sub_df$covid_total)
          
          cor_df$Country <- cor_df$Country %>% 
            as.factor() %>% 
            reorder(-cor_df$covid_total)
        }
        
        if(sort_by %in% "Correlation"){
          gtrends_sub_df$Country <- gtrends_sub_df$Country %>% 
            as.factor() %>% 
            reorder(-gtrends_sub_df$cor_covid_new_best)
          
          cor_df$Country <- cor_df$Country %>% 
            as.factor() %>% 
            reorder(-cor_df$cor_covid_new_best)
        }
        
        GEO_BOTH <- intersect(gtrends_sub_df$geo, cor_df$geo)
        
        if(length(GEO_BOTH) %in% 0) next
        
        p_line <- ggplot(gtrends_sub_df[gtrends_sub_df$geo %in% GEO_BOTH,], 
                         aes(x = date)) +
          geom_col(aes(y = covid_new, fill = paste("COVID-19", cases_deaths))) +
          geom_line(aes(y = hits, color = paste0("Search Popularity of ", keyword_en))) +
          facet_wrap(~Country,
                     scales = "free_y",
                     ncol = 1) +
          scale_fill_manual(values = "orange1") +
          scale_color_manual(values = "green4") +
          labs(x = "", y = paste("COVID-19", cases_deaths),
               fill = "", color = "",
               title = paste0("Trends in COVID ",tolower(cases_deaths),  
                              "\nand search popularity of ",
                              keyword, "\n \n")) +
          theme_ipsum() + 
          theme(legend.position="top",
                legend.text = element_text(size=14))
        
        p_cor <- cor_df[cor_df$geo %in% GEO_BOTH,] %>%
          ggplot() +
          geom_col(aes(x = time_lag, y = cor_covid_new, fill = cor_covid_new)) + 
          geom_vline(xintercept = 0,
                     color = "black") +
          scale_fill_gradient2(low =  "#1A9850",
                               mid = "#FFFFBF",
                               high = "#D73027",
                               midpoint = 0) +
          labs(x = "Time Lag (Days)",
               y = "Correlation",
               fill = "Correlation",
               title = paste0("Strengh of correlation between COVID\n",
                              tolower(cases_deaths), 
                              " and search popularity of\n",
                              keyword, 
                              " across different time lags\nof search popularity.")
               ) +
          theme_ipsum() +
          facet_wrap(~Country,
                     ncol = 1) +
          theme(legend.position="top",
                legend.text = element_text(size=14))
        
        p_all <- ggarrange(p_line,
                           p_cor,
                           ncol = 2)
        
        saveRDS(p_all, file.path(FIGURES_PATH, paste0("fig_line_cor",
                                                      "_keyword", keyword,
                                                      "_cases_deaths", cases_deaths,
                                                      "_continent", continent,
                                                      "_sort_by", sort_by,
                                                      ".Rds")))
        
        saveRDS(length(unique(GEO_BOTH)), 
                file.path(FIGURES_PATH, paste0("stat_line_cor_N_countries",
                                               "_keyword", keyword,
                                               "_cases_deaths", cases_deaths,
                                               "_continent", continent,
                                               ".Rds")))
        
        # 2. Histogram of Correlation ------------------------------------------
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
        
        saveRDS(p, file.path(FIGURES_PATH, paste0("fig_cor_hist",
                                                  "_keyword", keyword,
                                                  "_cases_deaths", cases_deaths,
                                                  "_continent", continent,
                                                  ".Rds")))
        
        # 3. Time Lag Hist -----------------------------------------------------
        p <- cor_max_df %>%
          mutate(text = "FILLER") %>%
          ggplot() +
          geom_histogram(aes(x = time_lag_covid_cor_max),
                         fill = "palegreen3",
                         color = "black",
                         text = text,
                         binwidth = 5) +
          geom_vline(aes(xintercept = time_lag_best), color = "red") +
          labs(x = "Time lag (days) of strongest correlation",
               y = "Number of Countries") +
          theme_ipsum()
        
        saveRDS(p, file.path(FIGURES_PATH, paste0("fig_time_lag_hist",
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
        
        saveRDS(p, file.path(FIGURES_PATH, paste0("fig_cor_map",
                                                  "_keyword", keyword,
                                                  "_cases_deaths", cases_deaths,
                                                  "_continent", continent,
                                                  ".Rds")))
        
        # 5. Hits Change Table -------------------------------------------------
        #### Define Colors
        customGreen = "#71CA97"
        customGreen0 = "#DeF7E9"
        customRed = "#ff7f7f"
        customRed0 = "#FA614B66"
        customGreen0 = "#DeF7E9"
        customYellow = "goldenrod2"
        
        
        
        data_for_table <- gtrends_df %>%
          filter(!is.na(week)) %>%
          group_by(Country, hits_change, cases_total) %>%
          summarize(
            TrendSparkline = spk_chr(
              hits, 
              type ="line",
              lineColor = 'black', 
              fillColor = "orange", 
              height=40,
              width=90
            ),
            week_2_hits = week_2_hits[1]
          ) %>%
          dplyr::select(Country, cases_total, hits_change, week_2_hits, TrendSparkline) %>%
          arrange(-hits_change) %>%
          ungroup()
        
        #### Make Table
        f_list <- list(
          `var1` = formatter("span", style = ~ style(color = "black")),
          `var2` = formatter("span", style = ~ style(color = "black")),
          `var3` = formatter("span", style = ~ style(color = "black")),
          `var4` = formatter("span", style = ~ style(color = "black"))
        )
        
        names_vec <- c("Country", "Total Cases", "Increase", "Average Hits List Week","Trend")
        names(f_list)         <- names_vec[1:4]
        names(data_for_table) <- names_vec
        
        table_max <- 10
        
        l <- formattable(
          data_for_table %>% as.data.table(), # [1:table_max,]
          align = c("l", "l", "l", "l"),
          f_list
        ) %>% format_table(align = c("l", "l", "l", "l")) %>%
          htmltools::HTML() %>%
          div() %>%
          # use new sparkline helper for adding dependency
          spk_add_deps() %>%
          # use column for bootstrap sizing control
          # but could also just wrap in any tag or tagList
          {column(width=12, .)}
        
        saveRDS(l, file.path(FIGURES_PATH, paste0("fig_hits_change_table",
                                                  "_keyword", keyword,
                                                  "_cases_deaths", cases_deaths,
                                                  "_continent", continent,
                                                  ".Rds")))
        
        
        # 6. Hits Change Map ---------------------------------------------------
        change_df <- gtrends_df %>%
          distinct(geo, Country, hits_change)
        
        #spark_df <- data_for_table %>%
        #  dplyr::select(Country, Trend)
        
        world_data_sf <- merge(world_sf, change_df, by = "geo", all.x = T, all.y = F)
        #world_data_sf <- merge(world_data_sf, spark_df, by = "Country", all.x = T, all.y = F)

        
        world_data_sf$text <- paste0(world_data_sf$name, "\n", 
                                     world_data_sf$hits_change %>% round(2))
        
        p <- ggplot() +
          geom_sf(data = world_data_sf,
                  aes(fill = hits_change,
                      text = text),
                  color = NA) +
          
          scale_fill_gradient2(low =  "#1A9850",
                               mid = "#FFFFBF",
                               high = "#D73027",
                               midpoint = 0) +
          theme_void() +
          theme(legend.position = "none") +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.line = element_blank()) #+
        #coord_map(
        #  projection = "mercator")
        
        saveRDS(p, file.path(FIGURES_PATH, paste0("fig_hits_change_map",
                                                  "_keyword", keyword,
                                                  "_cases_deaths", cases_deaths,
                                                  "_continent", continent,
                                                  ".Rds")))
        
      }
    }
  }
}


