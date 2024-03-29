# Prepare Data for Dashboard

#### PARAMETERS
DASHBOARD_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData", "data")

begin_day <- c("2020-02-01",
               "2020-03-01",
               "2020-04-01",
               "2020-05-01",
               "2020-06-01",
               "2020-07-01",
               "2020-08-01",
               "2020-09-01")

#begin_day <- "2020-02-01"

#### Load Keywords
keywords_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                                 "keywords", "FinalData", 
                                 "covid_keywords_alllanguages_clean.Rds"))
keywords <- keywords_df %>%
  filter(scrape %in% "yes") %>%
  pull(keyword_en) %>%
  tolower()

# World Shapefile --------------------------------------------------------------
# world_sp <- readRDS(file.path(dropbox_file_path, "Data", "world_shapefile", 
#                               "FinalData",
#                               "TM_WORLD_BORDERS-0.3_simplified.Rds"))

world_sp <- ne_countries(type = "countries", scale=50)
world_sp@data <- world_sp@data %>%
  dplyr::select(name, continent, iso_a2) %>%
  dplyr::rename(geo = iso_a2)

world_sp$geo <- world_sp$geo %>% as.character()

## Remove some polygons
world_sp <- world_sp[world_sp$continent != "Antarctica",]
world_sp <- world_sp[world_sp$name != "Fr. S. Antarctic Lands",]
world_sp <- world_sp[world_sp$name != "Heard I. and McDonald Is.",]
world_sp <- world_sp[world_sp$name != "Br. Indian Ocean Ter.",]
world_sp <- world_sp[world_sp$name != "S. Geo. and S. Sandw. Is.",]

## Rename Continent for select countries
# Do if open ocean
world_sp$continent[world_sp$name %in% "Seychelles"] <- "Africa"
world_sp$continent[world_sp$name %in% "Maldives"] <- "Asia"
world_sp$continent[world_sp$name %in% "Mauritius"] <- "Africa"
world_sp$continent[world_sp$name %in% "Saint Helena"] <- "Africa"

world_df <- world_sp@data

saveRDS(world_sp, file.path(DASHBOARD_PATH, "world.Rds"))

# Correlations -----------------------------------------------------------------
for(begin_day_i in begin_day){
  
  print(paste(begin_day_i, "-------------------------------------------------"))
  
  # cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
  #                             "global_with_refstate",
  #                             paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",begin_day_i,".Rds")))
  # 
  cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                paste0("correlations_gtrends_otherdata_varclean_since",begin_day_i,".Rds")))
  
  cor_df <- cor_df[tolower(cor_df$keyword_en) %in% keywords,]
  
  cor_df$keyword_en <- cor_df$keyword_en %>% tools::toTitleCase() %>% str_replace_all("\\bi\\b", "I")
  cor_df <- merge(cor_df, world_df, by = "geo")

  saveRDS(cor_df, file.path(DASHBOARD_PATH, paste0("correlations_since_",begin_day_i,".Rds")))
  
  
  # gTrends ----------------------------------------------------------------------
  gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                paste0("gtrends_otherdata_varclean_since",begin_day_i,".Rds")))
  
  #gtrends_df <- gtrends_df[gtrends_df$geo %in% c("US", "BR", "IN"),]
  
  gtrends_df <- gtrends_df[tolower(gtrends_df$keyword_en) %in% keywords,]
  gtrends_df <- gtrends_df[!is.na(gtrends_df$keyword_en),]
  
  gtrends_df$keyword_en <- gtrends_df$keyword_en %>% tools::toTitleCase() %>% str_replace_all("\\bi\\b", "I")
  
  gtrends_df <- gtrends_df[gtrends_df$date >= "2020-02-01",]
  gtrends_df <- merge(gtrends_df, world_df, by = "geo")
  
  gtrends_df <- gtrends_df %>%
    dplyr::select(keyword_en, keyword, date, hits, hits_ma7, name, geo, continent, cases_new, death_new,
                  cor_casesMA7_hitsMA7_max, cor_casesMA7_hitsMA7_nolag, cor_casesMA7_hitsMA7_lag,
                  cor_deathMA7_hitsMA7_max, cor_deathMA7_hitsMA7_nolag, cor_deathMA7_hitsMA7_lag,
                  cases_total, death_total)
  
  saveRDS(gtrends_df, file.path(DASHBOARD_PATH, paste0("gtrends_since_",begin_day_i,".Rds")))
  
  # Sparkline Table --------------------------------------------------------------
  for(height_width in c("small", "large")){
    if(height_width == "small"){
      SPARK_HEIGHT <- 35
      SPARK_WIDTH <- 100
    } else{
      SPARK_HEIGHT <- 150
      SPARK_WIDTH <- 200
    }
    
    # gtrends_sum_df <- gtrends_df %>% 
    #   arrange(date) %>%
    #   group_by(name, continent, keyword_en,
    #            cases_total, death_total) %>% 
    #   summarize(cases_new_spark = spk_chr(cases_new,
    #                                       lineColor = 'orange', 
    #                                       fillColor = 'orange',
    #                                       chartRangeMin = 0,
    #                                       chartRangeMax = 8,
    #                                       width = width,
    #                                       height = height,
    #                                       tooltipChartTitle = "COVID-19 Cases",
    #                                       highlightLineColor = 'orange', 
    #                                       highlightSpotColor = 'orange'),
    #             death_new_spark = spk_chr(death_new,
    #                                       lineColor = 'orange', 
    #                                       fillColor = 'orange',
    #                                       chartRangeMin = 0,
    #                                       chartRangeMax = 8,
    #                                       width = width,
    #                                       height = height,
    #                                       tooltipChartTitle = "COVID-19 Deaths",
    #                                       highlightLineColor = 'orange', 
    #                                       highlightSpotColor = 'orange'),
    #             hits_ma7_spark = spk_chr(hits_ma7,
    #                                      lineColor = 'forestgreen', 
    #                                      fillColor = 'forestgreen',
    #                                      chartRangeMin = 0,
    #                                      chartRangeMax = 8,
    #                                      width = width,
    #                                      height = height,
    #                                      tooltipChartTitle = "Search Activity",
    #                                      highlightLineColor = 'orange', 
    #                                      highlightSpotColor = 'orange'),
    #             cor_casesMA7_hitsMA7_max = cor_casesMA7_hitsMA7_max[1], 
    #             cor_casesMA7_hitsMA7_lag = cor_casesMA7_hitsMA7_lag[1],
    #             cor_deathMA7_hitsMA7_max = cor_deathMA7_hitsMA7_max[1], 
    #             cor_deathMA7_hitsMA7_lag = cor_deathMA7_hitsMA7_lag[1]) 
    
    # COMPOSIT
    gtrends_df$group <- paste0(gtrends_df$name,
                               gtrends_df$keyword_en)
    
    gtrends_spark_df <- gtrends_df %>%
      #filter(keyword_en %in% "Loss of Smell") %>%
      arrange(date) %>%
      split(.$group) %>% 
      map_df(~{
        l_cases <- sparkline(.x$cases_new,
                             type='bar',
                             barColor="orange",
                             chartRangeMin = 0,
                             chartRangeMax = 8,
                             width = SPARK_WIDTH,
                             height = SPARK_HEIGHT,
                             tooltipChartTitle = "COVID-19 Cases",
                             highlightLineColor = 'orange', 
                             highlightSpotColor = 'orange')
        l_death <- sparkline(.x$death_new,
                             type='bar',
                             barColor="orange",
                             chartRangeMin = 0,
                             chartRangeMax = 8,
                             width = SPARK_WIDTH,
                             height = SPARK_HEIGHT,
                             tooltipChartTitle = "COVID-19 Deaths",
                             highlightLineColor = 'orange', 
                             highlightSpotColor = 'orange')
        l_hits <- sparkline(.x$hits_ma7 %>% round(2),
                            type="line",
                            lineColor = 'green', 
                            fillColor = NULL,
                            chartRangeMin = 0,
                            chartRangeMax = 8,
                            width = SPARK_WIDTH,
                            height = SPARK_HEIGHT,
                            tooltipChartTitle = "Search Interest",
                            highlightLineColor = 'green', 
                            highlightSpotColor = 'green') 
        l_cases_hits <- spk_composite(l_cases, 
                                      l_hits)
        l_death_hits <- spk_composite(l_death, 
                                      l_hits) 
        data.frame(l_cases = as.character(htmltools::as.tags(l_cases)), 
                   l_death = as.character(htmltools::as.tags(l_death)), 
                   l_hits = as.character(htmltools::as.tags(l_hits)), 
                   l_cases_hits = as.character(htmltools::as.tags(l_cases_hits)),
                   l_death_hits = as.character(htmltools::as.tags(l_death_hits)))
      }, .id = 'Type') 
    
    ## Merge other data back in
    gtrends_sum_df <- gtrends_df %>%
      #filter(keyword_en %in% "Loss of Smell") %>%
      dplyr::group_by(group, name, keyword_en, keyword, continent, geo) %>%
      dplyr::summarise(cases_total = max(cases_total, na.rm=T),
                death_total = max(death_total, na.rm=T),
                cor_casesMA7_hitsMA7_nolag  = cor_casesMA7_hitsMA7_nolag[1],
                cor_casesMA7_hitsMA7_lag    = cor_casesMA7_hitsMA7_lag[1],
                cor_casesMA7_hitsMA7_max    = cor_casesMA7_hitsMA7_max[1],
                
                cor_deathMA7_hitsMA7_nolag  = cor_deathMA7_hitsMA7_nolag[1],
                cor_deathMA7_hitsMA7_lag    = cor_deathMA7_hitsMA7_lag[1],
                cor_deathMA7_hitsMA7_max    = cor_deathMA7_hitsMA7_max[1]) %>%
      ungroup()
    
    gtrends_spark_df <- gtrends_spark_df %>%
      dplyr::select(Type, l_cases_hits, l_death_hits) %>%
      dplyr::rename(group = Type) %>%
      left_join(gtrends_sum_df, by = "group") %>%
      dplyr::select(-group)
    
    saveRDS(gtrends_spark_df, file.path(DASHBOARD_PATH, paste0("gtrends_spark_since_",begin_day_i,"_",height_width,".Rds")))
  }
}



