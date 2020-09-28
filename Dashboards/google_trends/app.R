# Google Trends Dashboard

# TODO:
# 1. Table - which search term is most predictive for each country. The table will
#    report: country name, search term, correlation, total cases, total deaths.

# PACKAGES AND SETUP ===========================================================

#### Setting directory so will work locally
if (Sys.info()[["user"]] == "robmarty") {
  setwd("~/Documents/Github/covid-social-media-analysis/Dashboards/google_trends")
}

if (Sys.info()[["user"]] == "WB521633") {
  setwd("C:/Users/wb521633/Documents/Github/covid-social-media-analysis/Dashboards/google_trends")
}

#### Pacakges
library(sparkline)
library(shinydashboard)
library(RColorBrewer)
library(shinythemes)
library(DT)
library(dplyr)
library(rmarkdown)
library(lubridate)
library(shiny)
library(wesanderson)
library(ggplot2)
library(ggtext)
library(tidyr)
library(shinyWidgets)
library(zoo)
library(cowplot)
library(bcrypt)
library(shinyjs)
library(ngram)
library(rtweet)
library(stringdist)
library(stringr)
library(rgdal)
library(rgeos)
library(geosphere)
library(htmlwidgets)
library(tidyverse)
library(sf)
library(tidyverse)
library(raster)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(data.table)
library(formattable)
library(tidyr)
library(viridis)
library(data.table)
library(raster)
library(htmltools)
library(scales)
library(lubridate)
library(geosphere)
library(hrbrthemes)
#hrbrthemes::import_roboto_condensed()

gtrends_df <- readRDS(file.path("data", "gtrends.Rds"))
gtrends_spark_df <- readRDS(file.path("data", "gtrends_spark.Rds"))
cor_df     <- readRDS(file.path("data", "correlations.Rds"))
world      <- readRDS(file.path("data", "world.Rds"))

keyword_list <- gtrends_df$keyword_en %>% unique()

keywords_df <- read_csv(file.path("data", "covid_keywords.csv"))

keywords_df <- keywords_df %>%
  dplyr::rename(Portuguese = keyword_pt,
                English = keyword_en,
                Spanish = keyword_es,
                French = keyword_fr,
                Arabic = keyword_ar,
                German = keyword_de,
                Mandarin = keyword_zh,
                Dutch = keyword_nl,
                Italian = keyword_it,
                Norwegian = keyword_no) %>%
  dplyr::filter(scrape %in% c("yes")) %>%
  dplyr::select(English, Spanish, Portuguese, French, Arabic, German, Mandarin,
                Dutch, Italian, Norwegian) 

# UI -==========================================================================
ui <- fluidPage(
  
  navbarPage(
    theme = shinytheme("cosmo"), # journal, flatly, sandstone
    collapsible = TRUE,
    title = "Google Trends",
    
    id = "nav",
    
    # ** Landing Page ----------------------------------------------------------
    tabPanel(
      "Purpose",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        fluidRow(
          column(3,
          ),
          column(6, align = "center",
                 
                 
                 h2("Google Trends Data as Predictor of COVID-19"),
                 hr(),
                 HTML("<h4>When people fall sick, 
                 <a href='https://blog.google/technology/health/using-symptoms-search-trends-inform-covid-19-research/'>many turn to Google</a>
                 before considering medical attention
                 to understand their symptoms and see options for home treatments.
                    This dashboard illustrates how search activity for specific symptoms
                    strongly matches - and often preceds - trends in COVID-19 cases.</h4>"),
                 br(),
                 h4("Trends in search popularity of COVID-19 symptoms should not replace
                 administrative data on cases. The relation between the two is strong
                 but not perfect. However, Google data can supplement official data.
                This is particulrarly true in circumstances
                    when testing or data may not be widely available. Moreover, given that
                    Google trends information is updated in real time, sudden increases in 
                    search activity can warn of potential growth in COVID-19 cases."),
                 br(),
                 h4("As an example, growth in the search popularity of 'Loss of Smell'
                    preceded an increase in COVID-19 cases by about 10 days in the 
                    United States in mid-June. The 
                    dashboard shows this trend holds across many countries.")
          )
        ),
        
        fluidRow(
          #column(12,
          #       ),
          column(12, align = "center",
                 
                 plotOutput("us_ex_fig",
                            height = "350px",
                            width = "700px")
                 
          )
          #column(2,
          #),
          
          
        )
        
      )
    ),
    
    # ** Global Level ----------------------------------------------------------
    tabPanel(
      "Global Level",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        fluidRow(
          
          column(4,
          ),
          column(4, align = "center",
                 HTML("<b><em>Choose to examine COVID cases or deaths and restrict the analysis to
                        a specific continent</em></b>")
                 
          ),
          column(4,
          ),
          
        ),
        
        br(),
        
        
        fluidRow(
          
          column(3,
          ),
          
          column(3, align = "center",
                 
                 selectInput(
                   "select_covid_type",
                   label = strong("Cases/Deaths"),
                   choices = c("Cases", "Deaths"),
                   selected = "Cases",
                   multiple = F
                 )
          ),
          
          column(3, align = "center",
                 
                 selectInput(
                   "select_continent",
                   label = strong("Continent"),
                   choices = c("All",
                               unique(sort(cor_df$continent))),
                   selected = "All",
                   multiple = F
                 )
          ),
          
        ),
        
        hr(),
        
        h2(textOutput("which_keyword_title"),
           align = "center"),
        
        fluidRow(
          column(3,
          ),
          column(6, align = "center",
                 strong("The below figure shows the average correlation between different search
               terms and COVID (black point) and the distribution across countries (green)."),
          ),
          column(3,
          )
        ),
        
        fluidRow(
          column(3,
          ),
          column(6,
                 plotOutput("max_cor_hist",
                            height = "400px")
          ),
          column(3,
          )
          
        ),
        
        hr(),
        
        fluidRow(
          
          #h4(textOutput("cor_title_text"),
          #    align = "center"),
          
          wellPanel(
            
            fluidRow(
              
              column(2,
              ),
              column(8,
                     h2("For a specific search term, when is the correlation highest and how
                    does the correlation vary across countries?",
                        align = "center")
              ),
              column(2,
              )
              
            ),
            
            fluidRow(
              
              column(4,
              ),
              column(4, align = "center",
                     selectInput(
                       "select_keyword",
                       label = strong("Search Term"),
                       choices = keyword_list,
                       selected = "Loss of Smell",
                       multiple = F
                     )
              ),
              column(4,
              )
              
            ),
            
            ## Titles
            fluidRow(
              column(4,
                     column(12, strong(textOutput("text_best_time_lag")), align = "center")
              ),
              column(8,
                     column(12, strong(textOutput("text_cor_countries")), align = "center")
              )
            ),
            
            ## Hist 1
            fluidRow(
              column(4,
                     
                     plotOutput("cor_histogram_time_lag",
                                height = "200px")
              ),
              
              ## Hist 2
              column(4,
                     
                     plotOutput("cor_histogram",
                                height = "200px"),
              ),
              
              ## Hist 3
              column(4,
                     plotOutput("cor_map",
                                height = "200px")
              )
            )
            
          ) #           , style = "padding: 2px; height: 20px"
          
        ),
        
        hr(),
        
        
        fluidRow(
          column(3,
          ),
          column(6,
                 h2(textOutput("trends_title"), align = "center")
          ),
          column(3,
          )
        ),
        
        fluidRow(
          column(1,
          ),
          column(10,
                 strong(htmlOutput("trends_subtitle"), align = "center")
          ),
          column(1,
          )
        ),
        
        br(),
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel(tags$div( 
                      
                      HTML(paste(tags$span(style="color:white", "--------------------------------------------------------------------------------------"), sep = "")) 
                      
                    )),
                    
                    tabPanel("Table View", 
                             
                             fluidRow(
                               column(4,
                               ),
                               column(4,align = "center",
                                      uiOutput("ui_select_sort_by")
                               ),
                               column(4,
                               )
                             ),
                             
                             fluidRow(
                               
                               column(8, align = "center", offset = 2,
                                      htmlOutput("line_graph"),
                               )
                               
                             )
                             
                    ),
                    tabPanel("Map View",
                             
                             fluidRow(
                               column(10, align = "center", offset = 1,
                                      
                                      strong("Click a country on the map"),
                                      leafletOutput("cor_map_leaflet",
                                                    height = "700px")
                                      
                                      )
                             )
                             
                             
                       
                             
                    ), selected = "Table View"
                    
                    
        )
        
      )
    ),
    
    # ** Country Level ----------------------------------------------------------
    tabPanel(
      id = "country_level",
      "Country Level",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        fluidRow(
          
          column(3,
          ),
          
          column(6, align = "center",
                 
                 selectInput(
                   "select_covid_type_map",
                   label = strong("Cases/Deaths"),
                   choices = c("Cases", "Deaths"),
                   selected = "Cases",
                   multiple = F
                 )
          ),
          column(3,
          )
          
        ),
        
        fluidRow(
          column(12, align = "center",
                 h3(textOutput("country_name"))
          )
        ),
        
        fluidRow(
          
          column(6, align = "center",
                 
                 h4("Click a Country on the Map"),
                 
                 leafletOutput("global_map")
                 
          ),
          column(6, align = "center",
                 
                 
                 
                 
                 
          )
          
        ),
        
        br(),
        
        wellPanel(
          fluidRow(
            column(6, align = "center", offset = 3,
                   selectInput(
                     "select_keyword_map",
                     label = strong("Search Term"),
                     choices = keyword_list,
                     selected = "Loss of Smell",
                     multiple = F
                   )
            )
          ), 
          fluidRow(
            
            plotOutput("country_trends",
                       height = "300px")
            
          )
        )
        
        
        
      )
    ),
    
    # ** Changes in Search Term ---------------------------------------------------------------
    # tabPanel(
    #   "Warning System",
    #   tags$head(includeCSS("styles.css")),
    #   
    #   dashboardBody(
    #     
    #     h2("Warning System",
    #        align = "center"),
    #     
    #     fluidRow(
    #       column(8,
    #              "This page shows the change in search term activity. We compare the
    #               average search activity from the past week compared with the week before.",
    #              offset = 2
    #       ),
    #     ),
    #     
    #     fluidRow(
    #       
    #       column(4,
    #              # BLANK for offsetting
    #       ),
    #       
    #       column(2,
    #              selectInput(
    #                "select_term_change",
    #                label = strong("Select Term"),
    #                choices = keyword_list,
    #                selected = "Loss of Smell",
    #                multiple = F
    #              )
    #       ),
    #       
    #       column(2,
    #              selectInput(
    #                "select_continent_change",
    #                label = strong("Continent"),
    #                choices = c("All",
    #                            "Asia",
    #                            "Africa",
    #                            "Europe",
    #                            "South America",
    #                            "Oceania",
    #                            "North America"),
    #                selected = "All",
    #                multiple = F
    #              )
    #       )
    #       
    #     ),
    #     
    #     fluidRow(
    #       column(12,
    #              
    #              plotlyOutput("increase_map"),
    #              
    #              div(style = 'height:1000px; overflow-y: scroll',
    #                  htmlOutput("cor_table"))
    #              
    #       )
    #       
    #       
    #     )
    #     
    #     
    #   )
    # ),
    
    # ** Information -----------------------------------------------------------
    tabPanel(
      "Information",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        fluidRow(
          
          column(3,
          ),
          column(6, align = "center",
                 h2("Keywords"),
                 
                 div(style = 'overflow-x: scroll', tableOutput('keyword_table'))
                 
                 #tableOutput("keyword_table")
                 
          ),
          column(3,
          )
          
          
        )
        
      )
    )
    
  )
)

# SERVER =======================================================================
server = (function(input, output, session) {
  
  # * Global Map ---------------------------------------------------------------
  #### Data
  # Reactive expression for the data subsetted to what the user selected
  cor_df_react <- reactive({
    
    cor_df %>%
      dplyr::filter(type %in% input$select_covid_type_map) %>%
      dplyr::filter(keyword_en %in% input$select_keyword_map) 
    
  })
  
  
  #### Basemap
  output$global_map <- renderLeaflet({
    
    #### Default: map that shows up first
    #cor_sum_df <- cor_df %>%
    #  dplyr::filter(type %in% "Cases") %>%
    #  dplyr::filter(keyword_en %in% "Loss of Smell") 
    
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) 
    
  })
  
  observe({
    
    req(input$nav == "Country Level") # This makes leaflet show up; before no defaults.
    
    if(F){
      cor_sum_df <- cor_df %>%
        dplyr::filter(type %in% "Cases") %>%
        dplyr::filter(keyword_en %in% "Loss of Smell") 
    }
    
    cor_sum_df <- cor_df_react()
    
    cor_sum_df$name <- NULL  
    
    world_data <- merge(world, cor_sum_df, by = "geo", all.x=T, all.y=F)
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = c(world_data$cor[!is.na(world_data$cor)], 0, 1))
    
    leafletProxy("global_map",
                 data = world_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(label = ~name,
                  layerId = ~ name,
                  stroke = F,
                  fillOpacity = 1,
                  color = ~pal(cor)) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = c(world_data$cor[!is.na(world_data$cor)], 0, 1),
                title = paste0("Correlation<br>between<br>",
                               input$select_covid_type_map, 
                               " and<br>Search<br>Activity"),
                opacity = 1,
                bins = c(0, 0.5, 1)
      )
    
  })
  
  # * Country Figures ----------------------------------------------------------
  
  # *** Country Name Reactive -----
  country_name_react <- reactive({
    
    if(is.null(input$global_map_shape_click$id)){
      country_name <- "United States"
    } else{
      country_name <- input$global_map_shape_click$id
    }
    
    country_name
    
  })
  
  # *** Country Trends -----------------------------------------------------------
  country_data_react <- reactive({
    
    country_name <- country_name_react()
    
    gtrends_sub_df <- gtrends_df %>%
      dplyr::filter(name %in% country_name,
                    keyword_en %in% input$select_keyword_map) 
    
    ## If nrow = 0, give 1 row with just name
    if(nrow(gtrends_sub_df) == 0){
      
      gtrends_sub_df <- bind_rows(gtrends_sub_df,
                                  data.frame(name = country_name))
      
    }
    
    gtrends_sub_df
    
  })
  
  observe({
    
    
    output$country_trends <- renderPlot({
      
      color_cases <- "orange"
      color_hits <- "forestgreen"
      
      #### Subset to country
      if(F){
        gtrends_sub_df <- gtrends_df %>%
          filter(name %in% "United States",
                 keyword_en %in% "Loss of Smell") 
      }
      
      gtrends_sub_df <- country_data_react()
      
      #### Define case/death varibles
      if(input$select_covid_type %in% "Cases"){
        
        gtrends_sub_df <- gtrends_sub_df %>%
          dplyr::select(name, date, hits_ma7, hits, cases_new,
                        cor_casesMA7_hitsMA7_max, cor_casesMA7_hitsMA7_lag,
                        cases_total) %>%
          dplyr::rename(Country = name,
                        covid_new = cases_new,
                        "Correlation" = cor_casesMA7_hitsMA7_max,
                        "Correlation Lag" = cor_casesMA7_hitsMA7_lag)
        
      } 
      if(input$select_covid_type %in% "Deaths"){
        
        gtrends_sub_df <- gtrends_sub_df %>%
          dplyr::select(name, date, hits_ma7, hits, death_new,
                        cor_deathMA7_hitsMA7_max, cor_deathMA7_hitsMA7_lag,
                        death_total) %>%
          dplyr::rename(Country = name,
                        covid_new = death_new,
                        "Correlation" = cor_deathMA7_hitsMA7_max,
                        "Correlation Lag" = cor_deathMA7_hitsMA7_lag)
      } 
      
      #### Prep hits
      gtrends_sub_df$hits_fig <-  gtrends_sub_df$hits_ma7
      
      # multiplier so that max of hits is same as covid
      multiplier <- max(gtrends_sub_df$covid_new, na.rm=T) / max(gtrends_sub_df$hits_fig, na.rm=T)
      
      gtrends_sub_df$hits_fig <- gtrends_sub_df$hits_fig * multiplier
      
      #### Figure
      # Check if 1 row. If no data, previous step gives 1 row with "name" 
      # variable filled in.
      if(nrow(gtrends_sub_df) %in% 1){
        p <- ggplot() +
          labs(title = "No Data") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size=16))
      } else{
        p <-       ggplot() +
          geom_col(data = gtrends_sub_df,
                   aes(x = date, y = covid_new),
                   fill = color_cases,
                   color = color_cases) +
          geom_line(data = gtrends_sub_df,
                    aes(x = date, y = hits_fig),
                    color = color_hits,
                    size=1) +
          labs(x = NULL,
               y = input$select_covid_type_map) +
          scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Search\nPopularity",
                                                 breaks = c(0, max(gtrends_sub_df$hits_fig, na.rm=T)),
                                                 labels = c("Low", "High"))) +
          theme_ipsum() +
          theme(axis.title.y.left = element_text(angle = 0, 
                                                 vjust = 0.5, 
                                                 color=color_cases,
                                                 face = "bold",
                                                 size=16),
                axis.title.y.right = element_text(angle = 0, 
                                                  vjust = 0.5, 
                                                  color=color_hits,
                                                  face = "bold",
                                                  size=16),
                axis.text.y.left = element_text(color = color_cases,
                                                size=16),
                axis.text.y.right = element_text(color = color_hits,
                                                 size=16),
                axis.text = element_text(face = "bold", size=16),
                plot.title = element_text(face = "bold", hjust = 0.5, size=18))
      }
      
      p
      
      
    }, bg = "transparent")
    
  })
  
  
  
  # * Line Graph ---------------------------------------------------------------
  output$line_graph <- renderUI({
    
    
    
    #### Subset
    if(input$select_continent != "All"){
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    gtrends_spark_df <- gtrends_spark_df %>%
      filter(keyword_en %in% input$select_keyword)
    
    if(input$select_covid_type %in% "Cases"){
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(name, l_cases_hits, 
                      cor_casesMA7_hitsMA7_max, cor_casesMA7_hitsMA7_lag,
                      cases_total) %>%
        dplyr::rename(Country = name,
                      Trends = l_cases_hits,
                      "Correlation" = cor_casesMA7_hitsMA7_max,
                      "Correlation Lag" = cor_casesMA7_hitsMA7_lag)
      
      
    } 
    if(input$select_covid_type %in% "Deaths"){
      
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(name, l_death_hits,
                      cor_deathMA7_hitsMA7_max, cor_deathMA7_hitsMA7_lag,
                      death_total) %>%
        dplyr::rename(Country = name,
                      Trends = l_death_hits,
                      "Correlation" = cor_deathMA7_hitsMA7_max,
                      "Correlation Lag" = cor_deathMA7_hitsMA7_lag)
    } 
    
    #### Sort
    if(input$select_sort_by %in% "Name"){
      gtrends_spark_df <- gtrends_spark_df %>%
        arrange(Country)
    }
    
    if(input$select_sort_by %in% "Correlation"){
      gtrends_spark_df <- gtrends_spark_df %>%
        arrange(-Correlation)
    }
    
    if(input$select_sort_by %in% "Cases"){
      gtrends_spark_df <- gtrends_spark_df %>%
        arrange(-cases_total)
    }
    
    if(input$select_sort_by %in% "Deaths"){
      gtrends_spark_df <- gtrends_spark_df %>%
        arrange(-death_total)
    }
    
    #### Adjust Variables
    gtrends_spark_df$Correlation <- gtrends_spark_df$Correlation %>% round(3)
    
    #### Remove Unneeded Variables
    gtrends_spark_df$continent <- NULL
    gtrends_spark_df$death_total <- NULL
    gtrends_spark_df$cases_total <- NULL
    gtrends_spark_df$keyword_en <- NULL
    
    #### Make Table
    #Country	Trends	Correlation	Correlation Lag
    
    # https://stackoverflow.com/questions/49885176/is-it-possible-to-use-more-than-2-colors-in-the-color-tile-function
    color_tile2 <- function (...) {
      formatter("span", style = function(x) {
        style(display = "block",
              padding = "0 4px", 
              font.weight = "bold",
              `border-radius` = "4px", 
              `background-color` = csscolor(matrix(as.integer(colorRamp(...)(normalize(as.numeric(x)))), 
                                                   byrow=TRUE, dimnames=list(c("red","green","blue"), NULL), nrow=3)))
      })}
    
    
    f_list <- list(
      `Country` = formatter("span", style = ~ style(color = "black", font.weight = "bold", width = "2px")),
      `Correlation Lag` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
      # `Correlation` = formatter(
      #   "span",
      #   style = x ~ style(
      #     display = "inline-block",
      #     direction = "lft",
      #     font.weight = "bold",
      #     #"border-radius" = "4px",
      #     "padding-left" = "2px",
      #     "background-color" = csscolor(bar_color),
      #     width = percent(proportion(x)),
      #     color = csscolor("black")
      #   )
      # )
      
      `Correlation` = color_tile2(c("#95C6D3", "#ECDC87", "#EA7E71"))
      
    )
  
    # Make table
    # https://github.com/renkun-ken/formattable/issues/89
    table_max <- nrow(gtrends_spark_df)
    
    l <- formattable(
      gtrends_spark_df[1:table_max,] %>% as.data.table(),
      align = c("c", "c", "l", "l"),
      f_list
    ) %>% format_table(align = c("c", "c", "l", "l")) %>%
      htmltools::HTML() %>%
      div() %>%
      # use new sparkline helper for adding dependency
      spk_add_deps() #%>%
      # use column for bootstrap sizing control
      # but could also just wrap in any tag or tagList
      #{column(width=12, .)}
    
    l
    
    
  })
  
  # * Histogram - Time Lag Max Cor ---------------------------------------------
  output$cor_histogram_time_lag <- renderPlot({
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    df <- cor_df %>%
      
      dplyr::filter(type %in% input$select_covid_type) %>%
      filter(keyword_en %in% input$select_keyword) 
    
    p <- df %>%
      
      mutate(text = "FILLER") %>%
      ggplot() +
      geom_histogram(aes(x = lag),
                     fill = "palegreen3",
                     color = "black",
                     text = text,
                     binwidth = 3) +
      geom_vline(aes(xintercept = mean(df$lag)), color = "red") +
      labs(x = "Time lag (days) of strongest correlation",
           y = "Number of Countries") +
      theme_minimal()
    
    #p %>%
    #  ggplotly(tooltip = "text") %>%
    #  layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
    #  config(displayModeBar = F)
    p
    
  }, bg = "transparent")
  
  
  # * Histogram - Correlation --------------------------------------------------
  output$cor_histogram <- renderPlot({
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    df <- cor_df %>%
      
      dplyr::filter(type %in% input$select_covid_type) %>%
      filter(keyword_en %in% input$select_keyword) %>%
      
      dplyr::mutate(bins = round(cor*100, digits=-1) / 100) %>%
      distinct(geo, bins) %>%
      dplyr::group_by(bins) %>%
      dplyr::summarise(N = n()) %>%
      ungroup() 
    
    df_m <- seq(from = 0,
                to = 1,
                by = .1) %>%
      as.data.frame() %>%
      dplyr::rename(bins = ".") %>%
      mutate(bins = bins %>% round(1))
    
    df <- merge(df, df_m, by = "bins", all=T) %>%
      mutate(N = replace_na(N, 0)) %>%
      mutate(text = paste0("Correlation: ", bins, "\nN countries: ", N)) 
    
    p <- ggplot(df) +
      geom_col(aes(x = bins %>% as.factor(), 
                   y = N, 
                   fill = bins,
                   text = text), color = "black") +
      labs(x = "Correlation",
           y = "Number of Countries") +
      scale_fill_gradient(low = "white",
                          high = muted("red")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    #p %>%
    #  ggplotly(tooltip = "text") %>%
    #  layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
    #  config(displayModeBar = F)
    p
    
  }, bg = "transparent")
  
  
  # * Correlation Map ------------------------------------------------------------------
  output$cor_map <- renderPlot({
    world$name <- NULL
    
    if(F){
      cor_df <- cor_df %>%
        dplyr::filter(type %in% "Cases") %>%
        dplyr::filter(keyword_en %in% "Loss of Smell") 
    }  
    
    #### Subset world
    if(input$select_continent != "All"){
      world <- world[world$continent %in% input$select_continent,]
    }
    
    #### Subset cor
    cor_df <- cor_df %>%
      dplyr::filter(type %in% input$select_covid_type) %>%
      dplyr::filter(keyword_en %in% input$select_keyword) 
    
    #### Merge
    world_data <- merge(world, cor_df, by = "geo", all.x=T, all.y=F)
    world_data <- world_data %>% st_as_sf()
    
    world_data$text <- paste0(world_data$name, "\n", world_data$cor %>% round(2))
    
    p <- ggplot() +
      geom_sf(data = world_data,
              aes(fill = cor,
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
            axis.line = element_blank()) 
    
    #p %>%
    #  ggplotly(tooltip = "text") %>%
    #  layout(plot_bgcolor='transparent',
    #         paper_bgcolor='transparent') %>%
    #  config(displayModeBar = F)
    p
    
  }, bg = "transparent")
  
  # * Correlation Map - Leaflet ------------------------------------------------
  output$cor_map_leaflet <- renderLeaflet({
    gtrends_spark_df$name <- NULL
    
    # Step 1 convert htmlwidget to character representation of HTML components
    as.character.htmlwidget <- function(x, ...) {
      htmltools::HTML(
        htmltools:::as.character.shiny.tag.list(
          htmlwidgets:::as.tags.htmlwidget(
            x
          ),
          ...
        )
      )
    }
    
    add_deps <- function(dtbl, name, pkg = name) {
      tagList(
        dtbl,
        htmlwidgets::getDependency(name, pkg)
      )
    }

    #### Subset world
    if(input$select_continent != "All"){
      world <- world[world$continent %in% input$select_continent,]
      gtrends_spark_df <- gtrends_spark_df[gtrends_spark_df$continent %in% input$select_continent,]
      
    }
    
    #### Subset Keyword
    gtrends_spark_df <- gtrends_spark_df %>%
      dplyr::filter(keyword_en %in% input$select_keyword) 
    
    #### COVID Type
    if(input$select_covid_type %in% "Cases"){
      gtrends_spark_df$l_covid_hits <- gtrends_spark_df$l_cases_hits
      gtrends_spark_df$cor_covidMA7_hitsMA7_max <- gtrends_spark_df$cor_casesMA7_hitsMA7_max
      gtrends_spark_df$cor_covidMA7_hitsMA7_lag <- gtrends_spark_df$cor_casesMA7_hitsMA7_lag
    } else{
      gtrends_spark_df$l_covid_hits <- gtrends_spark_df$l_death_hits
      gtrends_spark_df$cor_covidMA7_hitsMA7_max <- gtrends_spark_df$cor_deathMA7_hitsMA7_max
      gtrends_spark_df$cor_covidMA7_hitsMA7_lag <- gtrends_spark_df$cor_deathMA7_hitsMA7_lag
    }
    
    #### Merge
    world_data <- merge(world, gtrends_spark_df, by = "geo", all.x=T, all.y=F)
    world_data <- world_data %>% st_as_sf()
    
    #### Prep Correlation
    world_data$cor <- ""
    world_data$cor[!is.na(world_data$cor_covidMA7_hitsMA7_max)] <-
      paste0("<br><b>Correlation:</b> ", world_data$cor_covidMA7_hitsMA7_max[!is.na(world_data$cor_covidMA7_hitsMA7_max)] %>%
               round(3))
    
    world_data$cor_lag <- ""
    world_data$cor_lag[!is.na(world_data$cor_covidMA7_hitsMA7_lag)] <-
      paste0("<br><b>Correlation Lag:</b> ", world_data$cor_covidMA7_hitsMA7_lag[!is.na(world_data$cor_covidMA7_hitsMA7_lag)])
    
    world_data$popup <- paste0("<b>", world_data$name, "</b>", 
                               world_data$cor, 
                               world_data$cor_lag, 
                               "<br>", world_data$l_covid_hits)
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = c(world_data$cor_covidMA7_hitsMA7_max[!is.na(world_data$cor_covidMA7_hitsMA7_max)], 0, 1))
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = world_data,
                  popup = ~popup,
                  popupOptions = popupOptions(minWidth = 200,
                                              maxHeight = 150),
                  stroke = F,
                  fillOpacity = 1,
                  color = ~pal(cor_covidMA7_hitsMA7_max)) %>%
          onRender("function(el,x) {
      this.on('popupopen', function() {HTMLWidgets.staticRender();})
    }") %>%
      addLegend("topright",
                pal = pal,
                values = c(world_data$select_covid_type_map[!is.na(world_data$select_covid_type_map)], 0, 1),
                title = paste0("Correlation<br>between<br>",
                               input$select_covid_type_map,
                               " and<br>Search<br>Activity"),
                opacity = 1,
                bins = c(0, 0.5, 1)
      ) %>%
      setView(zoom = 2, lat=0, lng=0)

  })
  
  # * Max Correlation Hist -----------------------------------------------------
  
  output$max_cor_hist <- renderPlot({
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    cor_all_sum_df <- cor_df %>%
      dplyr::filter(type %in% input$select_covid_type) %>%
      dplyr::group_by(keyword_en) %>%
      dplyr::summarise(cor = mean(cor)) %>%
      dplyr::ungroup()
    
    ggplot() +
      geom_violin(aes(x = reorder(keyword_en,
                                  cor),
                      y = cor),
                  data = cor_df,
                  fill = "palegreen3") +
      geom_point(aes(x = reorder(keyword_en,
                                 cor),
                     y = cor),
                 data = cor_all_sum_df,
                 fill = "palegreen3") +
      geom_hline(yintercept = 0) +
      coord_flip() +
      theme_minimal() +
      labs(x = "",
           y = "Correlation") +
      theme(axis.text.y = element_text(face = "bold", size = 14)) +
      theme(axis.text.x = element_text(size = 14))
    
  })
  
  # * US Example Figure --------------------------------------------------------
  
  output$us_ex_fig <- renderPlot({
    p <- readRDS(file.path("data", "us_ex_fig.Rds"))
    
    p 
    
  })
  
  # output$keyword_table <- DT::renderDataTable({
  #   DT::datatable(keywords_df, 
  #                 options = list(dom = 't'))
  # })
  
  output$keyword_table <- renderTable({
    keywords_df
  })
  
  # * renderUIs ----------------------------------------------------------------
  output$ui_select_covid_cases <- renderUI({
    
    numericInput(
      "select_covid_cases",
      label = strong(paste("Restrict to Countries with X", input$select_covid_type)),
      value = 100,
      min = 0
    )
    
  })
  
  output$country_name <- renderText({
    
    country_data_react()$name[1]
    
  })
  
  output$cor_title_text <- renderText({
    
    paste0("Correlation between search popularity of ",
           input$select_keyword, 
           " and COVID-19 ",
           input$select_covid_type)
    
  })
  
  output$trends_title <- renderText({
    
    paste0("Trends in ",
           input$select_keyword,
           " and COVID-19 ",
           input$select_covid_type)
    
  })
  
  output$trends_subtitle <- renderText({
    
    paste0("We compare trends in <span style='color:orange;'>COVID-19 ",
           tolower(input$select_covid_type), 
           "</span> and <span style='color:green;'>search popularity of ",
           input$select_keyword, 
           ".</span> We show the correlation between the two and the number of
           days in the past when the search popularity is most strongly 
           correlated with COVID-19 ", tolower(input$select_covid_type), 
           " (Correlation Lag).")
    # 
    # paste0("For each country, we determine when search activity of ",
    #        input$select_keyword,
    #        " has the highest correlation with COVID-19 ",
    #        tolower(input$select_covid_type), 
    #        ". ",
    #        "Does search activity some days in the past have the strongest correlation
    #        with COVID ", tolower(input$select_covid_type), "? If so, search activity may
    #        predict future ", tolower(input$select_covid_type), ".")
    
    
    
  })
  
  output$which_keyword_title <- renderText({
    
    paste0("Which search terms are most correlated with COVID-19 ",
           input$select_covid_type, "?")
    
  })
  
  output$text_best_time_lag <- renderText({
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    df <- cor_df %>%
      
      dplyr::filter(type %in% input$select_covid_type) %>%
      filter(keyword_en %in% input$select_keyword) 
    
    txt <- paste0("Across countries, the search term popularity of ",
                  input$select_keyword,
                  " is most strongly correlated with COVID ",
                  input$select_covid_type, " ",
                  abs(round(mean(df$lag), 0)), " days ",
                  ifelse(round(mean(df$lag), 0) < 0, "into the past", "into the future"),
                  ".")
    
    txt
    
  })
  
  output$text_cor_countries <- renderText({
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    df <- cor_df %>%
      
      dplyr::filter(type %in% input$select_covid_type) %>%
      filter(keyword_en %in% input$select_keyword) 
    
    txt <- paste0("Across countries, the average correlation between the search popularity of ",
                  input$select_keyword,
                  " and COVID ",
                  input$select_covid_type, 
                  " is ",
                  round(mean(df$cor), 2),
                  ". ",
                  df$name[which.max(df$cor)],
                  " has the strongest correlation (",
                  max(df$cor) %>% round(2),
                  ").")
    
    txt
    
  })
  
  
  output$ui_line_graph <- renderUI({
    
    n_states <- readRDS(file.path("precomputed_figures", 
                                  paste0("stat_line_cor_N_countries",
                                         "_keyword", input$select_keyword,
                                         "_cases_deaths", input$select_covid_type,
                                         "_continent", input$select_continent,
                                         ".Rds")))
    
    n_states_div <- ceiling(n_states/5)
    
    plotOutput("line_graph",
               height = paste0(n_states_div*180,"px"))
    
  })
  
  output$ui_select_sort_by <- renderUI({
    
    selectInput(
      "select_sort_by",
      label = strong("Sort By"),
      choices = c("Name", input$select_covid_type, "Correlation"),
      selected = "Name",
      multiple = F
    )
    
  })
  
})

# RUN THE APP ==================================================================
shinyApp(ui, server)
