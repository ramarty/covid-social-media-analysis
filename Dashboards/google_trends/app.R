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
library(tidyr)
library(shinyWidgets)
library(zoo)
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

# LOAD/PREP DATA ===============================================================

gtrends_df <- readRDS(file.path("data", "gtrends.Rds"))
world_sf <- readRDS(file.path("data", "world_ne.Rds"))
cor_df <- readRDS(file.path(DASHBOARD_PATH, "correlations.Rds"))
cor_max_df <- readRDS(file.path(DASHBOARD_PATH, "correlations_max_lag.Rds"))

# UI -==========================================================================
ui <- fluidPage(
  
  navbarPage(
    theme = shinytheme("cosmo"), # journal, flatly, sandstone
    collapsible = TRUE,
    title = "Google Trends",
    
    id = "nav",
    
    # ** Google Trends ---------------------------------------------------------
    tabPanel(
      "Search Term Correlate with COVID-19",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        h2("Search Terms Correlating with COVID-19 Cases and Deaths",
           align = "center"),
        
        fluidRow(
          column(8,
                 "This page shows how well different Google search terms correlate with COVID-19 cases and deaths. 
                 Predictive search terms can help track the spread of COVID-19, particularly when official data may
                 take time to be released. We explore how the strength of the correlation varies accross countries;
                 differences may arise due to different patterns of search behavior or people less frequently using Google.",
                 offset = 2
          )
          
        ),
        
        
        fluidRow(
          
          fluidRow(
            
            column(3,
                   
                   ),
            
            column(2,
                   
                   selectInput(
                     "select_keyword",
                     label = strong("Keyword"),
                     choices = gtrends_df$keyword_en %>% unique %>% sort(),
                     selected = "Loss of Smell",
                     multiple = F
                   )
            ),
            
            column(2,
                   
                   selectInput(
                     "select_covid_type",
                     label = strong("Cases/Deaths"),
                     choices = c("Cases", "Deaths"),
                     selected = "Cases",
                     multiple = F
                   )
            ),
            
            column(2,
                   
                   
                   selectInput(
                     "select_continent",
                     label = strong("Continent"),
                     choices = c("All", world_sf$continent %>% unique()),
                     selected = "All",
                     multiple = F
                   )
            ),
            
          )
          
        ),
        
        
        fluidRow(
          
          column(8,
                 
                 column(12, align = "center",
                        uiOutput("ui_sort_by")
                 ),
                 
                 uiOutput("ui_line_graph")
                 
                 
          ),
          
          column(4,
                 
                 wellPanel(
                   
                   h4(textOutput("cor_title_text"),
                      align = "center"),
                   
                   column(12, strong("Distribution"), align = "center"),
                   
                   plotlyOutput("cor_histogram",
                                height = "200px"),
                   
                   column(12, strong("Map"), align = "center"),
                   
                   plotlyOutput("cor_map",
                                height = "200px")
                   
                   , style = "padding: 2px;"
                 )
          )
        )
      )
    ),
    
    # ** Google Trends ---------------------------------------------------------
    tabPanel(
      "Increase in Search Term",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        h2("Increase in Search Term",
           align = "center"),
        
        fluidRow(
          column(8,
                 "DESCRIPTION HERE.",
                 offset = 2
          ),
        ),
        
        fluidRow(
          
          column(4,
                 # BLANK for offsetting
          ),
          
          column(2,
                 selectInput(
                   "select_term_increase",
                   label = strong("Select Term"),
                   choices = c("Loss of Smell", "Fever"),
                   selected = "Loss of Smell",
                   multiple = F
                 )
          ),
          
          column(2,
                 selectInput(
                   "select_continent_increase",
                   label = strong("Continent"),
                   choices = c("All", world_sf$continent %>% unique()),
                   selected = "All",
                   multiple = F
                 )
          )
          
        ),
        
        fluidRow(
          column(12,
                 
                 plotlyOutput("increase_map"),
                 
                 div(style = 'height:1000px; overflow-y: scroll',
                     htmlOutput("cor_table"))
                 
          )
          
          
        )
        
        
      )
    )
  )
)


# SERVER =======================================================================
server = (function(input, output, session) {
  
  # * gTrends Filtered ---------------------------------------------------------
  gtrends_r <- reactive({
    
    if(input$select_covid_type %in% "Cases"){
      gtrends_df$covid <- gtrends_df$cases_new
      gtrends_df$covid_ma7 <- gtrends_df$cases_new_ma7
      gtrends_df$covid_total <- gtrends_df$cases_total
      gtrends_df$covid_hits_cor <- gtrends_df$cases_hits_cor
    }
    
    if(input$select_covid_type %in% "Deaths"){
      gtrends_df$covid <- gtrends_df$death_new
      gtrends_df$covid_ma7 <- gtrends_df$death_new_ma7
      gtrends_df$covid_total <- gtrends_df$death_total
      gtrends_df$covid_hits_cor <- gtrends_df$death_hits_cor
    }
    
    gtrends_df$hits <- gtrends_df$hits_ma7 
    
    gtrends_sub_df <- gtrends_df %>%
      filter(#covid_total >= input$select_covid_cases,
        #covid_hits_cor >= input$select_cor,
        keyword_en %in% input$select_keyword)
    
    gtrends_sub_df <- gtrends_sub_df %>%
      group_by(geo) %>%
      mutate(hits = hits / max(hits, na.rm = T)) %>% # ensure max is 1 (for eg, for moving avg)
      mutate(hits = hits * max(covid)) %>%
      ungroup() 
    
    if(!(input$select_continent %in% "All")){
      gtrends_sub_df <- gtrends_sub_df[gtrends_sub_df$continent %in% input$select_continent,]
    }
    
    gtrends_sub_df
    
  })
  
  # * Last 7 Days Reactive -----------------------------------------------------
  gtrends_recent <- reactive({
    
    #### Subset to last 14 days and add variable indicating first or second week
    last_14_days <- gtrends_df$date %>% 
      unique %>% 
      sort() %>% 
      tail(14)
    
    gtrends_df$hits <- gtrends_df$hits_ma7
    
    first_week  <- last_14_days %>% head(7)
    second_week <- last_14_days %>% tail(7)
    
    gtrends_sub_df <- gtrends_df[gtrends_df$date %in% last_14_days,]
    gtrends_sub_df <- gtrends_sub_df[gtrends_sub_df$keyword_en %in% "Loss of Smell",]
    
    gtrends_sub_df$week <- NA
    gtrends_sub_df$week[gtrends_sub_df$date %in% first_week] <- 1
    gtrends_sub_df$week[gtrends_sub_df$date %in% second_week] <- 2
    
    #### Table
    gtrends_sub_df <- gtrends_sub_df %>%
      group_by(geo) %>%
      mutate(week_1_hits = mean(hits[week %in% 1]),
             week_2_hits = mean(hits[week %in% 2])) %>%
      ungroup() %>%
      mutate(increase = week_2_hits - week_1_hits) 
    
    gtrends_sub_df
    
  })
  
  # * Trends Map ---------------------------------------------------------------
  output$increase_map <- renderPlotly({
    
    increase_df <- gtrends_sub_df %>% 
      distinct(Country, geo, increase)
    
    world_sf <- merge(world_sf, increase_df, by = "geo", all.x = T, all.y = F)
    
    world_sf$text <- paste0(world_sf$name, "\n", world_sf$increase %>% round(2))
    
    if(!(input$select_continent %in% "All")){
      world_sf <- world_sf[world_sf$continent %in% input$select_continent,]
    }
    
    p <- ggplot() +
      geom_sf(data = world_sf,
              aes(fill = increase,
                  text = text),
              color = NA) +
      
      #  scale_fill_gradientn(colors = brewer.pal(n = 9, name = "RdYlGn")) +
      
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
            axis.line = element_blank()) +
      coord_map(
        projection = "mercator")
    
    p %>%
      ggplotly(tooltip = "text") %>%
      layout(plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
    
    
    
  })
  
  
  
  # * Trends Table --------------------------------------------------------
  output$cor_table <- renderUI({
    
    #### Define Colors
    customGreen = "#71CA97"
    customGreen0 = "#DeF7E9"
    customRed = "#ff7f7f"
    customRed0 = "#FA614B66"
    customGreen0 = "#DeF7E9"
    customYellow = "goldenrod2"
    
    data_for_table <- gtrends_recent() %>%
      group_by(Country, increase, cases_total) %>%
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
      dplyr::select(Country, cases_total, increase, week_2_hits, TrendSparkline) %>%
      arrange(-increase)
    
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
    
    # https://github.com/renkun-ken/formattable/issues/89
    
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
    
    l
    
  })
  
  # * Line Graph ---------------------------------------------------------------
  output$line_graph <- renderPlot({
    
    df <- gtrends_r()
    
    if(input$sort_by %in% c("Cases", "Deaths")){
      df$Country <- df$Country %>% as.factor() %>% reorder(-df$covid)
    }
    
    if(input$sort_by %in% "Correlation"){
      df$Country <- df$Country %>% as.factor() %>% reorder(-df$covid_hits_cor)
    }
    
    p <- ggplot(df, aes(x = date)) +
      geom_col(aes(y = covid, fill = paste("COVID-19", input$select_covid_type))) +
      geom_line(aes(y = hits, color = paste0("Search Popularity of ", input$select_keyword))) +
      facet_wrap(~Country,
                 scales = "free_y",
                 ncol = 2) +
      scale_fill_manual(values = "orange1") +
      scale_color_manual(values = "green4") +
      labs(x = "", y = paste("COVID-19", input$select_covid_type),
           fill = "", color = "") +
      theme_ipsum() + 
      theme(legend.position="top",
            legend.text = element_text(size=14))
    
    #ggplotly(p)
    p
    
  }, bg="transparent")
  
  # * Histogram ----------------------------------------------------------------
  output$cor_histogram <- renderPlotly({
    
    #print(head(gtrends_r()))
    #print(gtrends_r()$covid_hits_cor)
    
    df <- gtrends_r() %>%
      dplyr::mutate(bins = round(covid_hits_cor*100, digits=-1) / 100) %>%
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
    
    p %>%
      ggplotly(tooltip = "text") %>%
      layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
  })
  
  
  # * Histogram ----------------------------------------------------------------
  output$cor_map <- renderPlotly({
    
    df <- gtrends_r() 
    df <- df %>%
      distinct(geo, covid_hits_cor)
    
    world_sf <- merge(world_sf, df, by = "geo", all.x = T, all.y = F)
    
    world_sf$text <- paste0(world_sf$name, "\n", world_sf$covid_hits_cor %>% round(2))
    
    if(!(input$select_continent %in% "All")){
      world_sf <- world_sf[world_sf$continent %in% input$select_continent,]
    }
    
    p <- ggplot() +
      geom_sf(data = world_sf,
              aes(fill = covid_hits_cor,
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
            axis.line = element_blank()) +
      coord_map(
        projection = "mercator")
    
    p %>%
      ggplotly(tooltip = "text") %>%
      layout(plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
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
  
  output$cor_title_text <- renderText({
    
    paste0("Correlation between search popularity of ",
           input$select_keyword, 
           " and COVID-19 ",
           input$select_covid_type)
    
  })
  
  
  output$ui_line_graph <- renderUI({
    
    df <- gtrends_r()
    n_states <- df$geo %>% unique %>% length()
    
    rows <- round(n_states/2)
    
    plotOutput("line_graph",
               height = paste0(rows/2*300,"px"))
  })
  
  output$ui_sort_by <- renderUI({
    
    selectInput(
      "sort_by",
      label = strong("Sort By"),
      choices = c("Name", input$select_covid_type, "Correlation"),
      selected = "Name",
      multiple = F
    )
    
  })
  
  
  
  
  
  
  
  
  
})

# RUN THE APP ==================================================================
shinyApp(ui, server)
