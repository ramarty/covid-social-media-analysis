# Google Trends Dashboard

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
                 take time to be released.",
                 offset = 2
                 )
          
        ),
        
       
        
        fluidRow(
          
          column(2,
                 
                 selectInput(
                   "select_keyword",
                   label = strong("Keyword"),
                   choices = gtrends_df$keyword_en %>% unique %>% sort(),
                   selected = "Loss of Smell",
                   multiple = F
                 ),
                 
                 selectInput(
                   "select_covid_type",
                   label = strong("Cases/Deaths"),
                   choices = c("Cases", "Deaths"),
                   selected = "Cases",
                   multiple = F
                 ),
                 
                 selectInput(
                   "select_continent",
                   label = strong("Continent"),
                   choices = c("All", world_sf$continent %>% unique()),
                   selected = "All",
                   multiple = F
                 ),
                 
                 uiOutput("ui_select_covid_cases"),
                 
                 numericInput("select_cor",
                              label = strong("Min. Correlation"),
                              value = 0,
                              min = 0
                 )
                 
                 ),
          
          column(6,
                 
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
      
    )
    
  )
)


# SERVER =======================================================================
server = (function(input, output, session) {
  
  # * gTrends Filtered ---------------------------------------------------------
  
  gtrends_r <- reactive({
    
    if(input$select_covid_type %in% "Cases"){
      gtrends_df$covid <- gtrends_df$cases_new
      gtrends_df$covid_ma7 <- gtrends_df$cases_ma7
      gtrends_df$covid_total <- gtrends_df$cases_total
      gtrends_df$covid_hits_cor <- gtrends_df$cases_hits_cor
    }
    
    if(input$select_covid_type %in% "Deaths"){
      gtrends_df$covid <- gtrends_df$death_new
      gtrends_df$covid_ma7 <- gtrends_df$death_ma7
      gtrends_df$covid_total <- gtrends_df$death_total
      gtrends_df$covid_hits_cor <- gtrends_df$death_hits_cor
    }
    
    gtrends_df$hits <- gtrends_df$hits_ma7 
    
    gtrends_sub_df <- gtrends_df %>%
      filter(covid_total >= input$select_covid_cases,
             covid_hits_cor >= input$select_cor,
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
  
  # * Line Graph ---------------------------------------------------------------
  output$line_graph <- renderPlot({
    
    p <- ggplot(gtrends_r(), aes(x = date)) +
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
    
    
    
    # use style to modify layer
    #p <- style(p, hoverinfo = 'none', traces = c(3))
    
    # use plotly_build to modify layer
    #p <- plotly_build(p)
    #str(p$x$layout$annotations) # check annotations
    #p$x$layout$annotations = NULL # remove annotation
    #p
    # https://stackoverflow.com/questions/54695153/fix-plotly-legend-position-and-disable-plotly-panel-for-shiny-in-rmarkdown
    
    
    
    # plot_ly(world_sf,
    #         color = ~covid_hits_cor,
    #         colors = 'Purples',
    #         stroke = I("black"),
    #         span = I(1)) %>%
    #   layout(plot_bgcolor='transparent',
    #          paper_bgcolor='transparent') %>%
    #   config(displayModeBar = F) %>%
    #   layout(legend = list(orientation = "h",   # show entries horizontally
    #                        xanchor = "center",  # use center of legend as anchor
    #                        x = 0.5))
    
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
  
  
  
  
  
  
  
  
  
})

# RUN THE APP ==================================================================
shinyApp(ui, server)
