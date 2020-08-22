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
                 "This page shows (1) how well different Google search terms correlate with COVID-19 cases and deaths 
                 and (2) the time lag of Google search terms where the correlation is strongest. Trends in search terms 
                 correlated with COVID cases/deaths can be used to indicate possible increases or decreases in COVID cases/deaths.
                 Search term trends should not subsitute for official data; however, predictions may be useful when official 
                 data may take time to be captured.",
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
                     choices = c("Loss of Smell",
                                 "I Can't Smell",
                                 "Fever",
                                 "Cough"),
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
                     choices = c("All",
                                 "Asia",
                                 "Africa",
                                 "Europe",
                                 "South America",
                                 "Oceania",
                                 "North America"),
                     selected = "All",
                     multiple = F
                   )
            ),
            
          )
          
        ),
        
        
        fluidRow(
          
          column(8,
                 
                 column(12, align = "center",
                        uiOutput("ui_select_sort_by")
                 ),
                 
                 uiOutput("ui_line_graph")
                 
                 
          ),
          
          column(4,
                 
                 wellPanel(
                   
                   h4(textOutput("cor_title_text"),
                      align = "center"),
                   
                   column(12, strong(textOutput("text_best_time_lag")), align = "center"),
                   
                   br(),
                   br(),
                   br(),
                   
                   plotlyOutput("cor_histogram_time_lag",
                                height = "200px"),
                   
                   column(12, strong(textOutput("text_cor_countries")), align = "center"),

                   br(),
                   br(),
                   br(),
                   
                   plotlyOutput("cor_histogram",
                                height = "200px"),
                   
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
      "Changes in Search Term",
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
                   "select_term_change",
                   label = strong("Select Term"),
                   choices = c("Loss of Smell",
                               "I Can't Smell",
                               "Fever",
                               "Cough"),
                   selected = "Loss of Smell",
                   multiple = F
                 )
          ),
          
          column(2,
                 selectInput(
                   "select_continent_change",
                   label = strong("Continent"),
                   choices = c("All",
                               "Asia",
                               "Africa",
                               "Europe",
                               "South America",
                               "Oceania",
                               "North America"),
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
  
  # * Trends Map ---------------------------------------------------------------
  output$increase_map <- renderPlotly({
    
    p <- readRDS(file.path("precomputed_figures", 
                           paste0("fig_hits_change_map",
                                  "_keyword", input$select_term_change,
                                  "_cases_deaths", "Cases",
                                  "_continent", input$select_continent_change,
                                  ".Rds")))
    
    p %>%
      ggplotly(tooltip = "text") %>%
      layout(plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
  })
  
  # * Trends Table --------------------------------------------------------
  output$cor_table <- renderUI({
    
    l <- readRDS(file.path("precomputed_figures", 
                           paste0("fig_hits_change_table",
                                  "_keyword", input$select_term_change,
                                  "_cases_deaths", "Cases",
                                  "_continent", input$select_continent_change,
                                  ".Rds")))
    
    l
    
  })
  
  # * Line Graph ---------------------------------------------------------------
  output$line_graph <- renderPlot({
    
    p <- readRDS(file.path("precomputed_figures", 
                           paste0("fig_line_cor",
                                  "_keyword", input$select_keyword,
                                  "_cases_deaths", input$select_covid_type,
                                  "_continent", input$select_continent,
                                  "_sort_by", input$select_sort_by,
                                  ".Rds")))
    p
    
  }, bg="transparent")
  
  # * Histogram - Time Lag Max Cor ---------------------------------------------
  output$cor_histogram_time_lag <- renderPlotly({
    
    p <- readRDS(file.path("precomputed_figures", 
                           paste0("fig_time_lag_hist",
                                  "_keyword", input$select_keyword,
                                  "_cases_deaths", input$select_covid_type,
                                  "_continent", input$select_continent,
                                  ".Rds")))
    
    p %>%
      ggplotly(tooltip = "text") %>%
      layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
  })
  
  
  # * Histogram - Correlation --------------------------------------------------
  output$cor_histogram <- renderPlotly({
    
    p <- readRDS(file.path("precomputed_figures", 
                           paste0("fig_cor_hist",
                                  "_keyword", input$select_keyword,
                                  "_cases_deaths", input$select_covid_type,
                                  "_continent", input$select_continent,
                                  ".Rds")))
    
    p %>%
      ggplotly(tooltip = "text") %>%
      layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
  })
  
  
  # * Histogram ----------------------------------------------------------------
  output$cor_map <- renderPlotly({
    
    p <- readRDS(file.path("precomputed_figures", 
                           paste0("fig_cor_map",
                                  "_keyword", input$select_keyword,
                                  "_cases_deaths", input$select_covid_type,
                                  "_continent", input$select_continent,
                                  ".Rds")))
    
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
  
  output$text_best_time_lag <- renderText({
    
    best_time_lag <- readRDS(file.path("precomputed_figures", 
                                       paste0("stat_time_lag_best",
                                              "_keyword", input$select_keyword,
                                              "_cases_deaths", input$select_covid_type,
                                              "_continent", input$select_continent,
                                              ".Rds")))
    
    txt <- paste0("Across countries, the search term popularity of ",
                  input$select_keyword,
                  " is most strongly correlated with COVID ",
                  input$select_covid_type, " ",
                  abs(best_time_lag), " days ",
                  ifelse(best_time_lag < 0, "before", "after"),
                  ".")
    
    txt
    
  })
  
  output$text_cor_countries <- renderText({
    
    cor_country <- readRDS(file.path("precomputed_figures",
                                     paste0("stat_cor_countries",
                                            "_keyword", input$select_keyword,
                                            "_cases_deaths", input$select_covid_type,
                                            "_continent", input$select_continent,
                                            ".Rds")))
    
    txt <- paste0("Across countries, the average correlation between the search popularity of ",
                   input$select_keyword,
                   " and COVID ",
                   input$select_covid_type, 
                   " is ",
                   round(mean(cor_country$cor_covid_new), 2),
                   ". ",
                   cor_country$Country[which.max(cor_country$cor_covid_new)],
                   " has the strongest correlation (",
                   max(cor_country$cor_covid_new) %>% round(2),
                   ").")
    
    txt
    
  })
  
  
  
  
  
  
  
  
  
  output$ui_line_graph <- renderUI({
    
    n_states <- readRDS(file.path("precomputed_figures", paste0("stat_line_cor_N_countries",
                                                                "_keyword", input$select_keyword,
                                                                "_cases_deaths", input$select_covid_type,
                                                                "_continent", input$select_continent,
                                                                ".Rds")))
    
    plotOutput("line_graph",
               height = paste0(n_states*150,"px"))
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
