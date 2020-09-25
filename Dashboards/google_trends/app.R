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
#library(hrbrthemes)
#hrbrthemes::import_roboto_condensed()

keyword_list <- readRDS(file.path("precomputed_figures", 
                                  paste0("keyword_list",
                                         ".Rds")))

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
                 before going to a doctor
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
    
    # ** Figures ---------------------------------------------------------------
    tabPanel(
      "Figures",
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
                     
                     plotlyOutput("cor_histogram_time_lag",
                                  height = "200px")
              ),
              
              ## Hist 2
              column(4,
                     
                     plotlyOutput("cor_histogram",
                                  height = "200px"),
              ),
              
              ## Hist 3
              column(4,
                     plotlyOutput("cor_map",
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
                 strong(textOutput("trends_subtitle"), align = "center")
          ),
          column(1,
          )
        ),
        
        br(),
        
        
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
          
          column(12,
                 
                 
                 
                 uiOutput("ui_line_graph")
                 
                 
          )
          
        )
      )
    ),
    
    # ** Changes in Search Term ---------------------------------------------------------------
    tabPanel(
      "Warning System",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        h2("Warning System",
           align = "center"),
        
        fluidRow(
          column(8,
                 "This page shows the change in search term activity. We compare the
                  average search activity from the past week compared with the week before.",
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
                   choices = keyword_list,
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
    ),
    
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
    
    data_for_table <- readRDS(file.path("precomputed_figures",
                                        paste0("data_hits_change_table",
                                               "_keyword", input$select_term_change,
                                               "_cases_deaths", "Cases",
                                               "_continent", input$select_continent_change,
                                               ".Rds")))
    
    f_list <- list(
      `var1` = formatter("span", style = ~ style(color = "black")),
      `var2` = formatter("span", style = ~ style(color = "black")),
      `var3` = formatter("span", style = ~ style(color = "black")),
      `var4` = formatter("span", style = ~ style(color = "black"))
    )
    
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
    
    cases_deaths <<- input$select_covid_type
    keyword_en <<- input$select_keyword
    
    p <- readRDS(file.path("precomputed_figures",
                           paste0("fig_line",
                                  "_keyword", input$select_keyword,
                                  "_cases_deaths", input$select_covid_type,
                                  "_continent", input$select_continent,
                                  "_sort_by", input$select_sort_by,
                                  ".Rds")))
    
    #p 
    
    # 
    # n_countries <- length(unique(line_graph_df$title))
    # 
    # plt_one_country <- function(df){
    #   plot_ly(df) %>%
    #     add_trace(x = ~date,
    #               y = ~hits,
    #               type = 'scatter',
    #               mode = 'lines',
    #               line = list(color = 'green')) %>%
    #     add_trace(x = ~date,
    #               y = ~covid_new,
    #               type = 'bar',
    #               marker = list(color = 'orange')) %>%
    #     add_annotations(
    #       text = ~unique(Country),
    #       x = 0.5,
    #       y = 1,
    #       yref = "paper",
    #       xref = "paper",
    #       xanchor = "middle",
    #       yanchor = "top",
    #       showarrow = FALSE,
    #       font = list(size = 14)
    #     )
    # }
    # 
    # dfa %>%
    #   group_by(Country) %>%
    #   do(mafig = plt_one_country(.)) %>%
    #   subplot(nrows = n_countries) %>%
    #   layout(
    #     showlegend = FALSE,
    #     #title = '',
    #     width = 700,
    #     height = n_countries*100,
    #     hovermode = FALSE
    #   ) 
    # 
    # 
    
    # list(src = file.path("precomputed_figures", 
    #                      paste0("fig_line",
    #                             "_keyword", input$select_keyword,
    #                             "_cases_deaths", input$select_covid_type,
    #                             "_continent", input$select_continent,
    #                             "_sort_by", input$select_sort_by,
    #                             ".png")),
    #      contentType = 'image/png',
    #      width = 400,
    #      height = 250*19,
    #      alt = "This is alternate text")
    
    #})
    
    p
    
  }, bg="transparent")
  
  # * Histogram - Time Lag Max Cor ---------------------------------------------
  output$cor_histogram_time_lag <- renderPlotly({
    
    time_lag_best <- readRDS(file.path("precomputed_figures", 
                                       paste0("stat_time_lag_best",
                                              "_keyword", input$select_keyword,
                                              "_cases_deaths", input$select_covid_type,
                                              "_continent", input$select_continent,
                                              ".Rds")))
    
    time_lag_best <<- time_lag_best
    
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
  
  # * Max Correlation Hist -----------------------------------------------------
  
  output$us_ex_fig <- renderPlot({
    p <- readRDS(file.path("precomputed_figures",
                           paste0("fig_max_cor_hist",
                                  "_cases_deaths", input$select_covid_type,
                                  "_continent", input$select_continent,
                                  ".Rds")))
    
    p #%>%
    #ggplotly(tooltip = "text") %>%
    #layout(plot_bgcolor='transparent',
    #       paper_bgcolor='transparent') %>%
    #config(displayModeBar = F)
    
  })
  
  # * US Example Figure --------------------------------------------------------
  
  output$us_ex_fig <- renderPlot({
    p <- readRDS(file.path("precomputed_figures", "us_ex_fig.Rds"))
    
    p #%>%
    #ggplotly(tooltip = "text") %>%
    #layout(plot_bgcolor='transparent',
    #       paper_bgcolor='transparent') %>%
    #config(displayModeBar = F)
    
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
    
    paste0("For each country, we determine when search activity of ",
           input$select_keyword,
           " has the highest correlation with COVID-19 ",
           tolower(input$select_covid_type), 
           ". ",
           "Does search activity some days in the past have the strongest correlation
           with COVID ", tolower(input$select_covid_type), "? If so, search activity may
           predict future ", tolower(input$select_covid_type), ".")
    
    
    
  })
  
  output$which_keyword_title <- renderText({
    
    paste0("Which search terms are most correlated with COVID-19 ",
           input$select_covid_type, "?")
    
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
                  ifelse(best_time_lag < 0, "into the past", "into the future"),
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
