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

gtrends_df <- readRDS(file.path("data", "gtrends.Rds"))
gtrends_spark_df <- readRDS(file.path("data", "gtrends_spark.Rds"))
cor_df     <- readRDS(file.path("data", "correlations.Rds"))
world      <- readRDS(file.path("data", "world.Rds"))

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
                               unique(sort(world$continent))),
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
          
          column(2,
                 ),
          
          column(8,
                 
                 
                 htmlOutput("line_graph"),
                 #div(style = 'height:1000px; overflow-y: scroll',
                #     htmlOutput("line_graph")),
                 
                 
                 
          ),
          
          column(2,
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
  
  # * Trends Map ---------------------------------------------------------------
  output$increase_map <- renderPlotly({
    
    # p <- readRDS(file.path("precomputed_figures", 
    #                        paste0("fig_hits_change_map",
    #                               "_keyword", input$select_term_change,
    #                               "_cases_deaths", "Cases",
    #                               "_continent", input$select_continent_change,
    #                               ".Rds")))
    # 
    # p %>%
    #   ggplotly(tooltip = "text") %>%
    #   layout(plot_bgcolor='transparent',
    #          paper_bgcolor='transparent') %>%
    #   config(displayModeBar = F)
    
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
        dplyr::select(name, cases_new_spark, hits_ma7_spark, 
                      cor_casesMA7_hitsMA7_max, cor_casesMA7_hitsMA7_lag,
                      cases_total) %>%
        dplyr::rename(Country = name,
                      Cases = cases_new_spark,
                      "Search Activity" = hits_ma7_spark,
                      "Correlation" = cor_casesMA7_hitsMA7_max,
                      "Correlation Lag" = cor_casesMA7_hitsMA7_lag)
      
      print(head(gtrends_spark_df))
      
    } 
    if(input$select_covid_type %in% "Deaths"){
      
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(name, death_new_spark, hits_ma7_spark, 
                      cor_deathMA7_hitsMA7_max, cor_deathMA7_hitsMA7_lag,
                      death_total) %>%
        dplyr::rename(Country = name,
                      Deaths = death_new_spark,
                      "Search Activity" = hits_ma7_spark,
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
    f_list <- list(
      `name` = formatter("span", style = ~ style(color = "black")),
      `value` = formatter(
        "span",
        style = x ~ style(
          display = "inline-block",
          direction = "lft",
          font.weight = "bold",
          #"border-radius" = "4px",
          "padding-left" = "2px",
          "background-color" = csscolor(customRed0),
          width = percent(proportion(x)),
          color = csscolor("black")
        )
      )
    )
    
    ### Apply varable names
    #names(f_list)[1] <- admin_name
    #names(f_list)[2] <- var_name
    
    #names(data_for_table)[1] <- admin_name
    #names(data_for_table)[2] <- var_name
    
    # Make table
    # https://github.com/renkun-ken/formattable/issues/89
    table_max <- nrow(gtrends_spark_df)
    
    l <- formattable(
      gtrends_spark_df[1:table_max,] %>% as.data.table(),
      align = c("l", "l", "l"),
      f_list
    ) %>% format_table(align = c("l", "l", "l")) %>%
      htmltools::HTML() %>%
      div() %>%
      # use new sparkline helper for adding dependency
      spk_add_deps() %>%
      # use column for bootstrap sizing control
      # but could also just wrap in any tag or tagList
      {column(width=12, .)}
    
    l
    
    
  })
  
  # * Histogram - Time Lag Max Cor ---------------------------------------------
  output$cor_histogram_time_lag <- renderPlotly({
    
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
    
    p %>%
      ggplotly(tooltip = "text") %>%
      layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
  })
  
  
  # * Histogram - Correlation --------------------------------------------------
  output$cor_histogram <- renderPlotly({
    
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
    
    p %>%
      ggplotly(tooltip = "text") %>%
      layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
  })
  
  
  # * Histogram ----------------------------------------------------------------
  output$cor_map <- renderPlotly({
    
    # p <- readRDS(file.path("precomputed_figures", 
    #                        paste0("fig_cor_map",
    #                               "_keyword", input$select_keyword,
    #                               "_cases_deaths", input$select_covid_type,
    #                               "_continent", input$select_continent,
    #                               ".Rds")))
    # 
    # p %>%
    #   ggplotly(tooltip = "text") %>%
    #   layout(plot_bgcolor='transparent',
    #          paper_bgcolor='transparent') %>%
    #   config(displayModeBar = F)
    
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
