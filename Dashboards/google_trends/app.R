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

cor_after_dates <- c("2020-02-01",
                     "2020-03-01",
                     "2020-04-01",
                     "2020-05-01",
                     "2020-06-01",
                     "2020-07-01",
                     "2020-08-01")

# Defaults
gtrends_df <- readRDS(file.path("data", paste0("gtrends_since_",cor_after_dates[1],".Rds")))
gtrends_spark_df <- readRDS(file.path("data", paste0("gtrends_spark_since_",cor_after_dates[1],"_large.Rds")))
cor_df     <- readRDS(file.path("data", paste0("correlations_since_",cor_after_dates[1],".Rds")))
world      <- readRDS(file.path("data", "world.Rds"))

LOAD_GTRENDS_INIT <- TRUE

keyword_list <- gtrends_df$keyword_en %>% unique()

keywords_df <- read_csv(file.path("data", "covid_keywords.csv"))
languges_df <- read_csv(file.path("data", "countries_lang.csv"))

## Prep keywords
keywords_df$keyword_en <- keywords_df$keyword_en %>% tools::toTitleCase()
keywords_df <- keywords_df[keywords_df$keyword_en %in% gtrends_df$keyword_en,]
keywords_df$category <- keywords_df$category %>% tools::toTitleCase() 

keywords_clean_df <- keywords_df %>%
  dplyr::rename(Portuguese = keyword_pt,
                English = keyword_en,
                Spanish = keyword_es,
                French = keyword_fr,
                Arabic = keyword_ar,
                German = keyword_de,
                Mandarin = keyword_zh,
                Dutch = keyword_nl,
                Italian = keyword_it,
                Norwegian = keyword_no,
                Swedish = keyword_sv,
                Russian = keyword_ru,
                Greek = keyword_el,
                Turkish = keyword_tr) %>%
  dplyr::filter(scrape %in% c("yes")) %>%
  dplyr::select(English, Spanish, Portuguese, French, Arabic, German, Mandarin,
                Dutch, Italian, Norwegian, Swedish, Russian, Greek, Turkish) 

# FUNCTIONS ========
gtpath <- ""

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
          column(12, align = "center",
                 h1("Google Trends Data as Predictor of COVID-19")
          )
        ),
        
        fluidRow(
          column(6, align = "center", offset = 3,
                 
                 hr(),
                 h2("Overview"),
                 
                 HTML("<h4>When people fall sick, 
                 <a href='https://blog.google/technology/health/using-symptoms-search-trends-inform-covid-19-research/'>many turn to Google</a>
                 before considering medical attention
                 to understand their symptoms and see options for home treatments.
                    Using data from Jan 1 - September 20, 2020, this dashboard illustrates how search activity for specific symptoms
                    strongly matches - and often preceeds - trends in COVID-19 cases.</h4>"),
                 br(),
                 
                 HTML("<h4>Trends in search popularity of COVID-19 symptoms should not replace
                 administrative data on cases. The relation between the two is strong
                 but <a href='https://www.nature.com/news/when-google-got-flu-wrong-1.12413'>not perfect</a>. However, Google data can supplement official data.
                This is particularly true in circumstances
                    when testing or data may not be widely available. Moreover, given that
                    Google trends information is updated in real time, sudden increases in 
                    search activity can warn of potential growth in COVID-19 cases.</h4>"),
          )
        ),
        fluidRow(
          column(6, align = "center", offset = 3,
                 hr(),
                 h2("Determining correlation and prediction between COVID-19 and Google search term interest"),
                 HTML("<h4>We compute how strongly different search terms correlate with COVID-19 cases and deaths.
                      In addition, we determine whether search interest can help predict future cases or deaths
                      or whether search interest responds or comes after cases/deaths. To determine this, we shift COVID-19 cases/deaths
                      by up to 21 days from its actual date. We calculate the correlation between
                      the shifted COVID-19 and the search interest. Using all these estimated correlations, we determine the 
                      following metrics:</h4>")
                 # HTML("<h4>For each search term, we seek to understand (1) the strength of the correlation
                 #      between COVID-19 cases/deaths and Google search term interest and (2) when the correlation
                 #      is strongest.</h4"),
                 # br(),
                 # HTML("<h4>We shift search interest by up to 21 days from its actual date. We calculte the correlation
                 #      between COVID cases/deaths and the shifted search interest. The following values are determined:</h4>")
          )
        ),
        fluidRow(
          column(6, align = "left", offset = 3,
                 HTML("<ul>
                      <li><h4><b>Maximum Correlation</b></h4></li>
                      <li><h4><b>Lead/Lag Days:</b> The number of days COVID-19 cases/deaths was shifted to obtain the maximum correlation.
                      <b>Negative values</b> mean that search interest comes before COVID-19, helping to predict cases/deaths while
                      <b>positive values</b> indicate the search interest reacts to COVID-19 cases/deaths</h4></li>
                      <li><h4><b>Z-Score of Lead/Lag Days:</b> How different the maximum correlation is from other correlations. We calcualte the <b> z-score </b>, 
                 or the number of standard deviations the maximum correlation is from the average correlation. 
                 <b>Large z-scores</b> (typically around 1.9 or higher) indicate that the chosen lead/lag day is certain while
                 <b>small z-scores</b> indicate that the chosen lead/lag day isn't that different from others, so the chosen lead/lag day is relatively arbitrary</li>
                      </ul>"),
                 
                 # br(),
                 #HTML("<h4>To understand this, we shift search interest -21 to 21 days from its actual date. We use the 'shift'
                 #      with the best correlation. Using all estimated correlations, we calculate the z-score of the best correlation. <b>High z-scores</b>
                 #      indicate greater confidence in the optimal shift. The below figure illustrates the approach.</h4>"),
                 br()
          )
        ),
        
        fluidRow(
          column(12, align = "center",
                 HTML("<strong>Correlation between <span style='color:orange;'>COVID-19 cases</span> (shifted) and <span style='color:green;'>Search Term Interest</span></strong>"),
                 img(src="cor.gif", width='70%')
          )
        ),
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br()
        
        
        # fluidRow(
        #   column(6, align = "center", offset = 3,
        #          hr(),
        #          h2("Example"),
        #          
        #          h4("As an example, growth in the search popularity of 'Loss of Smell'
        #             preceded an increase in COVID-19 cases by about 10 days in the 
        #             United States in mid-June. The 
        #             dashboard shows this trend holds across many countries.")
        #   )
        # ),
        # 
        # fluidRow(
        #   #column(12,
        #   #       ),
        #   column(12, align = "center",
        #          
        #          plotOutput("us_ex_fig",
        #                     height = "350px",
        #                     width = "700px")
        #          
        #   )
        #   #column(2,
        #   #),
        #   
        #   
        # )
        
      )
    ),
    
    # ** Global Level ----------------------------------------------------------
    tabPanel(
      "Global Level",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        # fluidRow(
        #   column(6, align = "center", offset = 3,
        #          HTML("<b><em>Choose to examine COVID cases or deaths, restrict the analysis to
        #                 a specific continent, only consier the correlation between COVID-19 and search
        #               interest after a specific date and restrict to a select category of keywords.</em></b>")
        #   ),
        # ),
        # 
        # br(),
        
        fluidRow(
          column(2, align = "center", offset = 2,
                 selectInput(
                   "select_continent",
                   label = strong("Continent"),
                   choices = c("All",
                               unique(sort(cor_df$continent))),
                   selected = "All",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_covid_type",
                   label = strong("Cases/Deaths"),
                   choices = c("Cases", "Deaths"),
                   selected = "Cases",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_begin_date",
                   label = strong("Correlation After"),
                   choices = cor_after_dates,
                   selected = "2020-02-01",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_search_category",
                   label = strong("Search Term Categories"),
                   choices = c("All",
                               sort(unique(keywords_df$category))),
                   selected = "All",
                   multiple = F
                 )
          )
          
        ),
        
        hr(),
        
        h2(textOutput("which_keyword_title"),
           align = "center"),
        
        fluidRow(
          column(4,
          ),
          column(4, align = "center",
                 strong(textOutput("cor_distribution_text")),
          ),
          column(4,
          )
        ),
        
        fluidRow(
          column(6, offset = 3,
                 uiOutput("max_cor_hist_ui")
          ),
        ),
        
        hr(),
        
        fluidRow(
          
          column(4, align = "center", offset = 4,
                 uiOutput("select_keyword_ui")
          )
          
        ),
        
        br(),
        
        fluidRow(
          column(4, align = "center", offset = 2,
                 plotOutput("keyword_cor", height = "100px")
          ),
          column(4, align = "center",
                 plotOutput("keyword_lag", height = "100px")
          )
        ),
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel(tags$div( 
                      
                      HTML(paste(tags$span(style="color:white", "--------------------------------------------------------------------------------------"), sep = "")) 
                      
                    )),
                    
                    tabPanel(id = "map_view",
                             "Map View", 
                             
                             fluidRow(
                               column(12, align = "center", offset = 0,
                                      #strong("Click a country on the map"),
                                      uiOutput("cor_map_leaflet")
                                      
                               )
                             )
                             
                    ),
                    tabPanel(id = "table_view",
                             "Table View",
                             
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
                             
                             
                    ), selected = "Map View"),
        
        fluidRow(
          column(12, align = "center",
                 
                 #strong(htmlOutput("dummy_spark"))
                 
          )
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
          column(2, align = "center", offset = 2,
                 
                 selectizeInput(
                   "select_country",
                   label = strong("Select Country"),
                   choices = c("", sort(cor_df$name)),
                   selected = "",
                   multiple = F
                 )#,
          ),
          
          column(2, align = "center",
                 selectInput(
                   "select_covid_type_map",
                   
                   label = strong(HTML("Cases/Deaths")),
                   choices = c("Cases", "Deaths"),
                   selected = "Cases",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_begin_date_country",
                   label = strong("Correlation After"),
                   choices = cor_after_dates,
                   selected = "2020-02-01",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_search_category_country",
                   label = strong("Search Term Categories"),
                   choices = c("All",
                               sort(unique(keywords_df$category))),
                   selected = "All",
                   multiple = F
                 )
          )
        ),
        
        br(),
        
        fluidRow(
          column(6, align = "center", offset = 3,
                 strong(htmlOutput("trends_country_subtitle"), align="center"),
          )
        ),
        
        br(),
        
        fluidRow(
          column(8, align = "center", offset = 2,
                 div(style = 'overflow-y: scroll; height:300px', htmlOutput("line_graph_country"))
          )
        ),
        hr(),
        
        wellPanel(
          fluidRow(
            column(4, align = "center", offset = 4,
                   uiOutput("select_keyword_country_ui")
            )
          ),
          
          fluidRow(
            column(6, align = "center", offset = 3,
                   h4(textOutput("translation_text"))
            )
          ),
          
          fluidRow(
            column(5, align = "center",
                   h3("Historic Trends"),
                   strong("Data Available Until September 20, 2020")
            ),
            column(7, align = "center",
                   h3("Real Time Data: Search Interest in Past 90 Days"),
                   fluidRow(
                     column(8, align = "center", offset = 2,
                            HTML("<strong>Trends at different time spans and at subnational levels can be
                            explored on the 
                            <a href='https://trends.google.com/trends/'>Google Trends website.</a></strong>")
                     )
                   )
            )
          ),
          
          fluidRow(
            column(5, align = "left", 
                   br(),
                   # fluidRow(
                   #   column(12, align = "center",
                   #          htmlOutput("line_graph_country_key_title_1")
                   #          )
                   # ),
                   fluidRow(
                     column(6, align = "left", offset = 3,
                            htmlOutput("line_graph_country_key_title_2")
                     )
                   ),
                   
                   plotOutput("line_graph_country_key",
                              height = "225px")
            ),
            column(4, align = "center",
                   tags$div(id="wrapper"),
                   
                   uiOutput("gtrends_html_trends")
            ),
            column(3, align = "center",
                   tags$div(id="wrapper2"),
                   uiOutput("gtrends_html_map")
            )
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
          column(6, offset = 3,
                 h2("Data", align = "center"),
                 HTML("We access COVID-19 Cases and Deaths from the
                      <a href='https://covid19.who.int/?gclid=Cj0KCQjw8fr7BRDSARIsAK0Qqr73Wij8AiyjGx8dOs-MYxN7oxF5pzYmurbdVxj-x65Gc8tx1jJykaYaAqQNEALw_wcB'>WHO</a>
                      and download Google Trends data for all countries. To protect privacy, Google
                      only releases search interest data when there is a large enough search volume for a
                      specific search term. We translate search terms from English into
                      each countries most widely used language using Google Translate.
                      The below table shows which language is used for each country."),
                 
                 
          )
        ),
        fluidRow(
          br(),
          column(6, offset = 3, align = "center",
                 div(style = 'overflow-y: scroll; height:300px', tableOutput('language_table'))
          )
        ),
        
        fluidRow(
          column(6, offset = 3,
                 hr(),
                 h2("Subnational Case Study: Brazil", align = "center"),
                 HTML("While the dashboard shows country level results, the project
                 team also found that Google search term interest also correlates
                 with COVID-19 at the subnational level using Brazil as a <a href='https://drive.google.com/file/d/1-DrtOdFdKCv99G-w3zHK0VyDK65GqEpJ/view?usp=sharing'>case study.</a>")
          )
        ),
        
        fluidRow(
          column(6, offset = 3,
                 hr(),
                 h2("References", align = "center"),
                 
                 HTML("This dashboard builds off of a literature that
                      uses Google Trends to provide insight into COVID-19.
                      Studies that were used to inform the dashboard include
                      the following"),
                 
                 
                 # <li><a href='URL'>TITLE</a></li>
                 HTML("<br><br><ul>
                      <li><a href='https://www.sciencedirect.com/science/article/pii/S1201971220302496'>Association of the COVID-19 pandemic with Internet Search Volumes: A Google Trends(TM) Analysis</a></li>
                      <li><a href='https://www.medrxiv.org/content/10.1101/2020.05.07.20093955v2'>Utility and limitations of Google searches for tracking disease: the case of taste and smell loss as markers for COVID-19</a></li>
                      <li><a href='https://www.nytimes.com/2020/04/05/opinion/coronavirus-google-searches.html'>Google Searches Can Help Us Find Emerging Covid-19 Outbreaks</a></li>
                      <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7189861/'>The COVID-19 outbreak and Google searches: Is it really the time to worry about global mental health?</a></li>
                      <li><a href='https://ideas.repec.org/p/cep/cepdps/dp1693.html'>COVID-19, Lockdowns and Well-being: Evidence from Google Trends</a></li>
                      <li><a href='https://pubmed.ncbi.nlm.nih.gov/32279437/'>Use of Google Trends to investigate loss-of-smell-related searches during the COVID-19 outbreak</a></li>
                      <li><a href='https://europepmc.org/article/pmc/pmc7267744'>Predicting COVID-19 Incidence Using Anosmia and Other COVID-19 Symptomatology: Preliminary Analysis Using Google and Twitter.</a></li>
                      </ul>"),
          )
        ),
        
        fluidRow(
          column(12,
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br()
                 )
        )
        
        
      )
    )
    
  )
)

# SERVER =======================================================================
server = (function(input, output, session) {
  
  # GLOBAL FIGURES ***************** -------------------------------------------
  
  # ** Global Correlation Map --------------------------------------------------
  output$cor_map_leaflet <- renderUI({
    
    gtrends_spark_df <- readRDS(file.path("data", paste0("gtrends_spark_since_",
                                                         input$select_begin_date,"_large.Rds")))
    
    #req(input$nav == "Map View") # This makes leaflet show up; before no defaults.
    
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
      gtrends_spark_df$cor_covidMA7_hitsMA7_zscore <- gtrends_spark_df$cor_casesMA7_hitsMA7_zscore %>% round(3)
    } else{
      gtrends_spark_df$l_covid_hits <- gtrends_spark_df$l_death_hits
      gtrends_spark_df$cor_covidMA7_hitsMA7_max <- gtrends_spark_df$cor_deathMA7_hitsMA7_max
      gtrends_spark_df$cor_covidMA7_hitsMA7_lag <- gtrends_spark_df$cor_deathMA7_hitsMA7_lag
      gtrends_spark_df$cor_covidMA7_hitsMA7_zscore <- gtrends_spark_df$cor_deathMA7_hitsMA7_zscore %>% round(3)
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
      paste0("<br><b>Lead/Lag:</b> ", world_data$cor_covidMA7_hitsMA7_lag[!is.na(world_data$cor_covidMA7_hitsMA7_lag)], " days")
    
    world_data$cor_zscore <- ""
    world_data$cor_zscore[!is.na(world_data$cor_covidMA7_hitsMA7_zscore)] <-
      paste0("<br><b>Z-Score:</b> ", world_data$cor_covidMA7_hitsMA7_zscore[!is.na(world_data$cor_covidMA7_hitsMA7_zscore)],
             "<br>")
    
    world_data$cor_keyword <- ""
    world_data$cor_keyword[!is.na(world_data$keyword)] <-
      paste0("<b><em>", world_data$keyword[!is.na(world_data$keyword)], "</em></b>")
    
    world_data$l_covid_hits[is.na(world_data$l_covid_hits)] <- "<em>Low Google search activity<br>for this search term</em>"
    
    world_data$popup <- paste0("<h4>", world_data$name, "</h4>", 
                               world_data$cor_keyword,
                               world_data$cor, 
                               world_data$cor_lag, 
                               world_data$cor_zscore,
                               world_data$l_covid_hits)
    
    # https://stackoverflow.com/questions/61175878/r-leaflet-highcharter-tooltip-label
    pal <- colorNumeric(
      palette = "RdYlGn",
      domain = c(world_data$cor_covidMA7_hitsMA7_max[!is.na(world_data$cor_covidMA7_hitsMA7_max)], 0, 1))
    
    #aa <<- world_data
    
    leaflet(height = "700px") %>%
      addTiles() %>%
      addPolygons(data = world_data,
                  label = ~lapply(popup, HTML),
                  popupOptions = popupOptions(minWidth = 200,
                                              maxHeight = 150),
                  stroke = F,
                  fillOpacity = 1,
                  color = ~pal(cor_covidMA7_hitsMA7_max)) %>%
      onRender("function(el,x) {
      this.on('tooltipopen', function() {HTMLWidgets.staticRender();})
    }") %>%
      #   onRender("function(el,x) {
      #   this.on('popupopen', function() {HTMLWidgets.staticRender();})
      # }") %>%
      addLegend("topright",
                pal = pal,
                values = c(world_data$select_covid_type[!is.na(world_data$select_covid_type)], 0, 1),
                title = paste0("Correlation<br>between<br>",
                               input$select_covid_type,
                               " and<br>Search<br>Activity"),
                opacity = 1,
                bins = c(-1, -.5, 0, 0.5, 1)) %>%
      setView(zoom = 2, lat=0, lng=0) %>%
      add_deps("sparkline") %>%
      #add_deps("highchart", 'highcharter') %>%
      browsable()
    #) #%>%
    #add_deps("sparkline") 
    #browsable() %>%
    #
    
  })
  
  # ** Sparkline Table ---------------------------------------------------------
  output$line_graph <- renderUI({
    
    gtrends_spark_df <- readRDS(file.path("data", paste0("gtrends_spark_since_",
                                                         input$select_begin_date,"_large.Rds")))
    
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
                      cor_casesMA7_hitsMA7_max, 
                      cor_casesMA7_hitsMA7_lag,
                      cor_casesMA7_hitsMA7_zscore,
                      cases_total) %>%
        dplyr::rename(Country = name,
                      Trends = l_cases_hits,
                      "Correlation" = cor_casesMA7_hitsMA7_max,
                      "Lead/Lag" = cor_casesMA7_hitsMA7_lag,
                      "Z-Score" = cor_casesMA7_hitsMA7_zscore)
      
    } 
    if(input$select_covid_type %in% "Deaths"){
      
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(name, l_death_hits,
                      cor_deathMA7_hitsMA7_max, 
                      cor_deathMA7_hitsMA7_lag,
                      cor_deathMA7_hitsMA7_zscore,
                      death_total) %>%
        dplyr::rename(Country = name,
                      Trends = l_death_hits,
                      "Correlation" = cor_deathMA7_hitsMA7_max,
                      "Lead/Lag" = cor_deathMA7_hitsMA7_lag,
                      "Z-Score" = cor_deathMA7_hitsMA7_zscore)
    } 
    
    #### Sort
    if(!is.null(input$select_sort_by)){
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
    }
    
    #### Adjust Variables
    gtrends_spark_df$Correlation <- gtrends_spark_df$Correlation %>% round(3)
    gtrends_spark_df$`Z-Score` <- gtrends_spark_df$`Z-Score` %>% round(3)
    gtrends_spark_df$`Lead/Lag` <- paste(gtrends_spark_df$`Lead/Lag`, "days")
    
    #### Remove Unneeded Variables
    gtrends_spark_df$continent <- NULL
    gtrends_spark_df$death_total <- NULL
    gtrends_spark_df$cases_total <- NULL
    gtrends_spark_df$keyword_en <- NULL
    
    #### Make Table
    #Country	Trends	Correlation	Correlation Lag
    
    f_list <- list(
      `Country` = formatter("span", style = ~ style(color = "black", font.weight = "bold", width = "2px")),
      `Lead/Lag` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
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
      
      `Correlation` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
      `Z-Score` = formatter("span", style = ~ style(color = "black", font.weight = "bold"))
    )
    
    gtrends_spark_df <- gtrends_spark_df[!is.na(gtrends_spark_df$Correlation),]
    
    table_max <- nrow(gtrends_spark_df)
    
    l <- formattable(
      gtrends_spark_df[1:table_max,] %>% as.data.table(),
      align = c("c", "c", "l", "l"),
      f_list
    ) %>% format_table(align = c("c", "c", "l", "l", "l")) %>%
      htmltools::HTML() %>%
      div() %>%
      # use new sparkline helper for adding dependency
      spk_add_deps() %>%
      # use column for bootstrap sizing control
      # but could also just wrap in any tag or tagList
      {column(width=12, .)}
    
    l
    
    
  })
  
  
  # ** Max Correlation Dotplot -------------------------------------------------
  # **** Title ---------------------------
  output$cor_distribution_text <- renderText({
    paste0("Correlation between COVID-19 ",
           tolower(input$select_covid_type),
           " and Google search interest. Data after ",
           input$select_begin_date, 
           " used.")
  })
  
  # **** Figure ---------------------------
  output$max_cor_hist <- renderPlot({
    
    cor_df     <- readRDS(file.path("data", paste0("correlations_since_",
                                                   input$select_begin_date,
                                                   ".Rds")))
    
    if(input$select_search_category != "All"){
      kwords <- keywords_df$keyword_en[keywords_df$category %in% input$select_search_category ]
      cor_df <- cor_df[cor_df$keyword_en %in% kwords,]
    }
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    cor_df <- cor_df %>%
      dplyr::filter(type %in% input$select_covid_type) 
    
    p1 <- ggplot() +
      geom_dotplot(data = cor_df,
                   aes(x = reorder(keyword_en,
                                   cor),
                       y = cor,
                       fill = "=  One Country"),
                   binaxis = "y", 
                   stackdir = "center",
                   dotsize = 1,
                   binwidth = .02,
                   color = "palegreen4") +
      scale_fill_manual(values = c("palegreen3")) +
      labs(fill = NULL) +
      geom_hline(yintercept = 0) +
      coord_flip() +
      theme_minimal() +
      labs(x = "",
           y = "Correlation",
           title = NULL) +
      theme(axis.text.y = element_text(face = "bold", size = 14),
            axis.text.x = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.position = "top") 
    
    # cor_df$lag <- cor_df$lag + runif(cor_df$lag)
    # 
    # p2 <- ggplot() +
    #   geom_dotplot(data = cor_df,
    #                aes(x = reorder(keyword_en,
    #                                cor),
    #                    y = lag,
    #                    fill = "=  One Country"),
    #                binaxis = "y", 
    #                stackdir = "center",
    #                dotsize = 0.75,
    #                binwidth = 0.5,
    #                color = "palegreen4") +
    #   scale_fill_manual(values = c("palegreen3")) +
    #   labs(fill = NULL) +
    #   geom_hline(yintercept = 0) +
    #   coord_flip() +
    #   theme_minimal() +
    #   labs(x = "",
    #        y = "",
    #        title = "Lag of Best Correlation") +
    #   theme(axis.text.y = element_blank(),
    #         axis.text.x = element_text(size = 14),
    #         legend.text = element_text(size = 14),
    #         legend.position = "top",
    #         plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) 
    # 
    # ggarrange(p1, p2,
    #           common.legend = T,
    #           widths = c(.6,.4))
    
    p1
    
    
  })
  
  output$max_cor_hist_ui <- renderUI({
    
    height <- "700px"
    if(input$select_search_category == "Symptoms") height <- "500px"
    if(input$select_search_category == "Mental Health") height <- "600px"
    if(input$select_search_category == "Coronavirus General") height <- "300px"
    
    plotOutput("max_cor_hist",
               height = height)
    
  })
  
  # ** Cor Histogram -----------------------------------------------------------
  output$keyword_cor <- renderPlot({
    
    cor_df     <- readRDS(file.path("data", paste0("correlations_since_",
                                                   input$select_begin_date,
                                                   ".Rds")))
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    cor_df <- cor_df %>%
      dplyr::filter(type %in% input$select_covid_type,
                    keyword_en %in% input$select_keyword) 
    
    cor_sum_df <- cor_df %>%
      ungroup() %>%
      mutate(cor = round(cor*10, 0)/10) %>%
      group_by(cor) %>%
      summarise(N = n()) %>%
      mutate(cor = cor %>% as.factor())
    
    
    seq(from=-1, to=1, by=.1) %>%
      as.data.frame() %>%
      dplyr::rename(cor = ".") %>%
      mutate(cor = cor %>% as.factor()) %>%
      left_join(cor_sum_df) %>%
      mutate(N = N %>% replace_na(0)) %>%
      mutate(cor = cor %>% as.character() %>% as.numeric()) %>%
      
      ggplot() +
      geom_col(aes(x = factor(cor), y = N),
               color = "black",
               fill = "palegreen3") + 
      labs(title = "Correlation",
           x = NULL,
           y = "Number\nOf\nCountries") +
      theme_minimal() +
      theme(axis.title.y = element_text(angle = 0,
                                        vjust = 0.5),
            plot.title = element_text(face = "bold", size=13,
                                      hjust = 0.5))
    
  })
  
  # ** Lag Histogram -----------------------------------------------------------
  output$keyword_lag <- renderPlot({
    
    cor_df     <- readRDS(file.path("data", paste0("correlations_since_",
                                                   input$select_begin_date,
                                                   ".Rds")))
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    cor_df <- cor_df %>%
      dplyr::filter(type %in% input$select_covid_type,
                    keyword_en %in% input$select_keyword) 
    
    # cor_sum_df <- cor_df %>%
    #   ungroup() %>%
    #   mutate(lag = round(lag/3, 0)*3) %>%
    #   group_by(lag) %>%
    #   summarise(N = n()) %>%
    #   filter(lag != 21)
    
    cor_df %>%
      ggplot() +
      geom_histogram(aes(x = lag),
                     color = "black",
                     fill = "palegreen3",
                     bins = 15) + 
      labs(title = "Lead/Lag",
           x = NULL,
           y = "Number\nOf\nCountries") +
      theme_minimal() +
      theme(axis.title.y = element_text(angle = 0,
                                        vjust = 0.5),
            plot.title = element_text(face = "bold", size=13,
                                      hjust = 0.5))
    
  })
  
  # COUNTRY FIGURES **************** -------------------------------------------
  
  # ** Sparkline Table ---------------------------------------------------------
  #observe({
  output$line_graph_country <- renderUI({
    
    #### Subset
    gtrends_spark_df <- readRDS(file.path("data", 
                                          paste0("gtrends_spark_since_",
                                                 input$select_begin_date_country,"_small.Rds"))) %>%
      filter(name %in% input$select_country)
    
    if(input$select_search_category_country != "All"){
      kwords <- keywords_df$keyword_en[keywords_df$category %in% input$select_search_category_country]
      gtrends_spark_df <- gtrends_spark_df[gtrends_spark_df$keyword_en %in% kwords,]
    }
    
    #### COVID Names
    if(input$select_covid_type_map %in% "Cases"){
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(l_cases_hits, keyword_en,
                      cor_casesMA7_hitsMA7_max, 
                      cor_casesMA7_hitsMA7_lag,
                      cor_casesMA7_hitsMA7_zscore,
                      cases_total) %>%
        dplyr::rename("Search Term" = keyword_en,
                      Trends = l_cases_hits,
                      "Correlation" = cor_casesMA7_hitsMA7_max,
                      "Lead/Lag" = cor_casesMA7_hitsMA7_lag,
                      "Z-Score" = cor_casesMA7_hitsMA7_zscore)
      
      
    } 
    if(input$select_covid_type_map %in% "Deaths"){
      
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(l_death_hits, keyword_en,
                      cor_deathMA7_hitsMA7_max, 
                      cor_deathMA7_hitsMA7_lag,
                      cor_deathMA7_hitsMA7_zscore,
                      death_total) %>%
        dplyr::rename("Search Term" = keyword_en,
                      Trends = l_death_hits,
                      "Correlation" = cor_deathMA7_hitsMA7_max,
                      "Lead/Lag" = cor_deathMA7_hitsMA7_lag,
                      "Z-Score" = cor_deathMA7_hitsMA7_zscore)
    } 
    
    gtrends_spark_df <- gtrends_spark_df %>%
      dplyr::select("Search Term", "Trends", "Correlation", "Lead/Lag", "Z-Score")
    
    gtrends_spark_df$`Z-Score` <- gtrends_spark_df$`Z-Score` %>% round(3)
    
    ## Change height/width
    # gtrends_spark_df$Trends <- gtrends_spark_df$Trends %>% 
    #   str_replace_all(":150", ":25") %>% # height
    #   str_replace_all(":200", ":100") # width
    
    #### Sort
    gtrends_spark_df <- gtrends_spark_df %>%
      arrange(-Correlation)
    
    #### Adjust Variables
    gtrends_spark_df$Correlation <- gtrends_spark_df$Correlation %>% round(3)
    gtrends_spark_df$`Lead/Lag` <- paste(gtrends_spark_df$`Lead/Lag`, "days")
    
    #### Make Table
    bar_color <- "#FF9999"
    
    f_list <- list(
      `Search Term` = formatter("span", style = ~ style(color = "black", font.weight = "bold", width = "2px")),
      `Lead/Lag` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
      `Z-Score` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
      `Correlation` = formatter(
        "span",
        style = x ~ style(
          display = "inline-block",
          direction = "lft",
          font.weight = "bold",
          #"border-radius" = "4px",
          "padding-left" = "2px",
          "background-color" = csscolor(bar_color),
          width = percent(proportion(x)),
          color = csscolor("black")
        )
      )
      
      #`Correlation` = formatter("span", style = ~ style(color = "black", font.weight = "bold"))
    )
    
    gtrends_spark_df <- gtrends_spark_df[!is.na(gtrends_spark_df$Correlation),]
    
    table_max <- nrow(gtrends_spark_df)
    
    l <- formattable(
      gtrends_spark_df[1:table_max,] %>% as.data.table(),
      align = c("c", "c", "l", "l"),
      f_list
    ) %>% format_table(align = c("c", "c", "l", "l", "l")) %>%
      htmltools::HTML() %>%
      div() %>%
      # use new sparkline helper for adding dependency
      spk_add_deps() %>%
      # use column for bootstrap sizing control
      # but could also just wrap in any tag or tagList
      {column(width=12, .)}
    
    l
    
  })
  #})
  
  # ** Historic Trends ---------------------------------------------------------
  # **** Title ------------------------------
  output$line_graph_country_key_title_1 <- renderText({
    paste0("<h4>COVID-19 ", input$select_covid_type_map, " and 
           Search Interest of ", input$select_keyword_country, "</h4>")
  })
  
  output$line_graph_country_key_title_2 <- renderText({
    
    cor_df     <- readRDS(file.path("data", paste0("correlations_since_",
                                                   input$select_begin_date_country,
                                                   ".Rds")))
    
    cor <- cor_df %>%
      filter(name %in% input$select_country) %>%
      filter(type %in% input$select_covid_type_map) %>%
      filter(keyword_en %in% input$select_keyword_country) 
    
    paste0("<b>Using data after ",
           input$select_begin_date_country, "<b><br>",
           "<ul>",
           "<li><b><em>Correlation:</em></b> ", cor$cor %>% round(3), "</li>",
           "<li><b><em>Lead/Lag:</em></b> ", cor$lag, " days</li>",
           "<li><b><em>Z-Score:</em></b> ", cor$zscore %>% round(3), "</li>",
           "</ul>")
    # 
    # paste0("<h4>Using data after ", input$select_begin_date_country, 
    #        "the correlation between COVID-19 ", tolower(input$input$select_covid_type_map)
    #        "<ul>",
    #        "<li><b>Correlation:</b> ", cor$cor %>% round(3), "</li>",
    #        "<li><b>Lead/Lag:</b> ", cor$lag, " days</li>",
    #        "<li><b>Z-Score:</b> ", cor$zscore %>% round(3), "</li>",
    #        "</ul></h4>")
    
  })
  
  # **** Figure ------------------------------
  output$line_graph_country_key <- renderPlot({
    
    if(F){
      gtrends_sub_df <- gtrends_df %>%
        filter(keyword_en %in% "Loss of Smell") %>%
        filter(name %in% "United States")
    }
    
    gtrends_sub_df <- gtrends_df %>%
      filter(keyword_en %in% input$select_keyword_country) %>%
      filter(name %in% input$select_country)
    
    #### COVID Names
    if(input$select_covid_type_map %in% "Cases"){
      gtrends_sub_df <- gtrends_sub_df %>%
        dplyr::select(keyword_en, date, hits_ma7, cases_new) %>%
        dplyr::rename(covid_new = cases_new)
    } 
    if(input$select_covid_type_map %in% "Deaths"){
      gtrends_sub_df <- gtrends_sub_df %>%
        dplyr::select(keyword_en, date, hits_ma7, death_new) %>%
        dplyr::rename(covid_new = death_new)
    } 
    
    multiplier <- max(gtrends_sub_df$covid_new, na.rm=T) / max(gtrends_sub_df$hits_ma7, na.rm=T)
    gtrends_sub_df$hits_ma7_adj <- gtrends_sub_df$hits_ma7 * multiplier
    
    color_cases <- "orange"
    color_hits <- "forestgreen"
    
    p <- gtrends_sub_df %>%
      ggplot() +
      geom_col(aes(x = date, y = covid_new),
               fill = color_cases,
               color = color_cases) +
      geom_line(aes(x = date, y = hits_ma7_adj),
                color = color_hits,
                size = 1) +
      labs(x = NULL, 
           y = input$select_covid_type_map) +
      scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Search Interest",
                                             breaks = c(0, max(gtrends_sub_df$hits_ma7_adj, na.rm=T)),
                                             labels = c("Low", "High"))) +
      theme_minimal() +
      theme(axis.title.y.left = element_text(#angle = 0, 
        #vjust = 0.5, 
        color=color_cases,
        face = "bold",
        size=13),
        axis.title.y.right = element_text(#angle = 0, 
          #vjust = 0.5, 
          color=color_hits,
          face = "bold",
          size=13),
        axis.text.y.left = element_text(color = color_cases,
                                        size=13),
        axis.text.y.right = element_text(color = color_hits,
                                         size=13),
        axis.text = element_text(face = "bold", size=13),
        plot.title = element_text(face = "bold", hjust = 0.5, size=13))
    
    p
    
  }, bg = "transparent")
  
  
  # ** GTrends HTML ------------------------------------------------------------
  # **** Translation Text ---------------------------
  output$translation_text <- renderText({
    
    geo <- world$geo[world$name %in% input$select_country] %>% as.character()
    
    search_en <- input$select_keyword_country
    language_code <- languges_df$Language_code_main[languges_df$Code %in% geo]
    
    if(length(search_en) %in% 0) search_en <- "Loss of Smell"
    if(length(language_code) %in% 0) language_code <- "en"
    
    search <- keywords_df[[paste0("keyword_", language_code)]][tolower(keywords_df$keyword_en) %in% tolower(search_en)]
    search <- search %>% str_replace_all("'", "")
    search_p20 <- search %>% str_replace_all(" ", "%20")
    
    if(language_code %in% "en"){
      out <- ""
    } else{
      out <- paste0("'", search_en, "' translated into ",
                    languges_df$Language_main[languges_df$Code %in% geo],": ",
                    search)
    }
    
    out 
    
    
  })
  
  # **** Trends ---------------------------
  output$gtrends_html_trends <- renderUI({
    
    # Do this as doesn't appear on default.
    if((input$select_country %in% "") & LOAD_GTRENDS_INIT){
      updateSelectInput(session, "select_country",
                        selected = "United States")
      LOAD_GTRENDS_INIT <<- F
    }
    
    ## Country
    geo <- world$geo[world$name %in% input$select_country] %>% as.character()
    
    ## Search term
    search_en <- input$select_keyword_country
    language_code <- languges_df$Language_code_main[languges_df$Code %in% geo]
    
    if(length(search_en) %in% 0) search_en <- "Loss of Smell"
    if(length(language_code) %in% 0) language_code <- "en"
    
    search <- keywords_df[[paste0("keyword_", language_code)]][tolower(keywords_df$keyword_en) %in% tolower(search_en)]
    search <- search %>% str_replace_all("'", "")
    search_p20 <- search %>% str_replace_all(" ", "%20")
    
    tags$body(HTML(paste0('<script type="text/javascript" src="https://ssl.gstatic.com/trends_nrtr/2213_RC01/embed_loader.js"></script> <script type="text/javascript"> var divElem = document.getElementById("wrapper"); document.getElementById("wrapper").innerHTML = ""; trends.embed.renderExploreWidgetTo(divElem, "TIMESERIES", {"comparisonItem":[{"keyword":"',search,'","geo":"',geo,'","time":"today 3-m"}],"category":0,"property":""}, {"exploreQuery":"q=',search_p20,'&geo=',geo,'&date=today 3-m","guestPath":"https://trends.google.com:443/trends/embed/"}); </script>')))
  })
  
  # **** Map ---------------------------
  output$gtrends_html_map <- renderUI({
    
    # Do this as doesn't appear on default.
    if((input$select_country %in% "") & LOAD_GTRENDS_INIT){
      updateSelectInput(session, "select_country",
                        selected = "United States")
      LOAD_GTRENDS_INIT <<- F
    }
    
    ## Country
    geo <- world$geo[world$name %in% input$select_country] %>% as.character()
    
    ## Search term
    search_en <- input$select_keyword_country
    language_code <- languges_df$Language_code_main[languges_df$Code %in% geo]
    
    if(length(search_en) %in% 0) search_en <- "Loss of Smell"
    if(length(language_code) %in% 0) language_code <- "en"
    
    search <- keywords_df[[paste0("keyword_", language_code)]][tolower(keywords_df$keyword_en) %in% tolower(search_en)]
    search <- search %>% str_replace_all("'", "")
    search_p20 <- search %>% str_replace_all(" ", "%20")
    
    tags$body(HTML(paste0('<script type="text/javascript" src="https://ssl.gstatic.com/trends_nrtr/2213_RC01/embed_loader.js"></script> <script type="text/javascript"> var divElem = document.getElementById("wrapper2"); document.getElementById("wrapper2").innerHTML = ""; trends.embed.renderExploreWidgetTo(divElem, "GEO_MAP", {"comparisonItem":[{"keyword":"',search,'","geo":"',geo,'","time":"today 3-m"}],"category":0,"property":""}, {"exploreQuery":"q=',search_p20,'&geo=',geo,'&date=today 3-m","guestPath":"https://trends.google.com:443/trends/embed/"}); </script>')))
  })
  
  # INFORMATION ********************* ------------------------------------------
  
  # ** Language Table ----------------------------------------------------------
  output$language_table <- renderTable({
    
    languges_df %>%
      dplyr::select(Name, Language_main) %>%
      dplyr::rename(Country = Name,
                    Language = Language_main) %>%
      dplyr::mutate(Country = Country %>% tools::toTitleCase(),
                    Language = Language %>% 
                      str_replace_all("Chinese (Traditional)",
                                      "Chinese (Simplified)") %>%
                      tools::toTitleCase()) %>%
      distinct(Country, Language) %>%
      arrange(Country)
    
  })
  
  
  # UIS ******************************** ---------------------------------------
  # ** Select Keyword: Global --------------------
  output$select_keyword_ui <- renderUI({
    
    out <- selectInput(
      "select_keyword",
      label = strong("Search Term"),
      choices = keyword_list,
      selected = "Loss of Smell",
      multiple = F
    )
    
    if(input$select_search_category != "All"){
      
      keyword_df_i <- keywords_df[keywords_df$category %in% input$select_search_category,]
      
      out <- selectInput(
        "select_keyword",
        label = strong("Search Term"),
        choices = keyword_df_i$keyword_en,
        selected = keyword_df_i$keyword_en[1],
        multiple = F
      )
    }
    
    out
    
  })
  
  # ** Select Keyword: Country --------------------
  output$select_keyword_country_ui <- renderUI({
    
    out <- selectInput(
      "select_keyword_country",
      label = strong("Search Term"),
      choices = keyword_list,
      selected = "Loss of Smell",
      multiple = F
    )
    
    if(input$select_search_category_country != "All"){
      
      keyword_df_i <- keywords_df[keywords_df$category %in% input$select_search_category_country,]
      
      out <- selectInput(
        "select_keyword_country",
        label = strong("Search Term"),
        choices = keyword_df_i$keyword_en,
        selected = keyword_df_i$keyword_en[1],
        multiple = F
      )
    }
    
    out
    
  })
  
  
  # * * * * * * * * * * * P U R G A T O R Y * * * * * *  ------------------------
  
  
  
  # ** Global Map ---------------------------------------------------------------
  
  #### Basemap
  output$global_map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(lat = 0, lng = 0, zoom = 1)
    
  })
  
  observe({
    
    req(input$nav == "Country Level") # This makes leaflet show up; before no defaults.
    
    world$name <- NULL
    
    cor_sum_df <- cor_df %>%
      filter(type %in% "Cases") %>% # input$select_covid_type_map
      group_by(geo, name) %>%
      summarise(keyword_en = keyword_en[which.max(cor)])
    
    cor_sum_df$keyword_en[cor_sum_df$keyword_en %in% "Coronavirus Symptoms"] <- "Coronavirus<br>Symptoms"
    cor_sum_df$keyword_en[cor_sum_df$keyword_en %in% "Corona Symptoms"] <- "Corona<br>Symptoms"
    
    world_data <- merge(world, cor_sum_df, by = "geo", all.x=T, all.y=F)
    
    world_data$contain_historic_data <- ifelse(!is.na(world_data$keyword_en),
                                               "Historic Data Available",
                                               NA)
    
    colors <- brewer.pal(n = length(unique(world_data$contain_historic_data)), 
                         name = "Spectral")
    
    pal <- colorFactor(colors, 
                       world_data$contain_historic_data[!is.na(world_data$contain_historic_data)])
    
    leafletProxy("global_map",
                 data = world_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(label = ~name,
                  layerId = ~ name,
                  stroke = F,
                  fillOpacity = 1,
                  color = ~pal(contain_historic_data)) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = world_data$contain_historic_data[!is.na(world_data$contain_historic_data)],
                title = "",
                opacity = 1
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
  
  # *** Country Correlation Figure ---------------------------------------------
  output$country_cor <- renderPlot({
    
    country_name <- country_name_react()
    
    cor_df_sub <- cor_df[cor_df$name %in% country_name,]
    cor_df_sub <- cor_df_sub[cor_df_sub$type %in% input$select_covid_type_map,]
    
    cor_df_sub$keyword_en <- cor_df_sub$keyword_en %>% as.character()
    
    MIN_COR <- min(cor_df_sub$cor, na.rm=T)
    MIN_COR <- ifelse(MIN_COR > 0, 0, MIN_COR)
    
    cor_df_sub %>% 
      ggplot() +
      geom_bar(aes(x = cor, 
                   y = reorder(keyword_en, cor),
                   fill = cor),
               color = "black",
               stat = "identity") +
      labs(x = "Correlation",
           y = "") +
      scale_fill_gradient2(low = "blue",
                           mid = "white", 
                           high = "firebrick4",
                           midpoint = 0) +
      xlim(MIN_COR, 1) +
      theme_minimal() +
      theme(legend.position = "none") 
    
  })
  
  # *** Country Trends -----------------------------------------------------------
  # country_data_react <- reactive({
  #   
  #   country_name <- country_name_react()
  #   
  #   gtrends_sub_df <- gtrends_df %>%
  #     dplyr::filter(name %in% country_name,
  #                   keyword_en %in% input$select_keyword_map) 
  #   
  #   ## If nrow = 0, give 1 row with just name
  #   if(nrow(gtrends_sub_df) == 0){
  #     
  #     gtrends_sub_df <- bind_rows(gtrends_sub_df,
  #                                 data.frame(name = country_name))
  #     
  #   }
  #   
  #   gtrends_sub_df
  #   
  # })
  
  #observe({
  
  output$country_trends <- renderPlot({
    
    color_cases <- "orange"
    color_hits <- "forestgreen"
    
    #### Subset to country
    if(F){
      gtrends_sub_df <- gtrends_df %>%
        filter(name %in% "United States",
               keyword_en %in% "Loss of Smell") 
    }
    
    
    gtrends_sub_df <- gtrends_df %>%
      filter(name %in% country_name_react()) %>%
      filter(keyword_en %in% input$select_keyword_country)
    
    #### Define case/death varibles
    if(input$select_covid_type %in% "Cases"){
      
      gtrends_sub_df <- gtrends_sub_df %>%
        dplyr::select(name, date, hits_ma7, hits, cases_new,keyword_en,
                      cor_casesMA7_hitsMA7_max, cor_casesMA7_hitsMA7_lag,
                      cases_total) %>%
        dplyr::rename(Country = name,
                      covid_new = cases_new,
                      "Correlation" = cor_casesMA7_hitsMA7_max,
                      "Lead/Lag" = cor_casesMA7_hitsMA7_lag)
      
    } 
    if(input$select_covid_type %in% "Deaths"){
      
      gtrends_sub_df <- gtrends_sub_df %>%
        dplyr::select(name, date, hits_ma7, hits, death_new, keyword_en,
                      cor_deathMA7_hitsMA7_max, cor_deathMA7_hitsMA7_lag,
                      death_total) %>%
        dplyr::rename(Country = name,
                      covid_new = death_new,
                      "Correlation" = cor_deathMA7_hitsMA7_max,
                      "Lead/Lag" = cor_deathMA7_hitsMA7_lag)
    } 
    
    #### Prep hits
    gtrends_sub_df$hits_fig <-  gtrends_sub_df$hits_ma7
    
    # multiplier so that max of hits is same as covid
    multiplier <- max(gtrends_sub_df$covid_new, na.rm=T) / max(gtrends_sub_df$hits_fig, na.rm=T)
    
    gtrends_sub_df$hits_fig <- gtrends_sub_df$hits_fig * multiplier
    
    ### COVID 
    
    gtrends_sub_covid_df <- gtrends_sub_df %>%
      dplyr::group_by(date, Country) %>%
      dplyr::summarise(covid_new = mean(covid_new, na.rm=T))
    
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
        geom_col(data = gtrends_sub_covid_df,
                 aes(x = date, y = covid_new),
                 fill = color_cases,
                 color = color_cases) +
        geom_line(data = gtrends_sub_df,
                  aes(x = date, y = hits_fig),
                  color = color_hits, 
                  group = color_hits,
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
  
  #})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  
  
  
  
  
  
  # * US Example Figure --------------------------------------------------------
  
  output$us_ex_fig <- renderPlot({
    p <- readRDS(file.path("data", "us_ex_fig.Rds"))
    
    p 
    
  })
  
  output$us_ex_fig_arrow <- renderPlot({
    p <- readRDS(file.path("data", "us_ex_fig_arrow.Rds"))
    
    p 
    
  }, bg = "transparent")
  
  # output$keyword_table <- DT::renderDataTable({
  #   DT::datatable(keywords_df, 
  #                 options = list(dom = 't'))
  # })
  
  
  
  # * renderUIs ----------------------------------------------------------------
  
  
  # 
  # output$ui_select_keyword_map <- renderUI({
  #   
  #   
  #   keywords_df <- cor_df %>%
  #     dplyr::filter(type %in% input$select_covid_type_map) %>%
  #     dplyr::filter(name %in% country_name_react()) 
  #   
  #   keywords_df$keyword_en <- keywords_df$keyword_en %>% as.character()
  #   
  #   selectInput(
  #     "select_keyword_map",
  #     label = strong("Search Term"),
  #     choices = keywords_df$keyword_en,
  #     selected = keywords_df$keyword_en[which.max(keywords_df$cor)],
  #     multiple = F
  #   )
  #   
  # })
  
  
  
  output$ui_select_covid_cases <- renderUI({
    
    numericInput(
      "select_covid_cases",
      label = strong(paste("Restrict to Countries with X", input$select_covid_type)),
      value = 100,
      min = 0
    )
    
  })
  
  output$country_name <- renderText({
    
    input$select_country
    
  })
  
  output$country_page_description <- renderText({
    
    paste0("Considering all countries, search terms including
                    `Loss of Smell`, `I Can't Smell` and `Loss of Taste` 
                    correlate most strongly with COVID-19 ",
           tolower(input$select_covid_type),
           ". However, other search terms strongly correlate 
                    with COVID trends in select countries. This page shows
                    how well each search term correlates with COVID-19 ",
           tolower(input$select_covid_type),
           " in individual countries.")
    
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
  
  output$trends_country_subtitle <- renderText({
    
    paste0("The table compares trends in <span style='color:orange;'>COVID-19 ",
           tolower(input$select_covid_type), 
           "</span> and the <span style='color:green;'>search term interest",
           "</span>. It shows the correlation when the correlation is 
           highest.")
    
  })
  
  output$trends_subtitle <- renderText({
    
    paste0("We compare trends in <span style='color:orange;'>COVID-19 ",
           tolower(input$select_covid_type), 
           "</span> and <span style='color:green;'>search popularity of ",
           input$select_keyword, 
           ".</span> We show the correlation between the two and the number of
           days in the past when the search popularity is most strongly 
           correlated with COVID-19 ", tolower(input$select_covid_type), 
           ".")
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
      selected = "Correlation",
      multiple = F
    )
    
  })
  
})

# RUN THE APP ==================================================================
shinyApp(ui, server)
