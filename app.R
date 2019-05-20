# libraries----
library(shiny)
library(shinydashboard)
library(lubridate)
library(magrittr)
library(plotly)
library(tidyverse)
library(PerformanceAnalytics)
library(TTR)
library(reshape2)
library(dbplyr)
library(RPostgres)
library(gridExtra)
library(caret)
library(factoextra)
library(purrr)
library(xtable)
library(rsconnect)
source('scripts/functions.R')

# Model Generator ----

serie <- c('S&P_500', 'NASDAQ', 'NIKKEI_225', 'FTSE_100', 'BOVESPA')
list_serie <- list()
for(s in serie){
  .aux_serie <- paste('data/', s, '.csv', sep = '') %>%
    read_csv(locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
    select(-one_of(c('Vol.', '% var.'))) %>%
    mutate(Fecha = dmy(Fecha)) %>%
    setNames(c('timestamp', 'close', 'open', 'high', 'low')) %>%
    arrange(timestamp)
  
  list_serie %<>% rlist::list.append(.aux_serie)
}




# UI-----
ui <- shinyUI(dashboardPage(
  dashboardHeader(title = 'PCA Analisys', titleWidth = 240),
  # Sidebar----
  dashboardSidebar(
    tags$a(href = "http://www.ucv.ve/",
           tags$img(src = "white-logo.png", height = 100, width = 100, 
           style = 'margin-left: 55px; margin-top: 12px;')),
    tags$a(href = "http://www.ucv.ve/estructura/facultades/facultad-de-ciencias-economicas-y-sociales.html",
           tags$img(src = "faces.png", height = 75, width = 150, 
                    style = 'margin-left: 30px; margin-top: 12px;')),
    br(),
    br(),
    
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Contributions", tabName = "contrib", icon = icon("bar-chart-o")),
      menuItem("Factor Map", tabName = "factorMap", icon = icon("dashboard"))),
    selectInput('index',
                label = 'Choose Index',
                choices = list('S&P_500' = 1,
                               'NASDAQ' = 2,
                               'NIKKEI_225' = 3,
                               'FTSE_100' = 4,
                               'BOVESPA' = 5),
                selected = 1),
    
    sliderInput('sample', 'Choose Sample',
                min = 2013, max = 2017,
                value = 2013)
  ),
  
  # Body----
  dashboardBody(
    # Color BodyHeader same as DashboardHeader
    tags$head(tags$style(HTML('
                              .skin-blue .main-header .logo {
                              background-color: #3c8dbc;
                              }
                              
                              .skin-blue .main-header .logo:hover {
                              background-color: #3c8dbc;
                              }
                              
                              .content-wrapper {
                              background-color: #778899;
                              }
                              '))),
    
    tabItems(
      
      # Dashboard
      tabItem('contrib',
              fluidRow(column(6, box(plotlyOutput("eigenvalue"), width = 12)),
                       column(6, box(plotlyOutput("variance"), width = 12))),
              
              fluidRow(column(6, box(plotlyOutput("contrib1"), width = 12)),
                       column(6, box(plotlyOutput("contrib2"), width = 12))),
              
              fluidRow(column(6, box(plotlyOutput("contrib3"), width = 12)),
                       column(6, box(plotlyOutput("contrib4"), width = 12))),
              
              fluidRow(column(6, box(plotlyOutput("contrib5"), width = 12)),
                       column(6, box(plotlyOutput("contrib6"), width = 12))),
              
              fluidRow(column(6, box(plotlyOutput("contrib7"), width = 12)))
      ),
      
      tabItem('factorMap',
            fluidRow(column(1, radioButtons('comp1', 'Select x axes component:',
                                            choices = list("Comp 1" = 1,
                                                           "Comp 2" = 2,
                                                           "Comp 3" = 3,
                                                           "Comp 4" = 4,
                                                           "Comp 5" = 5,
                                                           "Comp 6" = 6,
                                                           "Comp 7" = 7),
                                            selected = 1,
                                            inline = FALSE)),
                     
                     column(1, radioButtons('comp2', 'Select y axes component:',
                                            choices = list("Comp 1" = 1,
                                                           "Comp 2" = 2,
                                                           "Comp 3" = 3,
                                                           "Comp 4" = 4,
                                                           "Comp 5" = 5,
                                                           "Comp 6" = 6,
                                                           "Comp 7" = 7),
                                            selected = 2,
                                            inline = FALSE)),
            
                     column(10, box(plotOutput("factorMapPlot"), width = 12))),
            
            fluidRow(column(10,  sliderInput('fmtresh', 'Choose contribution treshold:',
                                            min = 2, max = 45,
                                            value = 20)))
    )
  ))))

# Server----
server <- function(input, output, session) {
 
  pca <- reactive({
    .tp = 0.02
    .sl = 0.025
    .h = 20
    .cut <- 0.5
    
    i <- input$index %>% as.numeric()
    
      predictors_var <- list_serie[[i]] %>% predict_tp(tp = .tp, sl = .sl, h = .h) %>%
        mutate(class_2 = factor(class)) %>% #levels = c('stay', 'buy')))
        select(-one_of('class')) %>% 
        createIndicators() %>% 
        filter(year(timestamp) %in% seq(2009, input$sample, 1)) %>% 
        getPredictors()
      
      pca <- predictors_var %>% prcomp(scale = FALSE)
      
      pca

  })
  
  output$eigenvalue <- renderPlotly({

      fviz_eig(pca(), addlabels = TRUE, 
               choice = 'eigenvalue',
               geom = 'bar',
               barfill = 'tomato3',
               barcolor = 'tomato3') +
        labs(title = 'Histograma de Autovalores', x = 'Componentes', y = 'Autovalores')

    })

  output$variance <- renderPlotly({

    fviz_eig(pca(), addlabels = TRUE, 
             choice = 'variance', 
             geom = 'bar',
             barfill = 'tomato3',
             barcolor = 'tomato3') +
      labs(title = 'Histograma de variabilidad explicada', x = 'Componentes', y = '% Variabilidad explicada')
    
  })
  
  output$contrib1 <- renderPlotly({
    
    fviz_contrib(pca(), choice = "var", axes = 1, top = 25) +
      labs(y = '% Contribución', title = 'Contribución de variables a Comp-1')
    
  })
  
  output$contrib2 <- renderPlotly({
    
    fviz_contrib(pca(), choice = "var", axes = 2, top = 25) +
      labs(y = '% Contribución', title = 'Contribución de variables a Comp-2')
    
  })
  
  output$contrib3 <- renderPlotly({
    
    fviz_contrib(pca(), choice = "var", axes = 3, top = 25) +
      labs(y = '% Contribución', title = 'Contribución de variables a Comp-3')
    
  })
  
  output$contrib4 <- renderPlotly({
    
    fviz_contrib(pca(), choice = "var", axes = 4, top = 25) +
      labs(y = '% Contribución', title = 'Contribución de variables a Comp-4')
    
  })
  
  output$contrib5 <- renderPlotly({
    
    fviz_contrib(pca(), choice = "var", axes = 5, top = 25) +
      labs(y = '% Contribución', title = 'Contribución de variables a Comp-5')
    
  })
  
  output$contrib6 <- renderPlotly({
    
    fviz_contrib(pca(), choice = "var", axes = 6, top = 25) +
      labs(y = '% Contribución', title = 'Contribución de variables a Comp-6')
    
  })
  
  output$contrib7 <- renderPlotly({
    
    fviz_contrib(pca(), choice = "var", axes = 7, top = 25) +
      labs(y = '% Contribución', title = 'Contribución de variables a Comp-7')
    
  })
  
  output$factorMapPlot <- renderPlot({

    comp1 <- input$comp1 %>% as.numeric()
    comp2 <- input$comp2 %>% as.numeric()
    
    contribt <- input$fmtresh %>% as.numeric()
    
    fviz_pca_var(pca(),
                 axes = c(comp1, comp2),
                 col.var = "contrib",
                 select.var = list(contrib = contribt),
                 # alpha.var = "contrib",
                 repel = TRUE)

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

