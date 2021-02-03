library(plotly)
library(shiny)
library(shinythemes)
library(agricolae)
library(ggplot2)
library(nlme)
library(Hmisc)
library(knitr)

shinyUI(fluidPage(
  titlePanel( "Análisis de las notas", "ITM"),
  navbarPage( theme = shinytheme("cerulean"),
              "",
    tabPanel("General",
             ### ingresar la base de datos
             sidebarPanel(
                    
  
    fileInput(inputId="file",label=h5("Ingrese los datos en formato.txt"),
              accept=c('text/csv','text/comma-separated-values',
                       'text/plain','.csv','.txt')),
    selectInput("prom", "Escoja el promedio ",c(Choose='')),
    selectInput("semes", "Escoja el semestre ",c(Choose='')),
    selectInput("campus", "Escoja el factor ",c(Choose='')),
    selectInput("sga", "Escoja el siga ",c(Choose='')),
    uiOutput("estado")
    
    
    
               ),
    mainPanel(
      tabsetPanel(
        tabPanel("Datos",
                 fluidRow(dataTableOutput("table"))),
        tabPanel("Resumen",
                 fluidRow(
                   dataTableOutput("table1") )),
        tabPanel("Gráficas",
                 fluidRow(
                   plotOutput("total_plot",height = "400px", width="100%"),
                   h4("Boxplot comparativo de las notas finales"),
                   plotOutput("pred_plot61")
                   ))
              )
      
      )),
    tabPanel("Semestre",
             ### ingresar la base de datos
             sidebarPanel(
               
               radioButtons("typeInput", "Semestre",
                            choices = c("2018-1", "2018-2", 
                                        "2019-1", "2019-2"),
                            selected = "2018-1"),
               uiOutput("countryOutput")
             ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Resumen",
                            fluidRow( 
                              dataTableOutput("results"),  
                              plotlyOutput("preee",height="300px",width="100%"),
                              dataTableOutput("tabless14"),
                              h4("Comparativo"),
                              verbatimTextOutput("summary3"),
                              verbatimTextOutput("summary4"),
                              plotOutput("total_plot1",height = "400px", width="100%"),
                              dataTableOutput("results1"), 
                              plotlyOutput("preee1",height = "300px", width="100%"),
                              dataTableOutput("results2")
                              )),
                   tabPanel("Distribución notas",
                            fluidRow(
                              dataTableOutput("tabless7"),
                              plotOutput("coolplot"),
                              br(), br()
                            ))
                   ))
               
             )
    )))