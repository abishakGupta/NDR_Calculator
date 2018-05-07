library(shiny)
library(readxl)
library(ggplot2)
library(d3heatmap)
library(tidyr)

ui <- fluidPage(list(tags$head(HTML('<link rel="icon", href="iconNDR.png", type="image/png" />'))),
                div(style="padding: 1px 0px; width: '100%'",
                    titlePanel(title=div(img(height=100,width=100,src="iconNDR.png"), "Normalized Drug Response Calculator"), windowTitle = "NDR-Calulator")
                ),
  tabsetPanel(
    tabPanel("Use Processed Raw Data",
             sidebarLayout(
               sidebarPanel(
                 fileInput("dataProcessed", "Load raw data file",accept = c(".csv",".xlsx")),
                 tags$hr(),
                 actionButton("runTask1", "Compute NDR"),
                 tags$hr(),
                 downloadButton("downloadData1", "Download Results")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Raw Data Table",DT::dataTableOutput("processedTable")),
                   tabPanel("NDR Table",DT::dataTableOutput("tableNDR")),
                   tabPanel("Heatmap Plot", d3heatmap::d3heatmapOutput("heatmap")),
                   tabPanel("Individual Plot", uiOutput("choose_columns"),plotOutput("drugResponses"))
                 )
               )
             )
    ),
    tabPanel("Use Plate Format Data",
             sidebarLayout(
               sidebarPanel(
                 selectInput("inputSelection", "Load initial mode:", choices = c('Load initial plate reading'='1','Estimate using growth rate'='2')),
                 uiOutput("choose_initial_options"),
                 tags$hr(),
                 fileInput("dataFinal", "Load final plate reading",accept = c(".csv",".xlsx")),
                 splitLayout(
                   numericInput("inputRfinal", "Start row number", 12, min = 0, max = 25),
                   numericInput("plateNum", "Plate number", 1, min = 1, max = 10)
                 ),
                 tags$hr(),
                 fileInput("dataAnnot", "Load annotation file",accept = c(".csv",".xlsx")),
                 tags$hr(),
                 actionButton("runTask", "Compute NDR"),
                 tags$hr(),
                 tags$hr(),
                 downloadButton("downloadData", "Download Results")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Initial Table",DT::dataTableOutput("initTable")),
                   tabPanel("Final Table",DT::dataTableOutput("finalTable")),
                   tabPanel("Annotation Table",DT::dataTableOutput("annotTable")),
                   tabPanel("NDR Table",DT::dataTableOutput("table")),
                   tabPanel("Plot", plotOutput("plot"))
                 )
               )
             )
    ),
    tabPanel("About",
             fluidRow(
               column(10,includeMarkdown("about.md"), br())
             )
    )
  )
)

