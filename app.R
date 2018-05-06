library(tidyr)
library(shiny)
require(openxlsx)
library(DT)
library(reshape2)
library(tidyverse)
library(d3heatmap)
## app.R ##
server <- function(input, output,session) {

  resTaskP <- eventReactive(input$runTask1, {
    
    #check the validity of processed file
    if (grepl("\\.xlsx$", basename(input$dataProcessed$datapath)))
      dataframeProcessed <- read.xlsx(input$dataProcessed$datapath, colNames = TRUE)
    else if (grepl("\\.csv$", basename(input$dataProcessed$datapath)))
      dataframeProcessed <- read.table(input$dataProcessed$datapath, sep=",",stringsAsFactors = F)
    else
      return ()
    
    resTask <- dataframeProcessed
    resTask[resTask$DRUG_NAME=="dmso",2] <- "DMSO"
    GR_DMSO= median(resTask[resTask$DRUG_NAME=="DMSO",c("finalCells")]/resTask[resTask$DRUG_NAME=="DMSO",c("initCells")])
    GR_BzCl= median(resTask[resTask$DRUG_NAME=="BzCl",c("finalCells")]/resTask[resTask$DRUG_NAME=="BzCl",c("initCells")])
    resTask= resTask[!(resTask$DRUG_NAME %in% c("BzCl","DMSO","xBzCl","empty","Cytarabine/Idarubicin")),]
    
    NDR= pmax(-1,(1 - 2 ^ (log2(resTask[,c("finalCells")]/resTask[,c("initCells")])/log2(GR_BzCl))) / (1 - 2 ^ (log2(GR_DMSO)/log2(GR_BzCl))))
    
    resTask <- cbind(resTask,NDR)
    resTask
  })
  
  
  resTask <- eventReactive(input$runTask, {
    
    #check the validity of annotation file
    if (grepl("\\.xlsx$", basename(input$dataAnnot$datapath)))
      dataframeAnnot <- read.xlsx(input$dataAnnot$datapath, colNames = TRUE)
    else if (grepl("\\.csv$", basename(input$dataAnnot$datapath)))
      dataframeAnnot <- read.table(input$dataAnnot$datapath, sep=",",stringsAsFactors = F)
    else
      return ()
    
    #check the validity of final data file
    if (grepl("\\.xlsx$", basename(input$dataFinal$datapath)))
      dataframeFinal <- read.xlsx(input$dataFinal$datapath, colNames = TRUE, startRow=input$inputRfinal)
    else if (grepl("\\.csv$", basename(input$dataFinal$datapath)))
      dataframeFinal <- read.table(input$dataFinal$datapath, sep=",",stringsAsFactors = F,skip=input$inputRfinal)
    else
      return()
    
    lookupCharacter= c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")
    resTask <- dataframeAnnot[dataframeAnnot$Dest_Plate == input$plateNum,]
    # get the final readings
    finalCells <- rep(0,dim(resTask)[1])
    for (k in 1:dim(resTask)[1]){
      rowNum= which(lookupCharacter== resTask$DRow[k]) 
      colNum= as.numeric(resTask$DCol[k])
      finalCells[k] <-  as.numeric(dataframeFinal[rowNum,colNum])
    }
    resTask <- cbind(resTask,finalCells)
    
    #check the validity of initial data file or rate
    if (input$inputSelection == 1){
      if (grepl("\\.xlsx$", basename(input$dataInitial$datapath)) )
        dataframeInitial <- read.xlsx(input$dataInitial$datapath, colNames = TRUE, startRow=input$inputRinit)
      else if (grepl("\\.csv$", basename(input$dataInitial$datapath)))
        dataframeInitial <- read.table(input$dataInitial$datapath, sep=",",stringsAsFactors = F,skip=input$inputRinit)
      else
        return()
    }
    else if (input$inputSelection == 2){
      if(!is.null(input$growthRate)){
        dataframeInitial <- dataframeFinal
        dataframeInitial[,] <- median(resTask[resTask$DRUG_NAME=="DMSO",c("finalCells")])/input$growthRate
      }
      else
        return()
    }
    else
      return()
      
    initCells <- rep(0,dim(resTask)[1])
    for (k in 1:dim(resTask)[1]){
        rowNum= which(lookupCharacter== resTask$DRow[k]) 
        colNum= as.numeric(resTask$DCol[k])
        initCells[k] <-  as.numeric(dataframeInitial[rowNum,colNum])
    }
    resTask <- cbind(resTask,initCells)

    
    resTask[resTask$DRUG_NAME=="dmso",2] <- "DMSO"
    GR_DMSO= median(resTask[resTask$DRUG_NAME=="DMSO",c("finalCells")]/resTask[resTask$DRUG_NAME=="DMSO",c("initCells")])
    GR_BzCl= median(resTask[resTask$DRUG_NAME=="BzCl",c("finalCells")]/resTask[resTask$DRUG_NAME=="BzCl",c("initCells")])
    resTask= resTask[!(resTask$DRUG_NAME %in% c("BzCl","DMSO","xBzCl","empty","Cytarabine/Idarubicin")),]
    
    NDR= pmax(-1,(1 - 2 ^ (log2(resTask[,c("finalCells")]/resTask[,c("initCells")])/log2(GR_BzCl))) / (1 - 2 ^ (log2(GR_DMSO)/log2(GR_BzCl))))
    
    resTask <- cbind(resTask,NDR)
    resTask
  })
  
  ## For data download
  output$downloadData<-downloadHandler(
    filename = function() { paste(sub("\\..*$", '',basename(input$dataFinal$datapath)) , '_results.csv', sep='')},
    content = function(file) { write.csv(resTask(),file)}
    )
  
  output$downloadData1<-downloadHandler(
    filename = function() { paste(sub("\\..*$", '',basename(input$dataProcessed$datapath)) , '_results.csv', sep='')},content = function(file) { write.csv(resTaskP(),file)}
  )
  
  ## To render table
  output$table <- DT::renderDataTable({
    data <- resTask()
    data <- data[,c("DRUG_NAME","initCells","Dilution","finalCells","NDR")]
    return (data)
  })
  
  output$tableNDR <- DT::renderDataTable({
    data <- resTaskP()
    data <- data[,c("DRUG_NAME","initCells","Dilution","finalCells","NDR")]
    return (data)
  })
  
  output$annotTable <- DT::renderDataTable({
    if(is.null(input$dataAnnot))
      return ()
    
    if (grepl("\\.xlsx$", basename(input$dataAnnot$datapath)))
      dataframeFinal = read.xlsx(input$dataAnnot$datapath, colNames = TRUE)
    else if (grepl("\\.csv$", basename(input$dataAnnot$datapath)))
      dataframeFinal <- read.table(input$dataAnnot$datapath, sep=",",stringsAsFactors = F)
    else
      return ()
    return (dataframeFinal)
  })
  
  output$initTable <- DT::renderDataTable({
    if(is.null(input$dataInitial))
      return ()
    
    if (grepl("\\.xlsx$", basename(input$dataInitial$datapath)))
      dataframeFinal <- read.xlsx(input$dataInitial$datapath, colNames = TRUE, startRow=input$inputRinit)
    else if (grepl("\\.csv$", basename(input$dataInitial$datapath)))
      dataframeFinal <- read.table(input$dataInitial$datapath, sep=",",stringsAsFactors = F,skip=input$inputRinit)
    else
      return ()
    return (dataframeFinal)
  })
  
  output$finalTable <- DT::renderDataTable({
    if(is.null(input$dataFinal))
      return ()
    if (grepl("\\.xlsx$", basename(input$dataFinal$datapath)))
      dataframeFinal <- read.xlsx(input$dataFinal$datapath, colNames = TRUE, startRow=input$inputRfinal)
    else if (grepl("\\.csv$", basename(input$dataFinal$datapath)))
      dataframeFinal <- read.table(input$dataFinal$datapath, sep=",",stringsAsFactors = F,skip=input$inputRfinal)
    else
      return ()
    return (dataframeFinal)
  })
  
  output$processedTable <- DT::renderDataTable({
    if(is.null(input$dataProcessed))
      return ()
    if (grepl("\\.xlsx$", basename(input$dataProcessed$datapath)))
      dataframeFinal <- read.xlsx(input$dataProcessed$datapath, colNames = TRUE)
    else if (grepl("\\.csv$", basename(input$dataProcessed$datapath)))
      dataframeFinal <- read.table(input$dataProcessed$datapath, sep=",",stringsAsFactors = F)
    else
      return ()
    return (dataframeFinal)
  })
  
  ## To render plots
  output$plot <- renderPlot({
    data <- resTask()
    drugs <- unique(data$DRUG_NAME[data$NDR < 0])
    data <- data[data$DRUG_NAME %in% drugs,]
    plot <- ggplot(data,aes(x=Dilution, y=NDR, group=DRUG_NAME, color = DRUG_NAME)) + ylab("Concentration level")+ geom_line(size = 1)  + theme_classic(base_size = 18) + ggtitle("Potent drugs with at least one NDR < 0")
    print(plot)
    
  })
  
  output$heatmap <- renderD3heatmap({
    if (is.null(resTaskP()))
      return()
    myData <- resTaskP()[,c("NDR","Dilution","DRUG_NAME")]
    myData$NDR <- as.numeric(myData$NDR)
    myData$Dilution <- as.numeric(myData$Dilution)
    myData <- spread(myData, DRUG_NAME, NDR)
    
    d3heatmap(myData,scale = "none", colors = "Spectral",Colv=F,Rowv=F)
    
  })
  
  output$drugResponses <- renderPlot({
    if (is.null(input$columns))
      return()
    drugsData <- resTaskP()
    drugsData <- drugsData[drugsData$DRUG_NAME == input$columns,]
    drugsData$NDR <- as.numeric(drugsData$NDR)
    drugsData$Dilution <- as.numeric(drugsData$Dilution)
    output <- plot(drm(NDR~Dilution, data = drugsData,  fct = LL.5()),ylim=c(-1,1.5),xlab="Concentration",ylab="NDR")
      #ggplot(drugsData,aes(x=Conc, y=NDR,group= DRUG_NAME)) + xlab("Concentration levels")+ geom_line(size = 1)  + theme_classic(base_size = 18)
    
    print(output)
    
  })
  
  output$choose_initial_options <- renderUI({
    output = tagList()
    # If missing input, return to avoid error later in function
    if (is.null(input$inputSelection))
      return(NULL)
    else if (input$inputSelection == 1){
      output[[1]] = fileInput("dataInitial", "",accept = c(".csv",".xlsx"))
      output[[2]] = splitLayout(
        numericInput("inputRinit", "Start row number", 12, min = 0, max = 25),
        numericInput("plateNum", "Plate Number", 1, min = 1, max = 10)
      )
    }
    else if (input$inputSelection == 2){
      output[[1]] = numericInput("growthRate", "Growth Rate:", 2, min = 0, max = 10)
    }
    else
      resTask <- "No Task found"
    
    output
  })
  
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(resTaskP()))
      return()
    
    # Get the data set with the appropriate name
    data <- resTaskP()
    colnames <- unique(data$DRUG_NAME[data$NDR < 0])
    
    # Create the checkboxes and select them all by default
    radioButtons("columns", "Drugs with NDR < 0", 
                       choices  = colnames,
                       selected = "",
                       inline = T)
  })
  
}





ui <- fluidPage(
  titlePanel(title=div(img(height=100,width=100,src="NDR_icon.png"), "Normalized Drug Response Calculator", windowTitle = "NDR-Calulator")),
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
                            tabPanel("Heatmap Plot", d3heatmapOutput("heatmap")),
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

shinyApp(ui = ui, server = server)
