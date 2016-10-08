## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "CAH"),
  dashboardSidebar(
    fileInput("file1", "Choose CSV File", 
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    sidebarMenu(
      menuItem("CAH", tabName = "CAH"),
      menuItem("Raw data", tabName = "rawdata"),
      menuItem("CAH groups", tabName = "cahresult")
    ),
    sliderInput("nbGroups", "Number of groups:",
                min = 2, max = 100, value = 3, step = 1 # TODO dynamic depending on number of rows
    ),
    checkboxInput("doScaleData", "Center reduce variables:", value = FALSE, width = NULL),
    selectizeInput("cahMethod", "Choose your method:", 
                   c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), 
                   selected = "complete", multiple = FALSE,
                   options = NULL),
    selectizeInput("distance", "Choose your distance:", 
                   c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                   selected = "euclidean", multiple = FALSE,
                   options = NULL)
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "CAH",
              fluidRow(
                box(plotOutput("plotDendo", height = 400)),
                box(plotOutput("plotHeight", height = 400))
              ),
              fluidRow(
                box(plotOutput("plotGroups", height = 400))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "rawdata",
              tableOutput('rawData')
      ),
      
      # Third tab content
      tabItem(tabName = "cahresult",
              tableOutput('dataCah')
      )
    )
  )
)

testNoFile <- function(f, data){
  if (!is.null(data)){
    f(data)
  }
  else {
    stop("Please input a csv file")
  }
}


server <- function(input, output) {
  dataInput <- reactive({
    inFile <- input$file1
    
    results = list()
    if (is.null(inFile)){
      results$df <- read.csv('./data/donclassif.txt',header=T,sep=";")
    }
    else {
      results$df <- read.csv(inFile$datapath,header=T,sep=";")
    }
    
    if (input$doScaleData)
      data2work = scale(results$df)
    else
      data2work = results$df    
    
    matD <- dist(data2work, method= input$distance)
    results$cahS <- hclust(matD, method = input$cahMethod)
    
    results$rescah <- cutree(results$cahS,k=input$nbGroups)
    
    return(results)
  })
  
  output$plotDendo <- renderPlot({
    f <- function(data){
      plot(data$cahS)
      # TODO : abline(data$cah$height[input$nbGroups], 0, col = "red")
    }
    testNoFile(f, dataInput())
  })
  output$plotHeight <- renderPlot({
    f <- function(data){
      plot(2:(input$nbGroups+6), sort(data$cah$height,dec=T)[1:(input$nbGroups+5)],type="h",ylab="height",xlab="nbGroups", main='Height depending on number of groups')
      points(input$nbGroups, sort(data$cah$height,dec=T)[input$nbGroups-1], type="p", col = "red", cex=3)
    }
    testNoFile(f, dataInput())
  })
  output$plotGroups <- renderPlot({
    f3 <- function(data){
      names_ <- names(data$df)
      plot(data$df[, 1:2],col=data$rescah,xlab = names_[1],ylab = names_[2], main = "Groups on first two variables")
    }
    testNoFile(f3, dataInput())
  })
  output$rawData <- renderTable({
    f <- function(data){
      return(data$df)
    }
    testNoFile(f, dataInput())
  })
  output$dataCah <- renderTable({
    f <- function(data){
      df2 <- data$df
      df2$group <- data$rescah
      return(df2)
    }
    testNoFile(f, dataInput())
  })
}

shinyApp(ui, server)