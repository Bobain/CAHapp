## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "CAH"),
  dashboardSidebar(
    fileInput("file1", "Choose CSV File",
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
                min = 1, max = 100, value = 3, step = 1 # TODO dynamic depending on number of rows
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "CAH",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(plotOutput("plot2", height = 250))
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
    text(0,0, "please input a file")
  }
}


server <- function(input, output) {
  dataInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    results = list()
    
    results$df <- read.csv(inFile$datapath,header=T,sep=";")
    
    matD <- dist(results$df)
    results$cahS <- hclust(matD)
    
    results$rescah <- cutree(results$cahS,k=input$nbGroups)
    
    return(results)
  })
  
  output$plot1 <- renderPlot({
    f <- function(data){
      plot(data$cahS)
    }
    testNoFile(f, dataInput())
  })
  output$plot2 <- renderPlot({
    f2 <- function(data){
      plot(data$df,col=data$rescah)
    }
    testNoFile(f2, dataInput())
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