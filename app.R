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
      menuItem("Raw data with CAH groups", tabName = "cahresult"),
      menuItem("CAH groups on Chosen Vars", tabName = "groupsOnChosenVar")
    ),
    sliderInput("nbGroups", "Number of groups:",
                min = 3, max = 100, value = 4, step = 1 # TODO make max dynamic depending on number of rows
    ),
    checkboxInput("doScaleData", "Center reduce variables:", value = FALSE, width = NULL),
    selectizeInput("cahMethod", "Choose your method:", 
                   c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), 
                   selected = "complete", multiple = FALSE,
                   options = NULL),
    selectizeInput("distance", "Choose your distance:", 
                   c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                   selected = "euclidean", multiple = FALSE,
                   options = NULL),
    selectizeInput("var1name", "Choose first variable to plot:", NULL, 
                   selected = NULL, multiple = FALSE, options = NULL),
    selectizeInput("var2name", "Choose second variable to plot:", NULL, 
                   selected = NULL, multiple = FALSE, options = NULL)
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "CAH",
              fluidRow(
                box(plotOutput("plotDendo", height = 450)),
                box(plotOutput("plotHeight", height = 450))
              ),
              fluidRow(
                box(plotOutput("plotDiffHeight", height = 450)),
                box(plotOutput("plotGroups", height = 450))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "rawdata", tableOutput('rawData')),
      
      # Third tab content
      tabItem(tabName = "cahresult", tableOutput('dataCah')),
      
      # Fourth tab content
      tabItem(tabName = "groupsOnChosenVar", htmlOutput("plotGroupsForChosenVar"))
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

updateVarChoices <- function(df, session){
  names_ = names(df)
  updateSelectizeInput(session, "var1name", label = "Choose first variable to plot:", 
                       choices = names_, selected = names_[1], server = TRUE) # TODO need to know what this server option really means
  updateSelectizeInput(session, "var2name", label = "Choose second variable to plot:", 
                       choices = names_, selected = names_[2], server = TRUE) # TODO need to know what this server option really means
}

server <- function(input, output, session) {
  dfFromfileInput <- reactive({
    df <- read.csv(input$file1$datapath,header=T,sep=";")
    updateVarChoices(df, session)
    return(df)
  })
  
  dataInput <- reactive({
    inFile <- input$file1
    
    results = list()
    if (is.null(inFile)){
      results$df <- read.csv('./data/donclassif.txt',header=T,sep=";")
      updateVarChoices(results$df, session)
    }
    else {
      # results$df <- read.csv(inFile$datapath,header=T,sep=";")
      results$df <- dfFromfileInput()
    }
    
    if (input$doScaleData)
      data2work <- scale(results$df)
    else
      data2work <- results$df    
    
    matD <- dist(data2work, method= input$distance)
    results$cahS <- hclust(matD, method = input$cahMethod)
    results$sortedHeights <- sort(results$cahS$height,dec=T)
    
    results$rescah <- cutree(results$cahS,k=input$nbGroups)
    
    return(results)
  })
  
  output$plotDendo <- renderPlot({
    f <- function(data){
      par(mar=c(0, 4, 4, 2)) # c(bottom, left, top, right) : remove margin
      plot(data$cahS, xlab="", sub="")
      abline(data$sortedHeights[input$nbGroups-1], 0, col = "red")
    }
    testNoFile(f, dataInput())
  })
  
  output$plotHeight <- renderPlot({
    f <- function(data){
      plot(2:(input$nbGroups+6), data$sortedHeights[1:(input$nbGroups+5)],type="h",ylab="height",xlab="nbGroups", main='Height depending on number of groups')
      points(input$nbGroups, data$sortedHeights[input$nbGroups-1], type="p", col = "red", cex=3)
    }
    testNoFile(f, dataInput())
  })
  
  output$plotDiffHeight <- renderPlot({
    f <- function(data){
      diffHeight <- diff(data$sortedHeights)
      plot(3:(input$nbGroups+7), -diffHeight[1:(input$nbGroups+5)],type="h",ylab="Diff height",xlab="nbGroups", main='Neg differentiated height depending on number of groups')
      points(input$nbGroups, -diffHeight[input$nbGroups-2], type="p", col = "red", cex=3)
    }
    testNoFile(f, dataInput())
  })
  
  output$plotGroups <- renderPlot({
    f3 <- function(data){
      if (!("var1name" %in% input & "var2name" %in% input))
        plot(data$df[, c(1, 2)],col=data$rescah,
            xlab = names(data$df)[1],ylab = names(data$df)[2], main = "Groups on chosen variables plan")
      else
        plot(data$df[, c(input$var1name, input$var2name)],col=data$rescah,
            xlab = input$var1name,ylab = input$var2name, main = "Groups on chosen variables plan")
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
      data$df$group <- data$rescah
      return(data$df)
    }
    testNoFile(f, dataInput())
  })

  output$plotGroupsForChosenVar <- renderUI({ 
    #data <- dataInput()
  })
}

shinyApp(ui, server)