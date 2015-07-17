library(shiny)
library(igraph)
source("coinZ.R")

options(shiny.error=traceback)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # load the data and run coin
    load("Unamuno.RData")
    G<-gcoin(Unamuno, 1:31, UnamunoC, color=input$color, shape=input$shape, size=input$NodeSize, minimum=input$Minimum, p=as.numeric(input$pValue), lwidth=input$EdgeSize, layout=input$layout)
  })
})