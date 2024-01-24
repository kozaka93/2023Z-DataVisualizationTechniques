source("./src/sleep/wykresSleepTimeLine.R")
source("./src/sleep/wykresActigraph.R")

sleepLogic <- function(input, output, session) {
  
  output$sleeptimePlot <- renderPlotly({
    startDate <- as.Date(input$dateRangeSleep[1])
    endDate <- as.Date(input$dateRangeSleep[2])
  
    sleeptimeLine(startDate, endDate)
    
  })
  
  output$actigraphPlot <- renderPlotly({
    dateActi <- as.Date(input$dateActigraph)
    actigraphPlot(dateActi, input$selectPerson) # Tu będzie podawany user, trzeba wtedy w funkcji actigraphPlot go uwzględnić
  })
  
  
}
