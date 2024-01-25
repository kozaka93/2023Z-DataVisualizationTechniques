
source("./src/mood/wykresNastrojuLolipop.R")
source("./src/mood/wykresNastrojuLine.R")
source("./src/mood/wykresEmocjiGraf.R")
source("./src/mood/wykresCzynnosciGraf.R")

moodLogic <- function(input, output, session) {
  
  
  output$moodPlotly <- renderPlotly({
    startDate <- as.Date(input$dateRange[1])
    endDate <- as.Date(input$dateRange[2])
    
    if (input$chartType == "Lollipop") {
      moodLollipop(startDate, endDate, input$selectPerson)
    } else if (input$chartType == "Line") {
      moodLine(startDate, endDate, input$selectPerson)
    }
  })
  
  output$emotionsPlot <- renderD3({
    emotionsGraph(input$selectPerson)
  })
  
  output$activitiesPlot <- renderD3({
    activGraph(input$selectPerson)
  })
  
  
}