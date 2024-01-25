source("./src/food/wykres1.R")

foodLogic <- function(input, output, session) {
  
  foodReact <- reactive(
  if (input$selectPerson == "Adam") {
    "./data/food/zyw_ak.csv"
  } else if (input$selectPerson == "Hubert") {
    "./data/food/zyw_hk.csv"
  } else {
    "./data/food/zyw_mk.csv"
  }
  )
  
  output$caloriePlot <- renderPlotly({
    caloriePlot(foodReact())
  })
  output$foodPlot <- renderPlotly({
    foodPlot(foodReact(), input$showModeFood)
  })
}
