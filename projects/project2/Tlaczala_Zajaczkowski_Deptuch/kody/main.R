library(shiny)

runApp <- function() {
  source("ui.R")
  source("server.R")

  shinyApp(ui = shinyUI, server = shinyServer)
}

shiny::runApp()