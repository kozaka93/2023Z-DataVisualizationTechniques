library(shiny)
options(shiny.autoreload = TRUE)
Sys.setlocale("LC_ALL", "en_US.UTF-8")
try(
  setwd("./projekty/health/")
)
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)
