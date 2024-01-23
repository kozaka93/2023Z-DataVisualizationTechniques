library(shiny)
library(shinydashboard)
library(stringr)
library(dplyr)
library(ggplot2)
library(forcats)
library(plotly)
library(scales)
library(DT)
library(shinycssloaders)
library(networkD3)
library(tidyr)
library(heatmaply)
library(rjson)
library(markdown)
#Load the function to the local through Paul Bleicher's GitHub page
source("imports/calendarheatmap.R")

# Przy odpalaniu trzeba zmienic working dir
#Norbert - setwd("~/Documents/informatyczne/iadstudia/twd/linux_me_project")

source("ui/about_ui.R")
source("ui/bash_history_ui.R")
source("ui/system_packages_ui.R")
source("ui/git_stats_ui.R")
source("ui/r_libs_ui.R")
source("ui/python_libs_ui.R")

source("server/bash_history_server.R")
source("server/system_packages_server.R")
source("server/git_stats_server.R")
source("server/r_libs_server.R")
source("server/python_libs_server.R")

options(spinner.type = 7, spinner.color = "#3c8dbc")

ui <- dashboardPage(
  dashboardHeader(title = "Linux ME Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Bash History", tabName = "bashHistory"),
      menuItem("System Packages", tabName = "systemPackages"),
      menuItem("Git Statistics", tabName = "gitStats"),
      menuItem("R Packages", tabName = "rLibs"),
      menuItem("Python Packages", tabName = "pythonLibs")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(
        ".nav-tabs-custom > .nav-tabs {
            background-color: #3c8dbc;
            color: white;
          }
         .nav-tabs-custom > .nav-tabs > li {
            color: white;
         }
         .nav-tabs-custom > .nav-tabs > li.active {
            background-color: brown;
         }
         
         .nav-tabs-custom > .nav-tabs > li.header {
            color: white !important;
         }
         
         .nav-tabs-custom > .nav-tabs > li > a {
            color: white;
         }
        
        .nav-tabs-custom > .nav-tabs > li > a:hover {
            color: #ccc;
         }
         
         .nav-tabs-custom > .nav-tabs > li.active > a {
            color: #444;
         }"
      ))
    ),
    
    tabItems(
      tabItem(tabName = "about", aboutUI("about")),
      tabItem(tabName = "bashHistory", bashHistoryUI("bashHistory")),
      tabItem(tabName = "systemPackages", systemPackagesUI("systemPackages")),
      tabItem(tabName = "gitStats", gitStatsUI("gitStats")),
      tabItem(tabName = "rLibs", rLibsUI("rLibs")),
      tabItem(tabName = "pythonLibs", pythonLibsUI("pythonLibs"))
    )
  )
)

server <- function(input, output, session) {
  callModule(bashHistoryServer, "bashHistory")
  callModule(systemPackagesServer, "systemPackages")
  callModule(gitStatsServer, "gitStats")
  callModule(rLibsServer, "rLibs")
  callModule(pythonLibsServer, "pythonLibs")
}

shinyApp(ui, server)
