rLibsUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 4,
        box(
          title = "R packages",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          includeMarkdown(
"This tab focuses on our use of different **R** programming language's packages. The key aspect of this tab is analysing **Imports** relation between packages. It describes which dependencies need to be imported for a specific package to work.

Kuba, Norbert, and Mateusz are quite experienced users of the **R** programming language, having learned it through *Structured Data Processing* and *Data Visualization Techniques* classes. They are primarily using it for data processing and visualization tasks. They are familiar with a variety of **R** packages that are widely used in the data science community, e.g.:
- dplyr,
- data.table,
- ggplot2,
- plotly, 
- shiny
- leaflet"
          )
        ),
        box(
          title = "Inputs",
          width = NULL,
          status = "warning",
          solidHeader = TRUE,
          selectInput(
            ns("person"), 
            label = "Choose person",
            choices = c("Mateusz", "Kuba", "Norbert")
          ),
          sliderInput(
            ns("mostFrequentlyImported"),
            label = "Select number of most frequently imported packages",
            min = 1,
            max = 12,
            value = 6,
            step = 1
          )
        )
      ),
      column(
        width = 8,
        fluidRow(
          infoBoxOutput(ns("rVersion"), width = 4) %>% withSpinner(),
          infoBoxOutput(ns("allPackages"), width = 4) %>% withSpinner(),
          infoBoxOutput(ns("basePackages"), width = 4) %>% withSpinner(),
        ),
        fluidRow(
          box(
            title = "Import frequency histogram",
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput(ns("importsHistogram")) %>% withSpinner()
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Graph of imports",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        simpleNetworkOutput(ns("importsNetwork")) %>% withSpinner()
      )
    )
  )
}