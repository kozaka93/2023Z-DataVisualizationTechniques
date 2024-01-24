library(plotly)
library(shinyjqui)
library(shinydashboard)

generate_sleep_ui <- function() {
  
sleepUI <- fluidPage(
  titlePanel("Quality and length of sleep analysis"),
  
  fluidRow(
    box(
      title = "Time to bed",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 3,
      p("Explore our nightly adventures through a captivating line plot featuring the sleep habits of all three of us. 
      Each line represents the time of falling asleep (Y-axis) across different days (X-axis). Notice the distinctive patterns – Adam, the night owl, consistently hits the sack latest, Mateusz, the early bird, calls it a night the soonest, and Hubert gracefully balances in between. 
      During the winter holidays, our bedtime stories took a fascinating turn, with a collective tendency to embrace the night until around 2 - 4 AM. The climax? 
      A sleepless New Year's celebration, peaking on December 31st, with Adam bidding adieu to the night at almost 5 in the morning.
        ", style = "font-size: 20px;"),
    ),
    box(
      title = "Bedtime line plot",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 9,
      height = "700px",
      shinyjqui::jqui_resizable(plotlyOutput("sleeptimePlot", height = "610px"))
    )
  ),
  # add some space between rows
  tags$hr(),
  
  fluidRow(
    box(
      title = "Actigraph",
      status = "warning",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 9,
      plotlyOutput("actigraphPlot")
    ),
    box(
      title = "Activity during sleep",
      status = "warning",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 3,
      p("Actigraph gives us an insight into the details of our sleep. It presents the sleep
        duration as well as activity during sleep, measured by the sounds like snoring or coughing
        but also by the movement of body. Since each graph represents a separate night, in order to
        see a graph user has to choose a specific date and a person for whom the graph should be drawn.", style = "font-size: 20px;"),
      dateInput(
        "dateActigraph",
        "Choose a date for actigraph:",
        value = "2023-12-12"
      )
    )
  )
    

)

return(sleepUI)

}