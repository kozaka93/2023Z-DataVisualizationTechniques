library(plotly)
library(shinyjqui)
library(shinydashboard)
library(r2d3)

generate_mood_ui <- function() {
  
moodUI <- fluidPage(
  titlePanel("Analysis of mood, emotions and activities"),
      fluidRow(
        column(
          box(
            title = "How are you feeling today?",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            p("This plot shows your mood in time. You can choose the date range and the type of chart (see on the left).
            Note that for some days the data has been collected more than once, resulting in multiple points on the lollipop chart.
            The line chart shows the average mood for each day. ", style = "font-size: 20px;"),
            ),
          box(
            title = "Further insights",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            p("Embark on a visual journey into the life of an average student at Warsaw University of Technology through 2 bubble charts. 
            The first plot portrays emotions, offering insights into the emotional landscape of student life. 
            The second focuses on daily activities. On both plots circle sizes reflect the frequency of given act.", style = "font-size: 20px;"),
            ),
          width = 3
          ),
        box(
          title = "Mood in time",
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 9,
          plotlyOutput("moodPlotly", height = "600px")
        )
        ),
      fluidRow(
        box(
          title = "Emotions",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 6,
          d3Output("emotionsPlot", height = "600px")
        ),
        box(
          title = "Activities",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 6,
          d3Output("activitiesPlot", height = "600px")
        )
      )
  
)

return(moodUI)

}