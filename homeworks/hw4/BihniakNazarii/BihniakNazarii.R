library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)

# Mamy wykres, który dla każdego roku
# wyborów prezydenckich w SZ pokazuje liczbę głosów
# za prezydenta demokrata, republikana albo innego.
# Z tego wykresu nie widać jaka jest tendencja głosów
# dla każdej partii, więc dodamy krzywą która to pokazuje

# Wykres generowany jest leprzy od pierwotnego, ponieważ
# zawiera linie, która dobrze pakazuje tendencje oraz
# widać z niej kiedy i która partia wygrała wybory
# w poszczególnym roku (łatwiej, niż porównywać słupki)

us_president_election <- read.csv("us_president_election.csv")

colnames(us_president_election)[colnames(us_president_election) == "party_simplified"] <- "Party"
#View(us_president_election)

ui <- fluidPage(
  
  titlePanel("US presidential election statistics"),
  
  
  fluidRow(
    column(6, 
           sliderInput("Years",
                       "Year",
                       value = c(min(us_president_election$Year), max(us_president_election$Year)),
                       min = min(us_president_election$Year),
                       max = max(us_president_election$Year),
                       step = 1)
           
    ),
    column(6,
           
    )
  ),
  fluidRow(
    column(6,
           plotlyOutput("pointPlot")
    ),
    column(6,
           plotlyOutput("histPlot")
    )
  ),
  fluidRow(
    column(6, 
           tableOutput("table")
    )
  )
)


server <- function(input, output) {

  
  output$histPlot <- renderPlotly({
    ggplotly(
      
      ggplot(us_president_election %>% 
               filter(Year >= input$Years[1] &
                      Year <= input$Years[2]  
                        )
             , 
             aes(x = as.factor(Year), y = candidatevotes, fill = Party)) +
        geom_smooth(aes(group = Party, color = Party), method = "lm", se = FALSE, formula = y ~ poly(x, 11)) + 
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Comparison of Votes by Party",
             x = "Year",
             y = "Number of Votes") +
        scale_fill_manual(values = c("REPUBLICAN" = "red", "DEMOCRAT" = "blue", "OTHER" = "gray")) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Adjust x-axis text size and angle
              legend.position = "top",  # Move legend to the top
              plot.title = element_text(hjust = 0.5),  # Center the title
              )
      
    )
  })
  
}


shinyApp(ui = ui, server = server)

View(us_president_election)
