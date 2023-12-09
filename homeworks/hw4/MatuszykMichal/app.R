# Potrzebne pakiety
library(shiny) 
library(dplyr)
library(tidyr)
library(plotly)

# Wczytanie danych
data <- read.csv("c:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/hw4/poprawiony_wykres/Inflacja.csv")
data[, -1] <- lapply(data[, -1], function(x) as.numeric(gsub(",", ".", x)) - 100)

ui <- fluidPage(                                                                # Tworze prosty UI
  titlePanel("Inflation in Poland over the years"),                             # Dodaje tytul
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year", unique(data$year), selected = 2023),
      sliderInput("months", "Select Month Range", 1, ncol(data) - 1, value = c(1, ncol(data) - 1))
    ),
    mainPanel(
      plotlyOutput("plot"),
      textOutput("Dane z historyczne z gus") 
    )
  )
)

server <- function(input, output) {
  dat <- reactive({                                                             # wybieram odpowiednie dane
    selected_columns <- c("year", colnames(data)[(input$months[1] + 1):(input$months[2] + 1)])
    temp <- data %>% filter(year == input$year) %>% select(all_of(selected_columns)) %>%
      pivot_longer(cols = -year, names_to = "Month", values_to = "Inflation")
    temp$Month <- factor(temp$Month, levels = unique(temp$Month))
    temp %>% na.omit()
  })
  
  output$plot <- renderPlotly({
    plot_ly(dat(), x = ~Month, y = ~as.numeric(Inflation), type = 'bar', marker = list(color = "#4586ff")) %>%
      layout(
        title = paste("Poland - Inflation Data", 
                      "from", colnames(data)[(input$months[1] + 1)], 
                      "to", colnames(data)[(input$months[2] + 1)], 
                      input$year),
        xaxis = list(title = "Month", tickangle = -45, tickfont = list(size = 12)),
        yaxis = list(title = "Inflation (%)"),
        margin = list(l = 80, r = 50, b = 80, t = 80),
        font = list(size = 14, family = "Arial", color = "#444"),
        showlegend = FALSE,
        paper_bgcolor = '#f9f9f9',
        plot_bgcolor = '#f2f2f2',
        hoverlabel = list(bgcolor = 'white', font = list(size = 14)),
        barmode = 'overlay',
        bargap = 0.1
      ) 
  })
  
}

shinyApp(ui = ui, server = server)

