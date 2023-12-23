
library(dplyr)
library(shiny)
library(plotly)


partie <- c("Prawo i Sprawiedliwość", "Koalicja Obywatelska", "Trzecia Droga", "Lewica",
            "Konfederacja", "Bezpartyjni Samorządowcy", "Polska Jest Jedna")
wyniki <- c(22.7, 40.66, 14.57, 12, 5.86, 3.09, 1.11)
wyniki_ludzie <- c(7907, 14164, 5075, 4182, 2043, 1077, 388)
df <- data.frame(partie, wyniki, wyniki_ludzie)
names(df) <- c("Partie", "Procenty", "Głosy")
df <- df %>% 
  mutate(Partie = forcats::fct_reorder(df$Partie, -df$Procenty))


plot1 <- plot_ly(df,
                 x = ~Partie,
                 y = ~Procenty,
                 type = "bar")
plot2 <- plot_ly(df,
                 x = ~Partie,
                 y = ~Głosy,
                 type = "bar",
                 marker = list(color = "red")) %>% 
  layout(yaxis = list(range = c(0, 15000)))

ui <- fluidPage(

    
    titlePanel("Wyniki wyborów do sejmu 2023 w Lesznie"),
    textOutput("subtitle"),

    
    sidebarLayout(
        sidebarPanel(
            selectInput("wybor", "Rodzaj danych w wykresie",
                        c("Procenty", "Liczba głosów"))
        ),

        mainPanel(
           plotlyOutput("barPlot")
        )
    )
)


server <- function(input, output) {
  
    output$subtitle <- renderText({
      paste("Frekwencja 77,83%")
    })

    output$barPlot <- renderPlotly({
      if(input$wybor == "Procenty")plot1
      else plot2
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
