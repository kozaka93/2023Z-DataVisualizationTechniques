setwd("C:\\Users\\basiu\\OneDrive\\Pulpit\\sem 3\\techniki wizualizacji danych\\hw4")
library(readxl)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(dplyr)

wybory23 <- data.frame(x = c("Prawo i Sprawiedliwość", "Koalicja Obywatelska",
                           "Trzecia Droga", "Lewica", "Konfederacja", "Bezpartyjni", "inni"),
                     y = c(36.8,31.6,13,8.6,6.2,2.4,1.4))
colnames(wybory23) <- c("Ugrupowanie", "Procent")
wybory23 %>% 
  mutate(Ugrupowanie = forcats::fct_reorder(wybory23$Ugrupowanie,-wybory23$Procent)) -> wybory23

plot23 <- plot_ly(wybory23, x = ~Ugrupowanie, y = ~Procent, type = 'bar',
               marker = list(color = "purple",
                             line = list(color = "black",
                                         width = 1.5)))
plot23 <- plot23 %>% layout(title = "Wybory Parlamentarne 2023",
                      xaxis = list(title = "Ugrupowanie"),
                      yaxis = list(title = "Procent głosów", range = c(0,50))
                                  
                                   )
plot23

wybory19 <- data.frame(x = c("Prawo i Sprawiedliwość", "Koalicja Obywatelska",
                             "Lewica", "Konfederacja", "PSL"),
                       y = c(43.6,27.4,11.9,9.6,6.4))
colnames(wybory19) <- c("Ugrupowanie", "Procent")
wybory19 %>% 
  mutate(Ugrupowanie = forcats::fct_reorder(wybory19$Ugrupowanie,-wybory19$Procent)) -> wybory19

plot19 <- plot_ly(wybory19, x = ~Ugrupowanie, y = ~Procent, type = 'bar',
                  marker = list(color = "purple",
                                line = list(color = "black",
                                            width = 1.5)))
plot19 <- plot19 %>% layout(title = "Wybory Parlamentarne 2019 (wyniki exit poll)",
                            xaxis = list(title = "Ugrupowanie"),
                            yaxis = list(title = "Procent głosów", range = c(0,50))
                                         
                                    )
plot19

wybory15 <- data.frame(x = c("Prawo i Sprawiedliwość", "Koalicja Obywatelska","Kukiz 15",
                             "Nowoczesna", 
                             "Lewica", "PSL", "Korwin", "Razem", "Inne"),
                       y = c(37.58,24.08,8.81,7.6,7.55,5.13,4.76,3.62,0.86))
colnames(wybory15) <- c("Ugrupowanie", "Procent")
wybory15%>% 
  mutate(Ugrupowanie = forcats::fct_reorder(wybory15$Ugrupowanie,-wybory15$Procent)) -> wybory15

plot15 <- plot_ly(wybory15, x = ~Ugrupowanie, y = ~Procent, type = 'bar',
                  marker = list(color = "purple",
                                line = list(color = "black",
                                            width = 1.5)))
plot15 <- plot15 %>% layout(title = "Wybory Parlamentarne 2015",
                            xaxis = list(title = "Ugrupowanie"),
                            yaxis = list(title = "Procent głosów", range = c(0,50))
                                         
                            )
plot15

library(shiny)

ui <- fluidPage(
  
  titlePanel("Wyniki wyborów parlamentarnych w latach 2015, 2019, 2023"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "rok",
        "Wybierz rok",
        2015,
        2023,
        2019,
        4
      )
    ),
    
    
    mainPanel(
    )
  ),
  fluidRow(
    column(6,
           plotlyOutput("barPlot")
    ))
)

server <- function(input, output) {
  output$barPlot <- renderPlotly({
    if(input$rok == 2015) plot15
    else if(input$rok == 2019) plot19
    else plot23
    
    
  })
}

shinyApp(ui = ui, server = server)




