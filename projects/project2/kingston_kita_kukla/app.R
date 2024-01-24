library(shiny)
library(ggplot2)
library(dplyr)
library(ggalluvial)

library(hms)
library(stringr)
library(plotly)

library(countrycode)
library(janitor)
library(padr)
library(hablar)
library(stringi)
library(lubridate)
library(tidyverse)
library(ggbump)

library(igraph)
library(ggraph)
library(visNetwork)
library(calendR)
library(wordcloud2)
library(wordcloud)
library(tidytext)
library(shinythemes)

#wczytanie danych
source("kody/miloszowa_obrobka.R")
source("kody/michalowa_obrobka.R")

#funkcje wszelakie
source("kody/boxarob.R")
source("kody/sluparob.R")
source("kody/generate_wordcloud.R")
source("kody/miloszowa_obrobka.R")
source("kody/kalendarzrob.R")
source("kody/zawez_kalendarzem.R")
source("kody/plotlajek.R")



ui1 <- fluidPage(theme = shinytheme("united"),
  titlePanel("Historia Wyszukiwania"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_words_slider", "Liczba słów:", min = 1, max = 50, value = 25, step = 1),
      radioButtons("check3", "Chłopy",  choiceNames=c("Miłosz", "Michał", "Antek"), choiceValues=c("df_words1_mil", "df_words1_mic", "df_words1_ant"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Rozrzut słowny",wordcloud2Output("wordcloud_plot")),
        tabPanel("Takie tam słupki",plotlyOutput("plotlajek"))
      )
    )
  )

)
  

ui2<- fluidPage(theme = shinytheme("united"),
  titlePanel("Historia Oglądania"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("check2", "Chłopy",  choiceNames=c("Miłosz", "Michał", "Antek"), choiceValues=c("df_milosz", "df_michal", "df_antek")),
      radioButtons("radjo", "Miesięcznica czy tygodnik?", choiceNames = c("Kaczyński", "Turowicz"), choiceValues = c("kaczor", "turowicz")),
      dateRangeInput("chlop", "Kres twych dni", start="2023-07-01", end="2023-12-31"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Boxy",  plotlyOutput("plot2")),
        tabPanel("Kalendorz",plotOutput("kalendarz")),
        tabPanel("Słupy",plotlyOutput("plot4")),
        
      )
    )  
  ),
)



ui <- navbarPage(theme = shinytheme("united"),
  title="Permanentna inwigilacja",
  tabPanel("Historia Wyszukiwania", ui1),
  tabPanel("Historia Oglądania", ui2),
  
  footer = shiny::HTML("
                  <footer class='text-center text-sm-start' style='width:100%;'>
                  <hr>
                  <p class='text-center' style='font-size:12px;'>
                    © 2024
                    <a class='text-dark' href='https://www.youtube.com/watch?v=dQw4w9WgXcQ' >Miłosz Kita</a>,
                    <a class='text-dark' href='https://www.youtube.com/watch?v=dQw4w9WgXcQ' >Michał Kukla</a>,
                    <a class='text-dark' href='https://www.youtube.com/watch?v=dQw4w9WgXcQ' >Antoni Kingston</a>
                  </p>
                  </footer>
                  "),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #inicjalizacja
  df1 <- df_words1_mil
  df2 <- df_milosz
  output$wordcloud_plot <- renderWordcloud2(generate_wordcloud(df1$word, df1$n, 25))
  output$plotlajek <- renderPlotly(plotlajek(df1,25))
  output$plot2 <- renderPlotly(boxarob(df2,"kaczor"))
  output$kalendarz <- renderPlot(robkalendarz(df2,19539, 19722))
  output$plot4 <- renderPlotly(sluparob(df2, "kaczor"))
  #logika 2. zakładki
  observeEvent(input$check2,{
    
    wyb <- input$check2
    df2 <- get(wyb)
    pocz <- as.integer(input$chlop[1])
    kon <- as.integer(input$chlop[2])
    tmp <- zawez_kalendarzem(df2,pocz,kon)
    output$plot2 <- renderPlotly(boxarob(tmp,input$radjo))
    output$plot4 <- renderPlotly(sluparob(tmp,input$radjo))
    output$kalendarz <- renderPlot(robkalendarz(tmp,pocz,kon))
  })
  observeEvent(input$radjo,{
    wyb <- input$check2
    df2 <- get(wyb)
    pocz <- as.integer(input$chlop[1])
    kon <- as.integer(input$chlop[2])
    tmp <- zawez_kalendarzem(df2,pocz,kon)
    output$plot2 <- renderPlotly(boxarob(tmp,input$radjo))
    output$plot4 <- renderPlotly(sluparob(tmp,input$radjo))
               })
  observeEvent(input$chlop,{
    wyb <- input$check2
    df2 <- get(wyb)
    pocz <- as.integer(input$chlop[1])
    kon <- as.integer(input$chlop[2])
    tmp <- zawez_kalendarzem(df2,pocz,kon)
    output$plot2 <- renderPlotly(boxarob(tmp,input$radjo))
    output$plot4 <- renderPlotly(sluparob(tmp,input$radjo))
    output$kalendarz <- renderPlot(robkalendarz(tmp,pocz,kon))
  })
  
  
 
  
   
   
  
   #logika 1 zakładki
   observeEvent(input$check3, {
     wyb=input$check3
     df1=get(wyb)
     output$plotlajek <- renderPlotly(plotlajek(df1,input$num_words_slider))
     output$wordcloud_plot <- renderWordcloud2(generate_wordcloud(df1$word, df1$n, input$num_words_slider))
     
   })
   observeEvent(input$num_words_slider, {
     wyb=input$check3
     df1=get(wyb)
     output$plotlajek <- renderPlotly(plotlajek(df1,input$num_words_slider))
     output$wordcloud_plot <- renderWordcloud2(generate_wordcloud(df1$word, df1$n, input$num_words_slider))
     
   })
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
