library(shiny)
library(dplyr)
library(networkD3)
library(lubridate)
library(ggplot2)
library(tidytext)
library(stringi)
library(plotly)

words_Michal <- read.csv("data/michalWords.csv")
words_Mateusz <- read.csv("data/mateuszWords.csv")
words_Kornel <- read.csv("data/kornelWords.csv")
data_Mateusz <- read.csv("data/mateuszCombinedNoContent.csv")
data_Michal <- read.csv("data/michalCombinedNoContent.csv")
data_Kornel <- read.csv("data/kornelCombinedNoContent.csv")

shinyServer(function(input, output, session){
  # po tym reactive pracujemy na data i word_data jak na normalnych 
  # ramkach tylko trzeba pisac data() - z nawiasami
  data <- reactive({
    if(input$user == "Mateusz") {
      data_Mateusz
    }
    else if(input$user == "Kornel") {
      data_Kornel
    }
    else {
      data_Michal
    }
  })
  
  word_data <- reactive({
    if(input$user == "Mateusz") {
      words_Mateusz
    }
    else if(input$user == "Kornel") {
      words_Kornel
    }
    else {
      words_Michal
    }
  })

  output$plot <- renderForceNetwork({
    df <- word_data() %>%
      filter(nchar(word) %in% input$charNr)
    
    df <- df %>% 
      arrange(desc(count)) %>% 
      head(input$wordsNr)

    Links <- data.frame(
      source = rep(0, nrow(df)),   # Źródło krawędzi
      target = 1:nrow(df),         # Cel krawędzi
      value = df$count             # Wartość (liczba wystąpień słowa)
    )
    
    # Tworzenie węzłów (Nodes) - użyj kolumny word jako nazwy węzłów
    Nodes <- data.frame(
      name = c(input$user, df$word),     # Nazwy węzłów
      size = c(20, df$count*6000/max(df$count)),          # Rozmiar węzłów
      group = c(0, rep(1, nrow(df)))     # Grupa węzłów
      # group = c(" ", word_data$count)
    )

    # Skala kolorów
    ColourScale <- 'd3.scaleOrdinal()
            .domain(["me", "Word Group"])
            .range(["#213423", "#ff0050"]);'
    
    # Tworzenie grafu siłowego
    forceGraph <- forceNetwork(
      Links = Links,
      Nodes = Nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      Group = "group",
      opacity = 1,
      zoom = TRUE,
      Nodesize = "size",
      linkDistance = 200,
      linkWidth = 4,
      charge = -300,
      opacityNoHover = 1,
      legend = FALSE,
    )
    
    htmlwidgets::onRender(
      forceGraph,
      'function(el, x) {
        d3.selectAll(".node text").style("fill", "black");
      }'
    )

  })

  output$plotKiedy <- renderPlotly({
    df <- data() %>%
      mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01")) %>%
      filter(
        sender_name == ifelse(input$user == "Mateusz", "Mati Deptuch",
          ifelse(input$user == "Kornel", "Kornel Tłaczała", "Michał Zajączkowski")
        )
      ) %>%
      mutate(
        Date = as.Date(timestamp),
        Time = format(timestamp, "%H:%M:%Y"),
        Hour = hour(timestamp),
        Month = month(timestamp),
        Day = day(timestamp)
      )
    
    if (input$time == "Hour") {
      plotdata <- df %>%
        group_by(Hour) %>%
        summarise(n = n())
      plotdata$n <- plotdata$n / 365
      
      p <- plot_ly(plotdata, x = ~Hour, y = ~n, type = 'bar', marker = list(color = "#0594ff")) %>%
        layout(xaxis = list(title = "Hour", tickmode = "array", tickvals = seq(0, 24, 1)), yaxis = list(title = "Average number of sent messages"))
    }

    if (input$time == "Day Of Month") {
      plotdata <- df %>%
        group_by(Day) %>%
        summarise(n = n())
      plotdata$n <- plotdata$n / 12
      
      p <- plot_ly(plotdata, x = ~Day, y = ~n, type = 'bar', marker = list(color = "#0594ff")) %>%
        layout(xaxis = list(title = "Day Of Month", tickmode = "array", tickvals = seq(1, 31, 1)), yaxis = list(title = "Average number of messages sent daily"))
    }

    if (input$time == "Month") {
      plotdata <- df %>% 
        group_by(Month) %>% 
        summarise(n = n())

      plotdata$Month <- month.abb[plotdata$Month]

      p <- plot_ly(plotdata, x = ~Month, y = ~n, type = 'bar', marker = list(color = "#0594ff")) %>%
        layout(xaxis = list(title = "Month", categoryorder = "array", categoryarray = month.abb),
              yaxis = list(title = "Number of sent messages"))
    }
    
    p %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        font = list(family = "Helvetica Neue", size = 18)
      ) %>% 
    config(displayModeBar = FALSE)
  })
  
output$plotZKim <- renderPlotly({
  who <- case_when(
    input$zKim == "Groups" ~ c("True"),
    input$zKim == "People" ~ c("False"),
    input$zKim == "All" ~ c("True", "False")
  )
  
  plotdata <- data() %>%
    filter(
      sender_name == ifelse(
        input$user == "Mateusz", "Mati Deptuch",
        ifelse(input$user == "Kornel", "Kornel Tłaczała", "Michał Zajączkowski")
      )
    ) %>% 
    filter(is_group %in% who)%>%
    group_by(receiver_name) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(10)
  
  
  plot_ly(
    plotdata,
    x = ~n,
    y = ~reorder(substr(receiver_name, 1, 20), n),
    type = 'bar'
   ) %>%
    layout(
      xaxis = list(
        title = "Number of sent messages",
        fixedrange = TRUE),
      yaxis = list(title = "Person/group name", fixedrange = TRUE),
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      font = list(size = 16, family = "Arial Black")
    ) %>% 
    config(displayModeBar = FALSE)
})

output$plotOdKogo <- renderPlotly({
  who <- case_when(
    input$odKogo == "Groups" ~ c("True"),
    input$odKogo == "People" ~ c("False"),
    input$odKogo == "All" ~ c("True", "False")
  )
  
  plotdata <- data() %>%
    filter(
      sender_name != ifelse(
        input$user == "Mateusz", "Mati Deptuch",
        ifelse(input$user == "Kornel", "Kornel Tłaczała", "Michał Zajączkowski")
      )
    ) %>% 
    filter(is_group %in% who)%>%
    group_by(sender_name) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(10)
  
  plot_ly(
    plotdata,
    x = ~n,
    y = ~reorder(substr(sender_name, 1, 20), n),
    type = 'bar'
    ) %>%
    layout(
      xaxis = list(
        title = "Number of sent messages",
        fixedrange = TRUE),
      yaxis = list(title = "Person/group name",
      fixedrange = TRUE),
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      font = list(size = 16, family = "Arial Black")
    ) %>% 
    config(displayModeBar = FALSE)
})
  
})
