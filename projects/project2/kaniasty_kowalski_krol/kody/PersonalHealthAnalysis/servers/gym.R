library(r2d3)
library(jsonlite)
library(dplyr)
library(plotly)
library(tidyr)


source("./src/gym/spider.R")
source("./src/gym/muscles/muscles.R")
source("./src/gym/weightMusclesDistribution.R")
source("./src/gym/progress.R")
source("./src/gym/Calculations.R")

gymLogic <- function(input, output, session) {
  
  observe({
    selected_days <- as.numeric(input$spider_date_start)
    end_date <- Sys.Date()
    start_date <- end_date - selected_days + 1
    
    updateDateRangeInput(session, "dateRangeGym", start = start_date, end = end_date)
  })
  
  reactiveData <- reactiveVal()
  reactiveDataFiltered <- reactiveVal()
  
  observeEvent(input$selectPerson, {
    newData <- read.csv(paste0("./data/gym/", input$selectPerson, ".csv"))
    reactiveData(newData)
  }, ignoreNULL = TRUE)
  
  observe({
    data <- reactiveData()
    if(is.null(data)) {
      return(NULL)
    }
    data$start_time <- as.POSIXct(data$start_time, format="%d/%m/%Y %H:%M")
    data$end_time <- as.POSIXct(data$end_time, format="%d/%m/%Y %H:%M")
    start_date_posix <- as.POSIXct(input$dateRangeGym[1])
    end_date_posix <- as.POSIXct(input$dateRangeGym[2])
    
    reactiveData(data)
    
    filteredData <- data %>% filter(start_time >= start_date_posix & end_time <= end_date_posix)
    
    reactiveDataFiltered(filteredData)
  })
  

  output$gym_spider <- renderPlotly({
    gymSpider(reactiveDataFiltered(), reactiveData())
  })
  
  output$gym_weight_distribution <- renderPlotly({
    weightDistribution(reactiveDataFiltered(), input)
  })
  
  calculated_values <- reactive({
    start_date <- input$dateRangeGym[1]
    end_date <- input$dateRangeGym[2]
    return(gym_calculations(reactiveDataFiltered()))
  })
  
  output$total_reps <- renderText({
    calculated_values()$total_reps
  })
  
  
  output$total_duration <- renderText({
    calculated_values()$total_duration
  })
  
  
  output$total_volume <- renderText({
    calculated_values()$total_volume
  })
  
  
  output$number_of_workouts <- renderText({
    calculated_values()$number_of_workouts
  })
  
  output$gym_muscles <- renderImage({
    image <- musclesPlot(reactiveDataFiltered())
    list(
      src="image_composite.png"
    )
  }, deleteFile = T)
  
  output$stacked_progress <- renderD3({
    data <- reactiveData()
    processed_data <- data %>%
      mutate(week = week(as.Date(start_time, format = "%d/%m/%Y")),
             year = year(as.Date(start_time, format = "%d/%m/%Y")),
             total_weight = weight_kg * reps) %>%
      group_by(year, week, muscle_group) %>%
      summarize(total_weight = sum(total_weight, na.rm = TRUE)) %>%
      ungroup() %>%
      complete(year, week, muscle_group, fill = list(total_weight = 0)) %>%
      arrange(year, week)
    
      d3_data <- processed_data %>%
        pivot_wider(names_from = muscle_group, values_from = total_weight) %>%
        replace(is.na(.), 0)
      r2d3(data = d3_data, script = "www/gym/stacked_bar_chart.js")
  })
}
