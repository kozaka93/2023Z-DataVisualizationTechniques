library(dplyr)
library(plotly)
library(htmltools)
library(tidyr)

# File paths
# input_file <- "./data/sleep/sleep-export/sleep-export-H2.csv"


actigraphPlot <- function(date, user) {
  
  if(user == "Hubert") {
    input_file <- "./data/sleep/sleep-export/sleep-export-H2.csv"
  }
  else if(user == "Mateusz") {
    input_file <- "./data/sleep/sleep-export/sleep-export-M.csv"
  }
  else if(user == "Adam") {
    input_file <- "./data/sleep/sleep-export/sleep-export-A.csv"
  }
  
  
  # Open the file for reading
  con <- file(input_file, "r")
  
  # Initialize variables
  file_number <- 1
  data_list <- list()
  current_data <- NULL
  start_reading <- FALSE
  
  # Process the file line by line
  while (TRUE) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    
    if (grepl("^Id,Tz,From,To", line)) {
      if (start_reading) {
        data_list[[paste0("sd", file_number)]] <- current_data
        file_number <- file_number + 1
      }
      current_data <- NULL
      start_reading <- TRUE
      
      # Store the column names and remove the double quotes
      col_names <- gsub('"', '', unlist(strsplit(line, ",")))
      
      # Read the next line for values
      line <- readLines(con, n = 1)
      values <- gsub('"', '', unlist(strsplit(line, ",")))
      
      # Create a dataframe with column names and values
      current_data <- as.data.frame(matrix(values, nrow = 1, byrow = TRUE))
      names(current_data) <- col_names
    }
  }
  
  # Save the last part
  if (!is.null(current_data)) {
    data_list[[paste0("sd", file_number)]] <- current_data
  }
  
  # Close the file
  close(con)
  
  # Format dates and times
  map_dates <- data.frame()
  
  for (i in 1:length(data_list)) {
    data_list[[paste0("sd", i)]] <- data_list[[paste0("sd", i)]] %>%
      select(-Event) %>%
      pivot_longer(cols = -c(Id, Tz, From, To, Sched, Hours, Rating, Comment, Framerate, Snore, Noise, Cycles, DeepSleep, LenAdjust, Geo), names_to = "Time", values_to = "Value")    
    
    data_list[[paste0("sd", i)]]$To <- as.POSIXct(data_list[[paste0("sd", i)]]$To, format = "%d. %m. %Y %H:%M")
    data_list[[paste0("sd", i)]]$From <- as.POSIXct(data_list[[paste0("sd", i)]]$From, format = "%d. %m. %Y %H:%M")
    # Format Value to numeric
    data_list[[paste0("sd", i)]]$Value <- as.numeric(data_list[[paste0("sd", i)]]$Value)
    
    data_list[[paste0("sd", i)]]$Time <- format(as.POSIXct(data_list[[paste0("sd", i)]]$Time, format = "%H:%M"), format = "%H:%M")
    # Create a map (date - index in the list)
    map_dates <- rbind(map_dates, data.frame(date = as.Date(data_list[[paste0("sd", i)]]$To[1]), ind = i))
  }
  
  # Each dataframe in the list is a sleep day containg columns such as To, Time, Value - 
  # these 3 are the most important
  
  
  if((date %in% map_dates$date) == FALSE) {
    # Empty plot with no X ticks and with 0-10 scale on Y axis and no data, with title of "No data for {date} for {user}"
    margin <- list(autoexpand = TRUE,
                   l = 30,
                   r = 20,
                   t = 50,
                   b = 100)
    # Dont draw the line
    plotly::plotly_empty() %>%
      layout(title = list(text = paste0("No data for ", date, " for ", user), font = list(color = "#f7f7f7")), 
             margin = margin,
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = 'rgba(0,0,0,0)'
             ) -> noDataPlot
    return(noDataPlot)
  }
  # Get the dataframe for the date
  df <- data_list[[map_dates[map_dates$date == date, ]$ind]]
  
  # Get the time and value for the date
  time <- df$Time
  value <- df$Value
  
  # Create a dataframe with time and value
  df <- data.frame(time, value)
  
  # Create area chart - actigraph
  p <- plot_ly(df, 
               x = ~time, 
               y = ~value, 
               type = 'scatter', 
               mode = 'lines',
               fill = 'tozeroy',
               fillcolor = 'rgba(96,92,168,0.5)',
               line = list(color = 'rgb(96,92,168)', width = 4,shape = "spline"),
               text = user,
               hovertemplate = paste0("<b>Time:</b> %{x}<br><b>Value: </b>%{y}<extra>%{text}</extra>")
               )
  
  margin <- list(autoexpand = TRUE,
                 l = 30,
                 r = 20,
                 t = 50,
                 b = 100)
  
  p <- layout(p, title = list(text = paste0("Actigraph plot for ", date, " for ", user), font = list(color = "#f7f7f7")), 
              xaxis = list(title = "Time during the night", tickangle = 45, titlefont = list(size = 16), 
                           color = "#f7f7f7",
                           gridcolor = 'rgba(247, 247, 247, 0.5)',
                           zerolinecolor = 'rgba(247, 247, 247, 0.5)'), 
              yaxis = list(title = "Sleep activity", titlefont = list(size = 16), 
                           color = "#f7f7f7",
                           gridcolor = 'rgba(247, 247, 247, 0.5)',
                           zerolinecolor = 'rgba(247, 247, 247, 0.5)'),
              annotations = list(x = 1, y = -0.3, text = "10 - fully awake, 0 - deeply asleep", 
                   showarrow = F, xref = "paper", yref = "paper", font=list(size=12, color="#f7f7f7")),
              margin = margin,
              plot_bgcolor = "rgba(0,0,0,0)",
              paper_bgcolor = 'rgba(0,0,0,0)'
              )
  
  
  return(p)
}
