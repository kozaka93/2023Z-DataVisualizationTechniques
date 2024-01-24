library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemr)
library(htmltools)
library(chron)

## Preprocesing data from csv

# File paths
input_file <- "./data/sleep/sleep-export/sleep-export-H2.csv"

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

# Format dates and times  - preparing only for this plot
for (i in 1:length(data_list)) {
  data_list[[paste0("sd", i)]] <- data_list[[paste0("sd", i)]] %>%
    select(-Event) %>%
    pivot_longer(cols = -c(Id, Tz, From, To, Sched, Hours, Rating, Comment, Framerate, Snore, Noise, Cycles, DeepSleep, LenAdjust, Geo), names_to = "Time", values_to = "Value")    
  
  data_list[[paste0("sd", i)]]$To <- as.POSIXct(data_list[[paste0("sd", i)]]$To, format = "%d. %m. %Y %H:%M")
  data_list[[paste0("sd", i)]]$From <- as.POSIXct(data_list[[paste0("sd", i)]]$From, format = "%d. %m. %Y %H:%M")
  data_list[[paste0("sd", i)]]$Value <- as.numeric(data_list[[paste0("sd", i)]]$Value)
  data_list[[paste0("sd", i)]]$Time <- format(as.POSIXct(data_list[[paste0("sd", i)]]$Time, format = "%H:%M"), format = "%H:%M")
}



# Main function returning the plot
sleeptimeLine <- function(startDate, endDate) {

sleeptime <- data_list[[1]] %>%
  select(From) %>%
  head(1)

for(i in 2:length(data_list)) {
  sleeptime <- rbind(sleeptime, data_list[[paste0("sd", i)]] %>%
                       select(From) %>%
                       head(1))
}

sleeptime %>% 
  arrange(From) -> sleeptime

sleeptime$Date <- as.Date(sleeptime$From, format = "%Y-%m-%d %H:%M:%S")
sleeptime$Time <- times(format(as.POSIXct(sleeptime$From, format = "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")) # tymczasowo do czasu pójścia spać dopisuje się też data
sleeptime$From <- NULL
sleeptime$TimeN <- as.numeric(sleeptime$Time)


# Remove duplicated dates in sleeptime, taking the first one
sleeptime %>% 
  distinct(Date, .keep_all = TRUE) -> sleeptimeH

sleeptimeH[19, "TimeN"] <- 0.0
sleeptimeH[19,"Time"] <- chron::as.times(0.0)
sleeptimeH$TimeP <- as.POSIXct(as.character(sleeptimeH$Time), format = "%H:%M:%S")

sleeptimeA <- read.csv("./data/sleep/sleep-export/sleeptime-A.csv", sep = ";")
sleeptimeA$Date <- as.Date(sleeptimeA$Date, format = "%Y-%m-%d")
sleeptimeA$Time <- times(format(as.POSIXct(sleeptimeA$Time, format = "%H:%M:%S"), format = "%H:%M:%S"))
sleeptimeA$TimeP <- as.POSIXct(as.character(sleeptimeA$Time), format = "%H:%M:%S")

sleeptimeM <- read.csv("./data/sleep/sleep-export/sleeptime-M.csv", sep = ";")
sleeptimeM$Date <- as.Date(sleeptimeM$Date, format = "%Y-%m-%d")
sleeptimeM$Time <- times(format(as.POSIXct(sleeptimeM$Time, format = "%H:%M:%S"), format = "%H:%M:%S"))
sleeptimeM$TimeP <- as.POSIXct(as.character(sleeptimeM$Time), format = "%H:%M:%S")


# Filtering dates
sleeptimeH %>% 
  filter(Date >= startDate & Date <= endDate) -> sleeptimeH

sleeptimeA %>%
  filter(Date >= startDate & Date <= endDate) -> sleeptimeA

sleeptimeM %>%
  filter(Date >= startDate & Date <= endDate) -> sleeptimeM

# Line plot - static
# sleeptimeH %>%
#   ggplot(aes(x = Date, y = TimeN - 0.04166667)) +
#   geom_line(aes(color = "Hubert"), size = 1.5) + 
#   geom_point(aes(color = "Hubert"), size = 4) + 
#   geom_line(data = sleeptimeA, aes(x = Date, y = TimeN - 0.04166667, color = "Adam"), size = 1.5) +
#   geom_point(data = sleeptimeA, aes(x = Date, y = TimeN - 0.04166667, color = "Adam"), size = 4) +
#   geom_line(data = sleeptimeM, aes(x = Date, y = TimeN - 0.04166667, color = "Mateusz"), size = 1.5) +
#   geom_point(data = sleeptimeM, aes(x = Date, y = TimeN - 0.04166667, color = "Mateusz"), size = 4) +
#   scale_color_manual(
#     name = "Person",
#     values = c("Mateusz" = "green4", "Adam" = "red3", "Hubert" = "navy")
#     ) +
#   theme(
#     plot.title = element_text(size = 20, hjust = 0.5),
#     axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 14),
#     axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 14),
#     axis.title = element_text(size = 16),
#     panel.grid.major.x = element_blank(),
#     panel.background = element_rect(fill = "white"),
#     legend.position = "bottom",
#     legend.box = "vertical",
#     legend.key = element_rect(fill = "white")
#   ) + 
#   labs(
#     title = "Time of going to sleep each day",
#     x = "Date",
#     y = "Time of going to sleep"
#   ) +
#   scale_y_chron(
#     format = "%H:%M"
#   ) -> sleeptime_plot


# Line plot in plotly

sleeptime_plotly <- plot_ly() %>%
  add_trace(
    data = sleeptimeH,
    name = "Hubert",
    type = "scatter",
    mode = "markers+lines",
    x = ~Date,
    y = ~TimeP,
    hoverinfo = "text",
    text = paste(
      "<b>", "Date: ","</b>", sleeptimeH$Date, "<br>",
      "<b>", "Time to bed: ","</b>", sleeptimeH$Time
    ),
    hovertemplate = paste(
      "%{text}",
      "<extra><b>Hubert</b></extra>"
    ),
    showlegend = TRUE,
    marker = list(color = 'rgb(13, 57, 162)', size = 8),
    line = list(color = 'rgb(13, 57, 162)', width = 4, shape = "spline")
  ) %>%
  add_trace(
    data = sleeptimeA,
    name = "Adam",
    type = "scatter",
    mode = "markers+lines",
    x = ~Date,
    y = ~TimeP,
    hoverinfo = "text",
    text = paste(
      "<b>", "Date: ","</b>", sleeptimeA$Date, "<br>",
      "<b>", "Time to bed: ","</b>", sleeptimeA$Time
    ),
    hovertemplate = paste(
      "%{text}",
      "<extra><b>Adam</b></extra>"
    ),
    showlegend = TRUE,
    marker = list(color = 'rgb(205, 12, 24)', size = 8),
    line = list(color = 'rgb(205, 12, 24)', width = 4, shape = "spline")
  ) %>%
  add_trace(
    data = sleeptimeM,
    name = "Mateusz",
    type = "scatter",
    mode = "markers+lines",
    x = ~Date,
    y = ~TimeP,
    hoverinfo = "text",
    text = paste(
      "<b>", "Date: ","</b>", sleeptimeM$Date, "<br>",
      "<b>", "Time to bed: ","</b>", sleeptimeM$Time
    ),
    hovertemplate = paste(
      "%{text}",
      "<extra><b>Mateusz</b></extra>"
    ),
    showlegend = TRUE,
    marker = list(color = 'rgb(0, 135, 45)', size = 8),
    line = list(color = 'rgb(0, 135, 45)', width = 4, shape = "spline")
  )

# Legend styling 
leg <- list(
  orientation = "h",
  x = 0.5,
  y = -0.2,
  xanchor = "center",
  font = list(
    family = "sans-serif",
    size = 14,
    color = "#f7f7f7"),
  bgcolor = "rgba(0,0,0,0)",
  bordercolor = "rgba(0,0,0,0)"
  )

margin <- list(autoexpand = TRUE,
               l = 30,
               r = 20,
               t = 50,
               b = 20)

sleeptime_plotly <- sleeptime_plotly %>%
  layout(
    title = list(text ="Time of going to sleep each day", font = list(color = "#f7f7f7")),
    xaxis = list(title = "Date",
                 color = "#f7f7f7",
                 gridcolor = 'rgba(247, 247, 247, 0.5)',
                 zerolinecolor = 'rgba(247, 247, 247, 0.5)'),
    yaxis = list(title = "Time of going to sleep", 
                 zeroline = FALSE, 
                 #range = c(-0.004,0.21),
                 type = "time",
                 tickformat = "%H:%M",
                 color = "#f7f7f7",
                 gridcolor = 'rgba(247, 247, 247, 0.5)',
                 zerolinecolor = 'rgba(247, 247, 247, 0.5)'
                 ),
    legend = leg,
    margin = margin,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"

  )

return (sleeptime_plotly)
}
