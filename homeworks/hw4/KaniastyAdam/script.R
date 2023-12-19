library(plotly)
library(readr)
library(zoo)  # for rollmean

# Load and prepare data
data <- read_csv("./synthetic_gas_data_oscillating.csv")
data$Date <- as.Date(paste0(data$Date, "-01"), format = "%Y-%m-%d")

# Calculating 10-point trailing averages
data$Nordstream1_avg = rollmean(data$Nordstream1, 5, fill = NA, align = 'right')
data$Ukraine_avg = rollmean(data$Ukraine, 5, fill = NA, align = 'right')
data$Belarus_avg = rollmean(data$Belarus, 5, fill = NA, align = 'right')

fig <- plot_ly(data, x = ~Date, mode = "lines") %>%
  add_trace(y = ~Nordstream1, name = 'Nordstream 1', line = list(color = 'rgba(255, 100, 100, 0.4)', dash = 'dash')) %>%
  add_trace(y = ~Ukraine, name = 'Ukraine', line = list(color = 'rgba(100, 100, 255, 0.4)', dash = 'dash')) %>%
  add_trace(y = ~Belarus, name = 'Belarus', line = list(color = 'rgba(100, 255, 100, 0.4)', dash = 'dash')) %>%
  add_trace(y = ~Nordstream1_avg, name = 'Nordstream 1 Avg', line = list(color = 'rgba(255, 100, 100, 0.8)')) %>%
  add_trace(y = ~Ukraine_avg, name = 'Ukraine Avg', line = list(color = 'rgba(100, 100, 255, 0.8)')) %>%
  add_trace(y = ~Belarus_avg, name = 'Belarus Avg', line = list(color = 'rgba(100, 255, 100, 0.8)')) %>%
  layout(title = 'Monthly Flow of Russian Natural Gas through Pipelines',
         xaxis = list(
           title = 'Date',
           rangeslider = list(type = 'date')
         ),
         yaxis = list(title = 'Flow (mcm/day)'),
         showlegend = TRUE,
         hovermode = 'closest')

