### HW 3
### libraries and packages
library(plotly)
library(dplyr)
library("RColorBrewer")
library(nycflights13)

flights <- nycflights13::flights
airports <- read.csv("airports.csv")

options(stringsAsFactors = FALSE)


### data frames 

ave_flight_from_nyc <- flights %>%  
  filter(!is.na(air_time)) %>% 
  group_by(dest) %>% 
  summarise(distance = mean(distance),
            air_time = mean(air_time),
            ave_delay = mean(arr_delay))


df <- airports %>% 
  right_join(ave_flight_from_nyc, by = c("iata" = "dest")) %>% 
  mutate(distance = round(distance),
         air_time = round(air_time),
         delay = round(ave_delay)) %>%  
  mutate(formatted_time = sprintf("%d:%02d", air_time%/%60, air_time%%60))


### plot 

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray90"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

plot <- plot_geo(df, lat = ~lat, lon = ~long) %>% 
  add_markers(
  text = ~paste(
    paste("City: ", city),
    paste("Airport: ", airport), 
    paste("IATA code: ", iata), 
    paste("State: ", state),
    paste("Distance to NY:", distance, "mi"), 
    paste("Flight time to NY: ", formatted_time),
    paste("Average delay on flights from NY: ", delay, "min"),
    sep = "<br />"
    ),
  color = ~delay, 
  symbol = I("square"), 
  size = I(13), 
  hoverinfo = "text") %>% 
  colorbar(title = "Avearge delay<br />on arrivals from NY,<br />in minutes") %>% 
  layout(
  title = 'Airports in the USA<br />which serve New York City (JFK, LGA, EWR airports)', geo = g
)

plot

