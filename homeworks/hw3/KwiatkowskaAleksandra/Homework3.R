library(dplyr)
library(mapdata)
library(leaflet)
library(htmlwidgets)

# Dane dotyczące elektrowni na świecie
data <- read.csv("global_power_plant_database.csv")

data %>% 
  filter(capacity_mw > 1000) %>% 
  mutate(radius = case_when(capacity_mw < 2000 ~ 4.5,
                            capacity_mw < 4000 ~ 6.5,
                            TRUE ~ 8.5))-> data

mypalette <- colorFactor(palette = c("#FFB70F", "dodgerblue3", "black", "brown", "#54FF9F", "#B0E2FF", "yellow", "#FFF5EE", "snow4", "#FFBBFF"), 
                         domain = c("Gas", "Hydro", "Coal", "Oil", "Nuclear", "Wind", "Solar", "Cogeneration", "Petcoke", "Geothermal"),
                         ordered = TRUE)
legend_labels <- c("Gas", "Hydro", "Coal", "Oil", "Nuclear", "Wind", "Solar", "Cogeneration", "Petcoke", "Geothermal")
legend_colors <- mypalette(legend_labels)

title <- sprintf("<b>Global Distribution of Power Plants (2021)</b><br>
                 Mark size corresponds to power plant's electrical generating capacity") %>% lapply(htmltools::HTML)

mapa <- leaflet(data) %>%
  addTiles() %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  setView(lat=50, lng=15, zoom=4) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    fillColor = ~mypalette(primary_fuel),
    fillOpacity = 0.7,
    color = "black",
    radius = ~radius, 
    weight = 1.5,
    popup = ~paste("Name: ", name, "<br>Capacity: ", capacity_mw, " MW")
  ) %>% 
  addLegend(colors = legend_colors, labels = legend_labels, opacity =0.7, title = "Energy source", position = "bottomright" ) %>% 
  addControl(title, position = "topright")

mapa
  
saveWidget(mapa, file = "Map.html")

