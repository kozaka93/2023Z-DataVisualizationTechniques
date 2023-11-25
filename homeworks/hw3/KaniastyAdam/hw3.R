library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(htmlwidgets)

data <- read.csv("./repo/homeworks/hw3/KaniastyAdam/covid.csv")

world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
data <- world %>%
  left_join(data, by = c("ID" = "country"))

pal <- colorBin("YlOrRd", domain = data$total_cases_per_1m_population, bins = 5)

map <- leaflet(data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(total_cases_per_1m_population),
    fillOpacity = 0.7,
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~as.character(total_cases_per_1m_population),
    labelOptions = labelOptions(
      style = list(padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    popup = ~paste0(
      "<table>",
      "<tr><th>Country:</th><td>", ID, "</td></tr>",
      "<tr><th>Total Deaths:</th><td>", total_deaths, "</td></tr>",
      "<tr><th>Total Confirmed:</th><td>", total_confirmed, "</td></tr>",
      "<tr><th>Total Recovered:</th><td>", total_recovered, "</td></tr>",
      "</table>"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~total_cases_per_1m_population,
    opacity = 0.7,
    title = "Total Deaths",
    position = "bottomright"
  )

saveWidget(map, './repo/homeworks/hw3/KaniastyAdam/map.html', selfcontained = TRUE)

map