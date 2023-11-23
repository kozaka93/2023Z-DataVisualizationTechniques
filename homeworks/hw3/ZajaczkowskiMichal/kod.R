library(leaflet)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(geojsonio)
library(rworldmap)
library(htmltools)
# zrodlo
# https://en.wikipedia.org/wiki/Religion_in_the_European_Union

# wczytanie danych 
df <- read.csv("data.csv", sep = ";")
world_map <- getMap()


# czyszczenie danych
cleaned_data <- df %>% 
  select(-Total.Christians) %>% 
  mutate(mostPopularReligion = apply(.[ , -1], 1, function(x) names(x)[which.max(x)]))

cleaned_data[cleaned_data$Region =="Total Germany[32]", "Region"] <- "Germany"
cleaned_data[cleaned_data$Region =="Great Britain", "Region"] <- "United Kingdom"

#wyselekcjonowanie tylko krajów z UE
countries <- subset(world_map, NAME_SORT %in% cleaned_data$Region)

europe_data <- merge(countries, cleaned_data, by.x = "NAME_SORT", by.y = "Region")

leaflet(europe_data) %>%
  addTiles() %>%
  setView(lng = 20, lat = 55, zoom = 3) %>% 
  addPolygons(
    fillColor = ~colorFactor("Set1", mostPopularReligion)(mostPopularReligion),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    dashArray = "1",
    opacity = 1,
    # popup = ~paste(NAME, "<br>Most Popular Religion: ", mostPopularReligion),
    # wyróżnianie kraju po najechaniu
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    # wyswietlanie napisu po najechaniu na kraj
    label = ~paste(NAME, "Most Popular Religion: ", mostPopularReligion),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>% 
  # dodanie legendy
  addLegend(
    position = "bottomright",
    pal = colorFactor("Set1", europe_data$mostPopularReligion),
    values = ~mostPopularReligion,
    title = "Religion:",
    opacity = 1
  ) %>% 
  addControl(HTML("<strong>Most Popular Religions in EU Countries - 2015"),
             position = "topright") 
