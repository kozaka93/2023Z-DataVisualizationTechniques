library(ggplot2)
library(dplyr)
library(leaflet)
library(stringr)

#https://github.com/fivethirtyeight/data/tree/master/alcohol-consumption
drinks <- read.csv("drinks.csv") %>%
  mutate(
    country = recode(
      str_trim(country),
      "Russian Federation" = "Russia",
      "Bosnia-Herzegovina" = "Bosnia and Herzegovina",
      "Serbia" = "Republic of Serbia",
      "USA" = "United States of America",
      "Tanzania" = "United Republic of Tanzania",
      "Congo" = "Republic of the Congo",
      "Guinea-Bissau" = "Guinea Bissau",
      "DR Congo" = "Democratic Republic of the Congo"
    )
  )

world <-
  read_sf(
    "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
  )

data <- world %>%
  left_join(drinks, join_by("name" == "country"))

pal <- colorBin("Oranges", domain = data$beer_servings)

url <-
  'https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}{r}.png'

labels <- sprintf(
  "<b>%s </b><br>Beer servings:
  %s<br> Wine servings: %s<br> Spirit servings:
  %s<br> Total liters of alcohol: %s",
  data$name,
  data$beer_servings,
  data$wine_servings,
  data$spirit_servings,
  data$total_litres_of_pure_alcohol
) %>% lapply(htmltools::HTML)

title_leged <-
  sprintf("Avarage number of beer<br>servings per person") %>% lapply(htmltools::HTML)

title <-
  sprintf(
    "<b>The average number of servings of beer drunk per person (2010)</b><br>additional
             information about wine, spirit and pure alcohol consumption"
  ) %>% lapply(htmltools::HTML)

plot <- leaflet(data) %>%
  addTiles(urlTemplate = url) %>%
  setView(lng = 15, lat = 50, zoom = 4) %>%
  addPolygons(
    fillColor = ~ pal(data$beer_servings),
    weight = 0.5,
    opacity = 1,
    color = "grey",
    fillOpacity = 1,
    highlightOptions = highlightOptions(weight = 5,
                                        color = "cadetblue"),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = data$beer_servings,
    opacity = 0.7,
    title = title_leged,
    position = "bottomright"
  ) %>%
  addControl(title, position = "topright")
