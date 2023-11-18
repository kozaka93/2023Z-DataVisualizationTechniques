library(ggplot2)
library(dplyr)
library(leaflet)
library(geojsonsf)
library(geojson)
library(geojsonio)

#https://api.worldbank.org/v2/en/indicator/SG.GEN.PARL.ZS?downloadformat=csv
data <- read.csv("data.csv")
#https://www.kaggle.com/datasets/iamsouravbanerjee/world-population-dataset
population <- read.csv("world_population.csv")
#http://techslides.com/list-of-countries-and-capitals#google_vignette
capital_data <- read.csv("country-capitals.csv")
data <-
  merge(data, population, by.x = "Country.Code", by.y = "CCA3") %>%
  select(
    Country.Territory,
    Country.Code,
    Y2022,
    Capital,
    Continent,
    X2020.Population,
    Area,
    Density_per_km,
    Growth.Rate,
    World.Population.Percentage
  )
data <-
  merge(data, capital_data, by.x = "Country.Territory", by.y = "CountryName")

geojson_url <-
  "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
countries <- read_sf(geojson_url)
countries <-
  merge(countries,
        data,
        by.x = 'id',
        by.y = "Country.Code",
        all.x = T) %>% filter(name != "Antarctica")
pal <- colorNumeric('Oranges', domain = countries$Y2022)

#####################
labels <-
  sprintf(
    "<b>%s</b><br>Capital: %s<br>Population: %s<br>Women in parliament: %s%%",
    countries$name,
    countries$Capital,
    countries$X2020.Population,
    round(countries$Y2022, 2)
  ) %>%
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(
    data = countries,
    color = 'black',
    weight = 0.5,
    fillOpacity = 1,
    fillColor = ~ pal(countries$Y2022),
    highlightOptions = highlightOptions(weight = 4,
                                        color = "black"),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addCircleMarkers(
    data = countries,
    lng = ~ as.numeric(CapitalLongitude),
    lat = ~ as.numeric(CapitalLatitude),
    radius = 2,
    color = "black",
    opacity = 2,
    popup = ~ paste(Capital)
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = countries$Y2022,
    title = HTML(
      "<span style='font-family: Times New Roman;'>Percent of <br>Women in <br> Parliament</span>"
    ),
    labFormat = labelFormat(suffix = "%"),
    opacity = 1
  ) %>%
  addControl(html = "<div style='position:absolute; bottom:0px; opacity:0.8 ;right:0px; background:white; padding:5px; border:0px solid black; width:75px; border-radius: 5px; font-family: Georgia; box-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);'>Black points represent capitals</div>",
             position = "bottomright") %>%
  addControl(html = "<div style='background:white; padding:5px; border:0px solid black; width:300px; text-align:center; border-radius: 5px; font-family: Georgia; box-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3); font-size: 15px;'>Map representing Percent of Women in Parliament by Country (and some other general info about countries)</div>",
             position = "topright")

