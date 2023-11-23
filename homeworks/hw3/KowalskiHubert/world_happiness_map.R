library(leaflet)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(sf)


# Read in the data
df <- read.csv("happiness2019.csv", header = TRUE, sep = ",")
# Read in spacial data for the map of the world
world <- st_as_sf(map("world", fill = TRUE, plot = FALSE))

# Cleaning
df <- df %>%
  rename(Country = Country.or.region) %>%
  mutate(Country = case_when(
              Country == "United States" ~ "USA",
              Country == "United Kingdom" ~ "UK",
              Country == "Congo (Brazzaville)" ~ "Democratic Republic of the Congo",  
              Country == "Congo (Kinshasa)" ~ "Republic of Congo",
              TRUE ~ Country))

# Merge the data
map <- world %>% 
  left_join(df, by = c("ID" = "Country"), multiple = "all") %>%
  rename(Country = ID)

map <- st_transform(map, crs = 4326)

pal <- colorNumeric(palette = "YlOrRd", domain = map$Score)

labels <- sprintf("<strong>Country: </strong>%s<br><strong>Score: </strong>%g", map$Country, map$Score) %>% lapply(htmltools::HTML)

# Create the map
m <- leaflet(map , options = leafletOptions(worldCopyJump = T)) %>%
  setView(0, 30, 2) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(Score),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      fillOpacity = 0.8,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addLegend(pal = pal, values = df$Score, title = "Happiness Score",
            position = "bottomright") %>%
  addMarkers(lng = 27, lat = 62, popup = "<strong>Happiest country:</strong> Finland")


m

# Changing projection does not work really well
# robinson <- leafletCRS(crsClass = "L.Proj.CRS",
#                        code = "robin",
#                        proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +lat_ts=0 +ellps=WGS84",
#                        resolutions = 2^(0:18))
