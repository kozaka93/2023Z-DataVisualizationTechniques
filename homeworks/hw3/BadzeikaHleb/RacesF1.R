library(leaflet)
library(sf)
library(maps)
library(RColorBrewer)

# Read and merge your datasets
races <- read.csv("/path/to/races.csv")
circuits <- read.csv("/path/to/circuits.csv")
data <- merge(races, circuits, by = "circuitId")

# Count races per country
race_counts <- as.data.frame(table(data$country))

# Load world map
world <- st_as_sf(map("world", fill = TRUE, plot = FALSE))

# Merge with race data
world_data <- merge(world, race_counts, by.x = "ID", by.y = "Var1")

# Create a color palette
pal <- colorNumeric(palette = "YlOrRd", domain = world_data$Freq)

# Create the map
leaflet(world_data) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(Freq),
              fillOpacity = 0.7,
              color = "#FFFFFF",
              weight = 1,
              popup = ~paste(ID, Freq, "races")) %>%
  addLegend(pal = pal, values = ~Freq, opacity = 0.7)
