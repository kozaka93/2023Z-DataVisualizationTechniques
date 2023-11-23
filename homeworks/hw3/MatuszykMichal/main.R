  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  
  df <- read.csv("c:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/hw3/alcohol-consumption-vs-gdp-per-capita.csv")
  
  
  
  world <- ne_countries(scale = "medium", returnclass = "sf") 
  df_filtered <- df %>% filter(Year == 2018) %>% # Najnowsze dane jakie znalazlem
    rename("alcohol_consumption_in_l" = "Total.alcohol.consumption.per.capita..liters.of.pure.alcohol..projected.estimates..15..years.of.age.") # zmieniam nazwe kolumny, bo strasznie dluga
  
  
  
  merged_data <- merge(world, df_filtered, by.x = "name", by.y = "Entity", all.x = TRUE) #łączy dane 
  
  
  world_map <- ggplot() +
    geom_sf(data = merged_data, aes(fill = alcohol_consumption_in_l)) + 
    scale_fill_gradient2(low = "lightgreen", # poza tym, wg mnie lepiej widac te ktore maja niski poziom
                         mid = "#fc9272",
                         high = "#e34a33", # kolory wybrane z colorbrewer2
                         midpoint = 10) +
    theme_minimal() +
    labs(title = "Alcohol consumption per capita around the World in") +
    coord_sf(crs = st_crs("+proj=moll"))
  
  ggsave("c:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/hw3/MatuszykMichal/map.jpg", plot = world_map, width = 40, height = 20, units = "cm", dpi = 600)
  
