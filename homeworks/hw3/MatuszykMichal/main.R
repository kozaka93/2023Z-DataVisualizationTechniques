df <- read.csv("c:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/hw3/alcohol-consumption-vs-gdp-per-capita.csv")


df
View(df)


country <- map_data("world")

index_table <- data.frame(region = unique(country$region),
                          index = rnorm(length(unique(country$region)), 0, 5))

df %>% filter(`Total.alcohol.consumption.per.capita..liters.of.pure.alcohol..projected.estimates..15..years.of.age.` > 0) %>% 
  group_by(Year) %>% summarise(count = n()) # Z tego wynika, ze najnowszym rokiem z danymi jest 2018

df %>% filter(Year == 2018)



country %>% 
  filter(long < 180 ) %>% 
  left_join(df_filtered, by = c("region", "Entity"))

country %>%
  filter(long < 180) %>% 
  left_join(index_table) %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = index), color = "black", linewidth = 0.3) +
  coord_map("mollweide") #+ 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0) +
  theme_void() +
  theme(legend.position = "top") +
  labs(fill = "Index") 
  
  
  ###########################################################
  
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  
  
  world <- ne_countries(scale = "medium", returnclass = "sf") 
  df_filtered <- df %>% filter(Year == 2018) %>% # Najnowsze dane jakie znalazlem
    rename("alcohol_consumption_in_l" = "Total.alcohol.consumption.per.capita..liters.of.pure.alcohol..projected.estimates..15..years.of.age.") # zmieniam nazwe kolumny, bo strasznie dluga
  
  
  
  merged_data <- merge(world, df_filtered, by.x = "name", by.y = "Entity", all.x = TRUE) #łączy dane 
  
  
  ggplot() +
    geom_sf(data = merged_data, aes(fill = alcohol_consumption_in_l)) + 
    scale_fill_gradient2(low = "lightgreen", # poza tym, wg mnie lepiej widac te ktore maja niski poziom
                         mid = "#fc9272",
                         high = "#e34a33", # kolory wybrane z colorbrewer2
                         midpoint = 10) +
    theme_minimal() +
    labs(title = "Alcohol consumption per capita around the World")
  