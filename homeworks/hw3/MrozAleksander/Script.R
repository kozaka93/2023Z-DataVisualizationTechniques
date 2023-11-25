install.packages("mapdata")
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)

#https://www.kaggle.com/datasets/codebreaker619/alcohol-comsumption-around-the-world/

df <- read.csv("drinks.csv")
world_map <- map_data("world")
mapdata <- left_join(world_map, df, by = c("region" = "country")) %>% 
  filter(region != "Antarctica") 


map <- ggplot(mapdata, aes(x = long, y = lat, group = group, fill = total_litres_of_pure_alcohol)) +
  geom_polygon(colour = "white", linewidth = 0.1) + 
  scale_fill_gradient(
    name = "Total litres of pure alcohol\nconsumed yearly per person",
    low = "#16AF38",
    high = "#0E5F20",
    na.value = "gray",
    space = "Lab",
    guide = "colourbar"
  ) +
  theme_void() +
  labs(
    title = "Alcohol consumption around the world"
  ) +
  theme(
    plot.title = element_text(hjust = 0.05)  # Dostosuj wartość hjust, aby przesunąć w prawo
  ) + coord_fixed(1.3)

map

ggsave("map.png", map, bg = "white", width = 10, height = 5, dpi = 250)
