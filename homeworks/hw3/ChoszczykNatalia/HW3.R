library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

ikea_stores <- read.csv("ikea_stores.csv", sep = ";")
world <- map_data("world")

ikea_stores_europe <- ikea_stores %>% 
  filter(continent == "Europe")

ikea_stores_world <- world %>% 
  full_join(ikea_stores_europe, by = c("region" = "country")) %>% 
  filter(!is.na(nr))

ikea_stores_map <- ggplot(data = ikea_stores_world, 
                          mapping = aes(x = long, y = lat, group = group, fill = nr)) +
  geom_polygon(color = "black") +
  coord_map("mollweide", xlim = c(-10, 32), ylim = c(35, 70)) +
  theme_void() +
  theme(legend.position = "left",
        plot.title = element_text(face = "bold", size = 16))+
  labs(title = "Map of IKEA stores in Europe") +
  scale_fill_fermenter(palette = 8, trans="log10", direction = 1)

ikea_stores_map
