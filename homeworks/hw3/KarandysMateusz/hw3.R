library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

states <- map_data("state")
counties <- map_data("county")
area <- data.frame("region" = tolower(state.name), "area" = state.area)

counties %>% 
  group_by(region) %>% 
  summarize(counties_number = n_distinct(group)) %>% 
  right_join(states, by = "region") %>% 
  right_join(area, by = "region") %>% 
ggplot(aes(x = long, y = lat, group = group, fill = counties_number/area)) +
  geom_polygon(color = "gray") +
  labs(title = "Density of counties in states [1/sq mi]",
       fill = "Density") +
  coord_fixed(1.3) +
  coord_map("albers", 25, 50) +
  scale_fill_fermenter(palette = 8, direction = 1) +
  theme_minimal()
