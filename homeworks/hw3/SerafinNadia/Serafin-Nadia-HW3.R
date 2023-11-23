# https://ourworldindata.org/working-hours

# packages
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)

# data
data <- read.csv("labor-productivity-per-hour-pennworldtable.csv")
world <- map_data("world")

data <- data %>% 
  mutate(Entity = ifelse(Entity == "Czechia", "Czech Republic", Entity),
         Entity = ifelse(Entity == "United Kingdom", "UK", Entity))

europa_kraje <- c("Albania", "Andorra", "Austria", "Belarus", 
                  "Belgium", "Bosnia and Herzegovina", "Bulgaria", 
                  "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                  "Estonia", "Finland", "France", "Germany", "Greece", 
                  "Hungary", "Netherlands", "Ireland", "Italy", "Kosovo", 
                  "Liechtenstein", "Lithuania", "Luxembourg", "Latvia", 
                  "North Macedonia", "Malta", "Moldova", "Monaco", 
                  "Montenegro", "Norway", "Poland", "Portugal", 
                  "Romania", "Russia", "San Marino", "Serbia", "Slovakia", 
                  "Slovenia", "Spain", "Switzerland", "Sweden", "Ukraine",
                  "UK")

eu_map <- map_data("world", region = europa_kraje) %>% 
  filter(lat < 70)

dt <- data %>% 
  filter(Entity %in% europa_kraje) %>% 
  filter(Year == 2019) %>% 
  full_join(eu_map, by = c("Entity" = "region")) %>% 
  rename(region = Entity)

# map
m <- ggplot() +
  geom_polygon(data = dt, aes(x = long, y = lat, fill = Productivity..output.per.hour.worked, group = group, colour = ""), linewidth = 0.1) +
  coord_map("mollweide", xlim = c(-10, 41.0), ylim = c(35, 70)) +
  theme_void() +
  theme(legend.position = c(0.25, 0.85),
        plot.title = element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(colour = "black", size = 11),
        plot.background = element_rect(fill="white", colour = NA)
  ) +
  labs(title = "Productivity for countries in Europe in 2019 [product GDP per hour]",
       subtitle = "Source: ourworldindata.org",
       fill = " product GDP per hour") +
  scale_fill_distiller(palette = 7,
                       direction = 5,
                       na.value = "grey") +
  scale_color_manual(values = NA) +
  guides(colour = guide_legend("No data", override.aes = list(fill = "grey")),
         fill = guide_colorbar(barwidth = 1, barheight = 5, nbin = 25, title.position = "top"))
m

ggsave("map.png", plot = m, width = 12, height = 6, units = "in")


  

