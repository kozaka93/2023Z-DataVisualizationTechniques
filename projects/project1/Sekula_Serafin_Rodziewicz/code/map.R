### foodwaste map

### libraries

library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(viridis)

### data frames

df_map <- read.csv("data_map.csv")
df_map_locations <- map_data('world')

### prepare data frames for plot

df_map <- df_map %>% 
  mutate(Country = ifelse(Country=="United States", "USA", Country))

df_map_locations <- filter(df_map_locations, lat > -60)

prepared <- right_join(x = df_map, y = df_map_locations, by = join_by(Country == region)) 


### plot

map_plot <- ggplot(prepared, aes(y = lat, x = long)) +
  geom_polygon(aes(group = group, fill = Food_waste, color = "No data"), size = 0.1) +
  scale_fill_gradient2(
    low = "springgreen4", high = "tomato3", mid = "gold1",
    limits = c(34, 164), midpoint = 81, 
    na.value = "grey60",
    guide = guide_legend(na.value = "grey60",
                         keyheight = unit(6, units = "mm"), 
                         keywidth=unit(6, units = "mm"),
                         label.position = "right",
                         title.position = 'top',
                         nrow=6
                         )
  ) + 
  scale_color_manual(values="black") +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.position = c(0.15, 0.3),
    legend.title = element_text(color = "#4e4d47", size = 10),
    legend.text = element_text(color = "#4e4d47", face = "bold", size = 8),
    axis.title = element_blank(), 
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  guides(color = guide_legend(title = "",
                              override.aes = list(fill="grey60"),
                              keyheight = unit(6, units = "mm"), 
                              keywidth=unit(6, units = "mm")))+
  labs(fill = "Food waste\nper capita in kg") 

map_plot

ggsave("mapka1.png", plot = map_plot, bg = "transparent",
       width = 10, height = 5, dpi = 250)

