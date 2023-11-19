library(ggplot2)
library(dplyr)
library(sf)
library(spData)
library(tidyr)

alcohol_raw <- read.csv("data.csv", sep = ";")

world_sf <- st_as_sf(spData::world)
world_robinson <- st_transform(world_sf, crs = "+proj=robin")

alcohol <- alcohol_raw %>%
  select(Countries, Type, X2019, X2018, X2017) %>%
  mutate(Type = trimws(Type)) %>%
  pivot_longer(
    cols = c("X2019", "X2018", "X2017"),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = gsub("X", "", Year)) %>% 
  pivot_wider(names_from = Type, values_from = Value, values_fill = 0) %>%
  filter(Year == 2019) %>%
  right_join(world_robinson, by = c("Countries" = "name_long"))

map <- ggplot(data = alcohol) +
  geom_sf(aes(fill = Wine, geometry = geom), color = "black", size = 0.1) +
  scale_fill_gradient(
    low = "#f7e9ff",
    high = "purple",
    na.value = "grey50") +
  labs(
    title = "World Wine Consumption",
    subtitle = "Average per capita consumption of wine in 2019",
    fill = "Wine consumption per capita (litres)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

ggsave("map.jpg", map, width = 10, height = 6, dpi = 300)
