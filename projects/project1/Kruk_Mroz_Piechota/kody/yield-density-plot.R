library(dplyr)
library(ggplot2)
library(scales)
library(SmarterPoland)
library(ggridges)

crop_production <- read.csv("datasets/crop1.csv")

# -----------------------------------

countries <- countries %>% 
  select(country, continent) %>% 
  rename(Area = country)
options(scipen = 999)

production_density <- crop_production %>% 
  filter(Year %in% 2010:2020, Element == "Yield") %>% 
  mutate(Value = Value / 10000) %>% 
  group_by(Area) %>% 
  summarise(average_yield = mean(Value, na.rm = T)) %>% 
  inner_join(countries, by = "Area") 

# production_density %>% 
#   group_by(continent) %>% 
#   summarise(count = n())

p <- production_density %>% 
  ggplot(aes(x = average_yield, y = continent, height = ..density..)) +
  geom_density_ridges(fill = "#FFD29D", color = "white", stat = "density", trim = TRUE) +
  labs(x = "Average Yield (t/ha)", y = "Continent") +
  theme_ridges() +
  theme(
    panel.grid = element_line(linewidth = 0.3),
    plot.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major = element_line(color = "white"),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.y = element_line(color = "white"),
    axis.text.y.left = element_text(size = 17, color = "white"),
    axis.text.x.bottom = element_text(size = 17, color = "white"),
    axis.title.x = element_text(size = 20, color = "white"),
    axis.title.y = element_text(size = 20, color = "white")) +
  xlim(c(-3, 80))

production_density %>% 
   arrange(-average_yield) 
