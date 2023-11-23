library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(showtext)
library(extrafont)

font_add("Bakso Sapi", "/Users/Karolina/Library/Fonts/BaksoSapi-4BmlB.ttf")
alcohol_cons <- read.csv('alcohol-consumption-by-country-2023.csv')


w <- map_data("world")

world_map <- ggplot() + 
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "gray")+
  theme_void()


alcohol_cons <- alcohol_cons %>% 
  filter(region == "Europe") %>% 
  mutate(country = ifelse(country == "United Kingdom", "UK", country))


alcohol_joined <- full_join(alcohol_cons, w, by = c("country" = "region")) %>% 
  filter(!is.na(alcoholConsumptionByCountry_both) | country %in% 
           (c("Kosovo", "Czech Republic")))

alcohol_map <- ggplot(data = alcohol_joined, mapping = aes(x = long, y = lat, group = group,
                                              fill = alcoholConsumptionByCountry_both)) + 
  coord_map("mollweide", xlim = c(-10, 34), ylim = c(35, 70)) +  
  geom_polygon(color = "black") +
  labs(title = "Alcohol consumption in Europe",
       subtitle = "Year 2023",
       fill = "Consumption in\nliters per capita") +
  scale_fill_gradient2(low = "pink",
                       mid = "orange",
                       high = "navy",
                       midpoint = 10) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "Bakso Sapi"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, family = "Bakso Sapi"),
    legend.position = "left",
    legend.title = element_text(family = "Bakso Sapi"),
    legend.text = element_text(family = "Bakso Sapi")
  )


alcohol_map

