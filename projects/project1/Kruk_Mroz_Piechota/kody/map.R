# Install and load necessary libraries
install.packages("countrycode")
library(ggplot2)
library(countrycode)
library(tidyverse)

# Read data
# https://www.kaggle.com/datasets/imtkaggleteam/food-prices-in-world

unaffordable_diet <- read_csv("1- share-healthy-diet-unaffordable.csv")

# Filter and rename columns for unaffordable_diet_2021
unaffordable_diet_2021 <- unaffordable_diet %>% 
  filter(Year == "2021") %>% 
  rename(
    "Share" = `Share of the population who cannot afford a healthy diet`,
    "Country" = Entity
  )

# Create mapdata
mapdata <- map_data("world")
mapdata$CountryCode <- countrycode(
  sourcevar = mapdata$region,
  origin = "country.name",
  destination = "iso3c"
)

# Perform left join
mapdata <- left_join(
  mapdata,
  unaffordable_diet_2021,
  by = c("CountryCode" = "Code")
) %>% 
  filter(region != "Antarctica")


# Create map
map1 <- ggplot(mapdata, aes(x = long, y = lat, group = group, 
                            fill = Share, color = "No data")) +
  geom_polygon(linewidth = 0.1) + 
  theme_void() +
  theme(
    legend.position = c(0.18, 0.35),
    legend.text = element_text(colour = "white", size = 10),
    legend.title = element_text(colour = "white", size = 15),
    legend.key.size = unit(1.3, "lines")
  ) +
  scale_fill_stepsn(colours = c("#FFD29D", "#ECB785", "#D99B6C", "#C67F54", "#B3633B"), 
                    n.breaks=6, na.value = "grey") +
  scale_color_manual(values = NA) +
  guides(fill = guide_colourbar(title = " %", ticks = FALSE ),
         colour = guide_legend(title = "", override.aes = list(fill = "grey")))



print(map1)
ggsave("map.png", map1, bg = "transparent", width = 10, height = 5, dpi = 250)


