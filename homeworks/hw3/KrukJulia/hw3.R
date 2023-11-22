# loading libraries
library(ggplot2)
library(tidyverse)
library(countrycode)
library(showtext)

# adding fonts
font_add_google("Special Elite", family = "special")
font_add("roboto", "Roboto/Roboto-Regular.ttf")
showtext_auto()

# colour palette
my_palette <- c("#ef798a", "#d14081", "#5b2a86", "#360568")

# reading the data
# https://ourworldindata.org/grapher/distribution-of-women-share-in-parliament?tab=table
df <- read_csv("distribution-of-women-share-in-parliament.csv") 
world_map <- map_data("world")

# filtering data from 2022
df <- df %>% 
  filter(Year == 2022)


world_map$CountryCode <- countrycode(
  sourcevar = world_map$region,
  origin = "country.name",
  destination = "iso3c"
)

# joining with world map 
mapdata <- world_map %>% 
  left_join(df, by = c("CountryCode" = "Code"), relationship = "many-to-many") %>% 
  filter(CountryCode != "ATA")

# creating the map
map1 <- ggplot(mapdata, aes(x = long, y = lat, group = group, fill = wom_parl_vdem_owid, colour = "No data")) +
  geom_polygon(linewidth = 0) + 
  theme_void() +
  
  # title and subtitle
  labs(title = "Women share in parliament, 2022",
       subtitle = "Source: ourworldindata.org") +
  
  # theme settings
  theme(
    plot.title = element_text(colour = "black", size = 50, family = "roboto"),  # Zmniejszono rozmiar
    plot.subtitle = element_text(colour = "black", size = 30, family = "roboto"),  # Zmniejszono rozmiar
    legend.position = c(0.18, 0.35),
    legend.text = element_text(colour = "black", size = 30, family = "roboto"),  # Zmniejszono rozmiar
    legend.title = element_text(colour = "black", size = 40, family = "roboto"),  # Zmniejszono rozmiar
    legend.key.size = unit(1.3, "lines"),
    plot.background = element_rect(fill = "white", colour = NA)
  ) + 
  
  # colours
  scale_fill_stepsn(
    colors = my_palette, 
    n.breaks = 5,
    na.value = "grey"
  ) +
  scale_colour_manual(values = NA) +
  
  # legend settings
  guides(
    fill = guide_colourbar(
      title = " %",
      ticks = FALSE
    ),
    colour = guide_legend(title = "", override.aes = list(fill = "grey")),
    guide = guide_legend(
      keyheight = unit(3, units = "mm"), 
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = 'top', nrow = 1
    )
  )

# printing map
print(map1)

# saving as png
ggsave("map.png", map1, width = 10, height = 5, dpi = 250)
