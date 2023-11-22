library(dplyr)
library(ggplot2)
library(stringr)
library(maps)
#https://www.who.int/data/gho/data/indicators/indicator-details/GHO/15-years-old-first-drink-at-13-years-or-younger-(-)

first1 <- read.csv('15y0before13.csv')
world <- map_data("world")

europe <- subset(world, region %in% c("Albania", "Andorra", "Austria", 
                                      "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                      "Croatia", "Czech Republic","Denmark","Estonia","Finland", 
                                      "France", "Germany", "Greece","Hungary","Iceland", 
                                      "Ireland", "Italy", "Kosovo", "Latvia","Liechtenstein", 
                                      "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                      "North Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                                      "San Marino","Serbia","Slovakia","Slovenia","Spain",
                                      "Sweden","Switzerland","Ukraine","UK","Vatican"))


first <- first1[c(8,10,13,24)] %>%
  mutate(Location = recode(str_trim(Location),
                           "Czechia" = "Czech Republic",
                           "Republic of Moldova" = "Moldova",
                           "Macedonia"="North Macedonia", 
                           "United Kingdom of Great Britain and Northern Ireland" = "UK"))  %>%
  filter(Dim1=='Both sexes', Period=='2014') %>% 
  select(Location,FactValueNumeric)



data <- left_join(europe, first, join_by('region'=='Location'))


first_drink_map2 <- ggplot(data = data, aes(x = long, y = lat, group = group, colour="")) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = FactValueNumeric)) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "lightgrey") +
  labs(title = "Percent of people who began drinking before age 13", fill = "Percent") +
  geom_path(data = europe, aes(x = long, y = lat, group = group), color = "black", size = 0.2) +
  theme(
    legend.margin = margin(r = -100),
    legend.position = "left",
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA_character_),
    plot.background = element_rect(fill = "transparent", colour = NA_character_),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    legend.text = element_text(family = "Georgia", color = "white", size = 13),
    legend.title = element_text(family = "Georgia", color = "white", size = 13),
    plot.title = element_blank(),
  ) +
  scale_y_continuous(limits = c(30, 74)) +
  scale_colour_manual(values=NA) +              
  guides(colour=guide_legend("No data", override.aes=list(fill="lightgrey", colour = "transparent")))

first_drink_map2


ggsave(
  plot = first_drink_map2,
  filename = "first_drink_map_plakat_orange_bold_boarders_lightgrey.png",
  bg = "transparent"
)

