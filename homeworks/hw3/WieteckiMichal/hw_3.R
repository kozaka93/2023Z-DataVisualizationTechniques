library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(scales)

setwd('~/Desktop/pw/3 sem/techniki_wizualizacji_danych/hw 3/')


guns <- read.csv("guns.csv")

guns %>%
  mutate(mass_kill = ifelse(n_killed >= 1, 1,0)) %>%
  group_by(state) %>%
  summarise(mass_killings = sum(mass_kill)) %>%
  print(n = 51) %>%
  transmute(state = tolower(state), mass_killings) -> guns1

guns1 <- as.data.frame(guns1)



# usa <- map_data("usa")
states <- map_data("state")

data_guns <- merge(states, guns1, by.x = "region", by.y = "state")

options(scipen = 999)

ggplot(data = data_guns) +
  geom_polygon(aes(x = long, y = lat, fill = mass_killings, group = group), color = "white") +
  coord_map("albers", 25, 50) +
  scale_fill_fermenter(palette = 8,trans = "log10", direction = 1, name = "Liczba") +
  labs(title = "Liczba strzelanin z przynajmniej jedną ofiarą śmiertelną w poszczególnych stanach",
       subtitle = "Lata 2013-2018") +
  theme_void() +
  theme(title = element_text(face = "bold")) -> abc

ggsave('hw_3.png',abc,units = "px", height=900,width=2000)

  



