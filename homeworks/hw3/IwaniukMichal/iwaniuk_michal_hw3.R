library(sf)
library(spData)
library(tidyverse)

world_sf <- spData::world
df <- read_csv("beer-consumption-per-person.csv")
df <- df %>% filter(Year==2018) %>% rename("Litres beer" = `Indicator:Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol) - Beverage Types:Beer`)

df <-df %>% mutate(Entity = case_when(
  Entity == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
  Entity == "Russia" ~ "Russian Federation",
  Entity == "East Timor" ~ "Timor-Leste",
  Entity == "Cote d'Ivoire" ~ "Côte d'Ivoire",
  Entity == "Congo" ~ "Republic of the Congo",
  Entity == "Eswatini" ~ "eSwatini",
  Entity == "Gambia" ~ "The Gambia",
  Entity == "Laos" ~ "Lao PDR",
  Entity == "North Korea" ~ "Dem. Rep. Korea",
  Entity == "South Korea" ~ "Republic of Korea",
  Entity == "Brunei" ~ "Brunei Darussalam",
  Entity == "Czechia" ~ "Czech Republic",
  Entity == "North Macedonia" ~ "Macedonia",
  .default = Entity
))

world_beer <- left_join(world_sf,df,by=join_by("name_long"=="Entity")) %>%
  filter(!name_long=="Antarctica") 

p <- RColorBrewer::brewer.pal(n = 9, name = "YlGnBu")

ggplot(world_beer) + geom_sf(aes(fill=`Litres beer`))+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom")+
  scale_fill_stepsn(colors = p, breaks = c(0,0.2,0.5,seq(1, 7, 1)),na.value = "grey")+
  labs(fill=
         "Litres of pure alcohol",
       title = "Beer consumption per person, 2018",
       subtitle = "Average annual per capita beer consumption, measured in liters of pure alcohol.
Beer contains around 5% of pure alcohol per volume so that one liter of beer contains 0.05 liters of pure alcohol. 
This means that 5 liters of pure alcohol equals 100 liters of beer.")+
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size=8),
    legend.key.width = unit(2,"cm"),
    legend.title = element_text(vjust=0.8,size=9)
  )


