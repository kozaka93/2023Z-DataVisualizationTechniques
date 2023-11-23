library(htmlwidgets)
library(ggmap)
library(tidyverse)
library(dplyr)
library(patchwork)
library(ggrepel)
library(plotly)

data <- read.csv("co2_emission.csv")


# pogrupowanie danych tak aby dla lat 2015 - 2018 zliczyc dla kazdego panstwa
# srednia ilosc CO2 emission per capita
data %>% 
  select(country, X2000: X2018) %>% 
  mutate(avg = rowMeans(select(., X2015:X2018))) %>% 
  select(country, avg) %>% 
  mutate(region = country)-> data1518

w1 <- map_data("world")


w1 <- w1 %>% 
  filter(region != "Antarctica")
# subset z krajami europy
europe <- subset(w1, region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                                   "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                   "Croatia", "Cyprus", "Czech Republic","Denmark","Estonia","Finland", 
                                   "France","Georgia", "Germany", "Greece","Hungary","Iceland", 
                                   "Ireland", "Italy", "Kosovo", "Latvia","Liechtenstein", 
                                   "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                   "Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                                   "San Marino","Serbia","Slovakia","Slovenia","Spain",
                                   "Sweden","Switzerland","Turkey", "Ukraine","UK","Vatican", 
                                   "North Macedonia", "Russia"))

combined_europe <- left_join(europe, data1518, by = "region")

# wykres dla Europy
ggplot(combined_europe, aes(x = long, y = lat, group = group,
                            fill = avg, text =country)) +
  geom_polygon(color = "black") +
  scale_fill_continuous(low = "#e1c9b9",
                        high = "#440d31",
                        guide = "colorbar") +
  labs(title = "Average CO2 emission per capita [tons]",
       subtitle = "Europe 2015-2018",
       fill = "Emission") +
  theme_void() +
  coord_fixed(ratio=1.4, xlim = c(-25, 45), ylim = c(35, 80)) ->eur
eur

ggplotly(eur)%>% layout(
  title = "Average CO2 emission per capita [tons]",
  annotations = list(
    text = "2015 - 2018",
    x = 0.5,   
    y = 1.02,  
    xref = "paper",
    yref = "paper",
    showarrow = FALSE)) -> eur_i


saveWidget(eur_i, file = "europe_interactive.html")