library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)

#dane z https://www.kaggle.com/datasets/dianapratiwi/cancer-rate-by-countries
#bazowane na 2018 GLOBOCAN statistics, włączając wszystkie typy raka
cancer_rate <- read.csv('Cancer_rate_by_countries.csv')
countries <- map_data("world") %>% 
  mutate(Country=region)

map_data <- left_join(countries, cancer_rate, by = "Country")

size1 <- cancer_rate %>% 
  group_by(Country) %>% 
  summarize(count=n())

size2 <- map_data %>% 
  filter(!is.na(Cancer_Rate)) %>% 
  group_by(Country) %>% 
  summarize(count=n())

nrow(size1)-nrow(size2)

#jest 8 krajow, ktore z tych ramkach danych maja inna nazwe, naprawmy to

size1 %>% 
  filter(!(Country %in% size2$Country))

#w ramce countries Francja i USA nie sa podzielone na kawalki (a w ,,cancer_rate" mamy dane dla tych fragmentow nalezacych do krajow), wiec to zostawmy
#zmienmy nazwy usa i uk tak, zeby w obu ramkach byly takie same

cancer_rate <- cancer_rate %>% 
  mutate(Country = case_when(
    Country == 'United States' ~ 'USA',
    Country == 'United Kingdom' ~ 'UK',
    TRUE ~ Country ))

#teraz polaczmy te ramki jeszcze raz - dla uk i usa powinno juz potem dzialac

map_data <- left_join(countries, cancer_rate, by = "Country")

#zrobmy mapke pokazujaca cancer rate na swiecie z pdozialem na kraje

map_data %>% 
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=Cancer_Rate), color="grey", size= 0.5)+
  theme_void()+
  labs(title = "The cost of a meal at McDonald's compared to an inexpensive restaurant")+
  theme( plot.title.position = "plot",plot.title = element_text(color = "black", size=15), legend.title = element_blank(), legend.text = element_text(color = "black")) 

#dane dla malej ilosci krajow, moze zblizmy na Europe

europe_data <- map_data %>%
  filter(long >= -25 & long <= 45 & lat >= 35 & lat <= 70)
left_join(europe_data, map_data)

map <- europe_data %>% 
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=Cancer_Rate), color="grey", size= 0.5)+
  theme_void()+
  labs(title = "The number of new cancer cases per 100,000 population in 2018",
       fill = "cancer rate" )+
  theme( plot.title.position = "plot",plot.title = element_text(color = "black", size=16), legend.title = element_text(color = "black", size=10), legend.text = element_text(color = "black")) +
  scale_fill_gradient(low = "#FFB0B0", high = "darkred")+
  coord_map() 

#zapis wykresu pod nazwa ,mapa.png', na bialym tle
ggsave('mapa.png', plot=map, width = 9, height = 6, units = "in", bg='white')
