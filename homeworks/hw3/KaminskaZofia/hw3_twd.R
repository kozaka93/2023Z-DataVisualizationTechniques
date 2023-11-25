#PRACA DOMOWA 3 Crime indexes in Europe



#install.packages("mapdata")
#install.packages("dplyr")
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

#ŹRÓDŁO: worldpulationreview.com

crime_data <- read.csv("crime-rate-by-country-2023.csv")
crime_data <- mutate(crime_data, country = ifelse(country == "United Kingdom", "UK", country))
colnames(crime_data)
europe_data <- crime_data %>% filter(region == "Europe")
w1 <- map_data("world")

europe <- w1 %>% 
  full_join(europe_data, by = c("region" = "country"))

europe_filtered <- w1 %>% 
  full_join(europe_data, by = c("region" = "country")) %>% 
  filter(!is.na(crimeRateByCountry_crimeIndex) | region %in% (c("Kosovo", "Bosnia and Herzegovina", "United Kingdom", "UK")))
  

europe_crime <- ggplot(data = europe_filtered, mapping = aes(x = long, y = lat, group = group, 
                                                         fill = crimeRateByCountry_crimeIndex))+
  coord_map("mollweide", xlim = c(-10, 32), ylim = c(35, 70)) +
  #coord_fixed(xlim = c(-10, 32), ylim = c(35, 70), ratio = 1.4) +
  geom_polygon(color = "black") +
  theme_void()+
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12))+
  labs(title = "Crime rates in Europe",
       subtitle = "Year 2023",
       fill = "number of crimes per 100,000 inhabitants",)+
  scale_fill_distiller(palette =8,
                       direction =1)

europe_crime

ggsave('plot11.png', bg='lightgrey', units = "px",height=2000,width=3000, europe_crime)
#tło ustawione na jasnoszare żeby się kolor szwajcarii nie wydawał taki sam jak kolor tła bo jest bardzo jasny,
#mogłabym to zmienić uzywając innej palety kolorów też ale ta mi się podoba bo do crimów pasuje 

#pozdrawiam, życzę miłego dnia :)
