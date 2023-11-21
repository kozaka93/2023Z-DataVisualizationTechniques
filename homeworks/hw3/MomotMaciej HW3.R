dane <- read.csv("C:/Users/macie/Desktop/STUDIA/SEMESTR3/Techniki Wizualizacji Danych/Prace Domowe/Praca Domowa Mapa/CSGO_players_by_country.csv", sep = ";")
dane$Players <- as.numeric(dane$Players)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
world <- map_data("world")
                 
panstwa_europy <- c("Albania", "Andorra", "Austria", "Belarus", 
                   "Belgium", "Bosnia and Herzegovina", "Bulgaria", 
                   "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                   "Estonia", "Finland", "France", "Greece", "Spain", 
                   "Netherlands", "Ireland", "Kosovo", 
                   "Liechtenstein", "Lithuania", "Luxembourg", "Latvia", 
                   "North Macedonia", "Malta", "Moldova", "Monaco", 
                   "Montenegro", "Germany", "Norway", "Poland", "Portugal", 
                   "Romania", "San Marino", "Serbia", "Slovakia", 
                   "Slovenia", "Switzerland", "Sweden", "Ukraine", 
                   "Hungary", "UK", "Italy", "Russia")

data <- world %>% filter(region %in% panstwa_europy) %>% 
  left_join(dane, by = "region") %>% 
  mutate(percently = Players_Percent*100) %>% 
  mutate(przedzialy = case_when(Players < 300000 ~ "[0 - 0.3) mln",
                                Players < 700000 & Players >=300000 ~ "[0.3 - 0.7) mln",
                                Players < 1000000 & Players >=700000 ~ "[0.7 - 1.0) mln",
                                Players < 1300000 & Players >=1000000 ~ "[1.0 - 1.3) mln",
                                Players < 1700000 & Players >=1300000 ~ "[1.3 - 1.7) mln",
                                Players < 2100000 & Players >=1700000 ~ "[1.7 - 2.1) mln",
                                Players >=2100000 & Players < 4000000 ~ "[2.1 - 4.0) mln",
                                )) %>%  
  select(-Players_Percent)

#45.31763 148.59950
point <- data.frame(
  long = c(42),
  lat = c(58),
  names = c("~3.8 mln"),
  stringsAsFactors = FALSE
) 



colors <- colorRampPalette(c("#660000", "yellow"))(length(unique(data$przedzialy)))
plot <- data %>% ggplot() +
  geom_polygon(aes(x=long, y=lat, group = group, fill = reorder(przedzialy, -as.numeric(gsub("\\D", "", przedzialy)))), color = "black") +
  geom_text(data = point, aes(x = long, y = lat, label = names), color = "black", size = 7) + 
  coord_map("mollweide",xlim=c(-20, 41.0), ylim = c(35, 70)) + theme_bw() +
  scale_fill_manual(values = setNames(colors, levels(data$przedzialy))) +
  labs(x = "Długość geograficzna", y = "Szerokość geograficzna", title = "Liczba graczy Counter Strike Global Offensive w Europie", subtitle = "Stan na 2017 rok", fill = "Liczba graczy")

####################



