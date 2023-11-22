#zaladowanie potrzebnych bibliotek i przygotowanie danych
library(dplyr)
library(ggplot2)

#dane które tu wykorzystuje są pobrane ze strony podanej  w raporcie 
df <- read.csv('C:/Users/domin/Documents/Studia/TWD/hw2/cities_air_quality_water_pollution.18-10-2021 (1).csv')

df2 <- df %>% 
  group_by(AirQuality) %>% 
  summarise(WaterPollution = mean(WaterPollution))

#tworzenie wykresu
ggplot(df2, aes(x = AirQuality, y = WaterPollution)) +
  geom_point(color = "#1f77b4", size = 3, alpha = 0.7) +
  geom_smooth(color = "#ff7f0e", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "The relationship between water pollution and air quality",
    x = "Air Quality",
    y = "Water Pollution") +
  scale_x_continuous(breaks = seq(0,100,20))+
  scale_y_continuous(breaks = seq(0,100,20))+
  theme(plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))
