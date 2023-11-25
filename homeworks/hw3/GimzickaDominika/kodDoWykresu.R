#załadowanie odpowiednich bibliotek
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

#dane są ze strony https://www.kaggle.com/datasets/tanuprabhu/list-of-countries-by-number-of-internet-users
data <- read.csv('C:/Users/domin/Documents/Studia/TWD/hw3/List of Countries by number of Internet Users - Sheet1.csv')

#przygotowanie danych
colnames(data)[1] <- "Country"
data$Percentage <-  as.numeric(gsub("%", "", data$Percentage))

w1 <- map_data("world")

df <- w1 %>%
  filter(region != "Antarctica") %>%  #pomijamy Antartykę ponieważ w naszych danych nie ma jej uwzględnionej i zaburzałaby wygląd mapy
  left_join(data, by = c("region" = "Country"))

#rysowanie mapy
ggplot(data = df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Percentage), color = "white") +
  scale_fill_gradient(low = "lightblue", 
                      high = "darkblue", 
                      name = "Percentage [%]", 
                      na.value = "lightgray") +
  theme_minimal() +
  labs(title = "Percentage of internet users in each country",
       caption = "(The gray color indicates missing information)") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.caption = element_text(hjust = 0.5, vjust = 2.5, size = 10)
  ) +
  coord_fixed(ratio = 1.2, xlim = c(-165, 180), ylim = c(-55, 85))
  