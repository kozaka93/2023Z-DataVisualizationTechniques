library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(scales)

#sources
# https://data.texas.gov/dataset/CPI-1-1-Texas-Child-Population-ages-0-17-by-County/x5xb-idr6
# Load data
setwd("/Users/fantasy2fry/Documents/informatyczne/iadstudia/twd/2023Z-DataVisualizationTechniques/homeworks/hw3/FrydrysiakNorbert")

read.csv("datatexas.csv")->texas

texas=texas %>% group_by(County) %>% summarise(children=mean(
  X..of.Children.in.Total.Population))%>% select(
    County, children)

texas_map_data=map_data("county", "texas")
#to lowercases
texas$County <- tolower(texas$County)
#change dewitt to de witt in texas
texas$County[which(texas$County == "dewitt")] <- "de witt"

#left join
texas_map_data <- left_join(texas_map_data, texas, by = c("subregion" = "County"))
#rename column 7th
texas_map_data$children <- as.numeric(texas_map_data$children/100)


map=ggplot() + 
  geom_polygon(data=texas_map_data, aes(x=long, y=lat, group=group, fill=children), color="black") +
  coord_map("albers", 25, 50) +
  scale_fill_gradient(low="tomato1", high="lightgreen", labels=percent, na.value = "mediumpurple2") +
  labs(title="Średni procent dzieci w poszczególnych hrabstwach Teksasu",
       fill="", subtitle = "Dane z lat 2013-2022",
       caption = "Źródło: data.texas.gov") +
  theme(plot.title = element_text(hjust = 0.5))+theme_void()

map

ggsave("mapa.png", width = 20, height = 20, units = "cm",bg="white")
