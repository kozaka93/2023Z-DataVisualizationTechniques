#MAPA - przypadki smierci spowodowanych przez raka w stanach
setwd("C://Users//basiu//OneDrive//Pulpit//sem 3//techniki wizualizacji danych//hw3")
library(ggplot2)
library(stringr)
library(maps)
library(mapdata)
library(dplyr)


#ramka z liczba smierci spowodowanych przez (dowolnego) raka na 100 000 osob z podzialem na stany
read.csv("uscs_map_death_all.csv") -> df
View(df)

#kody stanow (potrzebne do zmergowania ramek)
#https://data.world/adamhelsinger/cancer-rates-by-u-s-state
read.csv("table-data.csv") ->state_code

#ramka koordynatow w usa z nazwami stanow
states <- map_data("state")


#zmergowanie wszystkich ramek
states %>%
  mutate(state = str_to_title(region)) -> states
#merge(states, state_code, by = "state") ->states_merged
inner_join(states, state_code, by = "state") -> states_merged
ggplot(states) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white")
colnames(df)[1] <-"code"
merge(states_merged, df,by = "code", all.x = T) -> a

#mapa (odwzrorowanie albersa bo takie jest oficjalne w USA)
a %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = Range, group = group), color = "white") +
  coord_map("albers", 25, 50) + 
  scale_fill_manual(values = c("#a1dab4", "#7bccc4","#2b8cbe","#245ea8")) ->plot

#dodanie najwyzszego i najnizszego wyniku
a %>%
  arrange(-Rate) %>%
  group_by(code) %>% head(1) -> highest
a %>%
  arrange(Rate) %>%
  group_by(code) %>%
  head(1) -> lowest

highest_state <- highest$code
lowest_state <- lowest$code
a %>%
  filter(code == highest_state) %>%
  summarise(long = mean(long), lat = mean(lat)) ->highest_mean
a %>%
  filter(code == lowest_state) %>%
  summarise(long = mean(long), lat = mean(lat)) ->lowest_mean

points <- data.frame(
  long = c(lowest_mean$long, highest_mean$long),
  lat = c(lowest_mean$lat, highest_mean$lat),
  names = c("Lowest", "Higest"),
  stringsAsFactors = FALSE
) 

plot + 
  geom_point(data = points, aes(x = long, y = lat), color = c("yellow","red"), size = 5) +
  geom_label(data = points, aes(x = long, y = lat+1.4, label = paste(c(lowest$code, highest$code), c(lowest$Rate,highest$Rate))), color = "black", size = 2.5) ->plot1


plot1 +
  labs(title = "Deaths from cancer per 100 000 people in the US ", subtitle = "In 2013") + 
  theme(plot.background = element_rect(fill = "beige")) +
  theme(panel.background = element_rect(fill = "beige"))+
  theme(legend.background = element_rect(fill = "beige"))+
  theme(panel.grid.major=element_line(colour="grey")) -> plot2

us_coord <- readxl::read_xlsx("states_coordinates.xlsx")
colnames(us_coord)[1] <- "state"

merge(us_coord,state_code, by = "state") %>%
  select(Longitude, Latitude, code) %>%
  mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude)) %>%
  filter(code != "AK") %>% filter(code != "HI")%>%
  filter(code != higest_state) %>%filter(code != lowest_state)-> state_labels


plot2 +
  geom_text(data = state_labels, aes(x = Longitude, y = Latitude, label = code), size = 2)

