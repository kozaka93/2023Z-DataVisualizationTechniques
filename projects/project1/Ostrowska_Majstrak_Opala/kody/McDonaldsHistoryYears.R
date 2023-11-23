setwd("C:/Users/User/Desktop/IAD/TWD/PrototypMCD")

opened <- read.csv("opened.csv")
closed <- read.csv("closed.csv")

library(maps)
install.packages("worldmap")
library(worldmap)
library(dplyr)

opened_filtered <- opened %>% 
  rename(date = Date.of.first.store) %>% 
  rename(country = Country.territory) %>% 
  mutate(date = ifelse(country=="Germany", 1971, date)) %>% 
  mutate(date = ifelse(country=="United States", 1940, date)) %>% 
  mutate(date = ifelse(country=="United Kingdom", 1974, date)) %>% 
  mutate(date = sub(".*, (\\d{4})", "\\1", date)) 

closed_filtered <- closed %>% 
  rename(date = Date.of.first.store) %>% 
  rename(country = Country.territory) %>% 
  rename(date_closed = Date.of.closure) %>% 
  mutate(date = sub(".*, (\\d{4})", "\\1", date)) %>% 
  mutate(date_closed = sub(".*, (\\d{4})", "\\1", date_closed)) 

data <- bind_rows(opened_filtered, closed_filtered)  

data2 <- data %>% 
  mutate(is_closed = ifelse(is.na(date_closed)==TRUE, FALSE, TRUE)) %>% 
  rename(region = country) %>% 
  select(region, date, is_closed) %>% 
  mutate(region = ifelse(region=="United States", "USA", region)) %>% 
  mutate(region = ifelse(region=="Canada (details)", "Canada", region)) %>% 
  mutate(region = ifelse(region=="France (details)", "France", region)) %>% 
  mutate(region = ifelse(region=="New Zealand (details)", "New Zealand", region)) %>% 
  mutate(region = ifelse(region=="Philippines (details)", "Philippines", region)) %>% 
  mutate(region = ifelse(region=="Israel (details)", "Israel", region)) %>% 
  mutate(region = ifelse(region=="Hungary (Hungarian People's Republic at the time)", "Hungary", region)) %>% 
  mutate(region = ifelse(region=="Serbia (part of Yugoslavia at the time)", "Serbia", region)) %>% 
  mutate(region = ifelse(region=="Czech Republic (part of Czechoslovakia at the time)", "Czech Republic", region)) %>% 
  mutate(region = ifelse(region=="Montenegro (part of Serbia and Montenegro at the time)", "Montenegro", region)) %>% 
  mutate(region = ifelse(region=="North Macedonia (named Republic of Macedonia at the time)", "North Macedonia", region)) %>%
  mutate(region = ifelse(region=="Russia (details) (part of Soviet Union at the time)", "Russia", region))



world_map <- map_data("world") %>% filter(region != "Antarctica") 

mapdata <- left_join(world_map, data2, by = "region")

mapdata <- mapdata %>% 
  mutate(date = as.numeric(date))

closed_lat <- mapdata %>% 
  filter(is_closed == T) %>% 
  filter(region != "San Marino" & region != "Montenegro" & region != "North Macedonia") %>% 
  group_by(region) %>% 
  summarise(mean_lat = mean(lat)) %>% 
  arrange(region)

closed_long <- mapdata %>% 
  filter(is_closed == T) %>% 
  filter(region != "San Marino" & region != "Montenegro" & region != "North Macedonia") %>% 
  group_by(region) %>% 
  summarise(mean_long = mean(long)) %>% 
  arrange(region)

# usuwanie Czarnogóry i Północnej Macedonii żeby była Jugosławia jako jeden punkt
closed_years <- closed_filtered %>% 
  rename(region = country) %>% 
  filter(region != "San Marino" & region != "Montenegro (part of Serbia and Montenegro at the time)" 
         & region != "North Macedonia (named Republic of Macedonia at the time)" 
         & region != "Bermuda (territory of United Kingdom)") %>% 
  arrange(region)

points_where_closed <- data.frame(
  long = closed_long$mean_long,
  lat = closed_lat$mean_lat,
  names = closed_years$date_closed,
  names_countries = closed_long$region,
  names_letters = LETTERS[1:9],
  stringsAsFactors = FALSE
)

legend_data <- data.frame(
  label = paste(points_where_closed$names_letters, " - ", points_where_closed$names_countries),
  x = rep(1, length(points_where_closed$names_letters)),
  y = seq(1, 60, length.out = length(points_where_closed$names_letters))
)

legend_data$label <- rev(legend_data$label)

legend_data$label

legend_text <- paste(legend_data$label)

gp <- ggplot(data = mapdata, aes(x = long, y = lat, group = group, fill = date)) +
  geom_polygon() +
  scale_fill_gradient(low = "orange",  high = "lightyellow", guide = guide_legend(
    title.theme = element_text(
      size = 15,
      face = "bold",
      colour = "white",
      angle = 0
    ),
    #title = "Year when the first \n McDonalds was opened",
    title = "",
    label.theme = element_text(colour = "white")
  )) +
  theme_void() 

p1 <- gp + 
  geom_point(data = points_where_closed, aes(x = long, y = lat), inherit.aes = FALSE, color = "white", size = 6, shape = 19) +
  geom_point(data = points_where_closed, aes(x = long, y = lat), inherit.aes = FALSE, color = "#2bbb76", size = 5, shape = 19) +
  geom_text(data = points_where_closed, aes(x = long, y = lat, label = names_letters), inherit.aes = FALSE, color = "white", size = 3) +
  coord_fixed(1.3)


ggsave("restaurants_opened.png", plot = p1, bg = "transparent")
