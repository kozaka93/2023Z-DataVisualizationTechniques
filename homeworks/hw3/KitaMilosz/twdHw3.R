library(tidyverse)
library(maps)
library(mapdata)
library(rvest)

dane <- read.csv("C:/Users/milos/Downloads/History_of_Mass_Shootings_in_the_USA.csv")
stany <- map_data("state") %>% mutate(region = tolower(region)) %>% distinct(region)
stany2 <- map_data("state") %>% mutate(region = tolower(region))
populacja <- read_html("https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population") %>%
  html_table(fill = TRUE) %>% 
  as.data.frame() %>%
  select(State, population = Census.population..April.1..2010.1..2.) %>%
  mutate(State = tolower(State),population = as.integer(gsub(",","",populacja$population)))

dane <- dane %>% 
  mutate(Date = as.Date(Date, format = "%d-%m-%Y")) %>%
  filter(Date >= "2000-01-01" & Date <= "2023-01-01") %>%
  mutate(State = tolower(State)) %>%
  group_by(State) %>%
  summarize(n_shooting = n())

dane$State[dane$State == "rhode isl"] = "rhode island"
dane$State[dane$State == "maryl"] <- "maryland"

dane <- dane %>%
  right_join(stany,join_by(State == region)) %>%
  left_join(populacja,join_by(State))

dane$n_shooting[is.na(dane$n_shooting)] = 0
dane <- dane %>%
  mutate(per_mln = n_shooting / as.integer(population) * 1000000) %>%
  left_join(stany2,join_by(State == region))

mapka <- ggplot(dane,aes(x = long, y = lat, group = group, fill = per_mln)) +
  geom_polygon(color = "black") +
  coord_map("albers", 25, 50) +
  theme_minimal() +
  labs(title = "Mass shooting per 1M people in USA in 21 century",
       fill = "Mass shooting per 1M people") +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 20),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.title = element_text(size = 12),
        axis.text = element_blank()) +
  scale_fill_distiller(palette = 7,direction = 1)

ggsave("mapka.pdf", plot = mapka, width = 14, height = 8, units = "in")
