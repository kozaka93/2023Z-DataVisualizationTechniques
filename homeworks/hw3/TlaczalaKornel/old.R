library(dplyr)
library(ggplot2)
library(mapdata)
library(countrycode)
library(worldfootballR)
library(sf)
library(leaflet)


# Get data about premier league players and their nationalities
team_urls <- tm_league_team_urls("England", 2023)
epl_players_2023 <- tm_squad_stats(team_urls)

# summarise by countries
players <- epl_players_2023 %>%
  mutate(nationality = case_when(
    nationality == "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
    nationality %in% c("England", "Scotland", "Wales", "Northern Ireland") ~ "UK",
    TRUE ~ nationality)) %>%
  group_by(nationality) %>%
  summarise(player_count = n(), playtime = sum(minutes_played)) %>%
  arrange(-player_count)

# check number of European countries that have players in the league
# check <- players %>% 
#   mutate(continent = countrycode(sourcevar = nationality,
#                                  origin = "country.name",
#                                  destination = "continent")) %>% 
#   mutate(continent = case_when(
#     is.na(continent) ~ "Europe",
#     nationality == "Kosovo" ~ "Europe",
#     nationality == "Turkey" ~ "Europe",
#     nationality == "Cyprus" ~ "Europe",
#     TRUE ~ continent)) %>% 
#   filter(continent == "Europe")


# load a map of the world and add continent information
world <- map_data("world") %>% 
  mutate(continent = countrycode(sourcevar = region,
                                 origin = "country.name",
                                 destination = "continent"))
  
world <- world %>% 
  mutate(continent = case_when(
    region == "Kosovo" ~ "Europe",
    region == "Turkey" ~ "Europe",
    region == "Cyprus" ~ "Europe",
    TRUE ~ continent))

# filter for only European countries
europe <- world %>% 
  filter(continent == "Europe")

# join map with player data
europe <- europe %>% 
  left_join(players, by = c("region" = "nationality"))
  
# check number of european countries that have their players in the league
# europe <- europe %>% 
#   group_by(region) %>%
#   summarise(player_count = mean(player_count)) %>%
#   na.omit()

# plot everything on the map
ggplot(europe, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "#444444", color = "#CCCCCC") +
  # coord_fixed(xlim = c(-15,80), ylim = c(32, 80))
  coord_map("mollweide", xlim = c(-10, 40), ylim = c(35, 71))
    
# mapRegions = map("italy", fill = TRUE, plot = FALSE)
# class(mapRegions)



