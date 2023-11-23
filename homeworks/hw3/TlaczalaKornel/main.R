library(leaflet)
library(rnaturalearth)
library(worldfootballR)
library(dplyr)
library(sf)

# ------------------------------------------------------------------------------
# getting map data
# ------------------------------------------------------------------------------

world <- ne_countries(scale = 50, returnclass = "sf")
europe <- subset(world, continent == "Europe")

# ------------------------------------------------------------------------------
# getting player data
# ------------------------------------------------------------------------------

# Get data about premier league players and their nationalities
team_urls <- tm_league_team_urls("England", 2023)
epl_players_2023 <- tm_squad_stats(team_urls)

# summarise by countries
players <- epl_players_2023 %>%
  mutate(nationality = case_when(
    nationality == "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
    nationality == "Cote d'Ivoire" ~ "Côte d'Ivoire",
    nationality == "Korea, South" ~ "Republic of Korea",
    nationality == "DR Congo" ~ "Democratic Republic of the Congo",
    nationality == "North Macedonia" ~ "Macedonia",
    nationality %in% c("England", "Scotland", "Wales", "Northern Ireland") ~ "United Kingdom",
    TRUE ~ nationality)) %>%
  group_by(nationality) %>%
  summarise(player_count = n(), playtime = sum(minutes_played)) %>%
  arrange(nationality) %>% 
  arrange(-player_count)


# ------------------------------------------------------------------------------
# joining map data with player data
# ------------------------------------------------------------------------------

mapData <- world %>% 
  left_join(players, by = c("name_long" = "nationality")) %>% 
  mutate(player_count = ifelse(is.na(player_count), 0, player_count)) %>% 
  mutate(playtime = ifelse(is.na(playtime), 0, playtime))
  

# testing if country names coincide
# ------------------------------------------------------------------------------
# test <- mapData %>% 
#   filter(is.na(player_count) == FALSE) %>% 
#   select(name_long, player_count) %>% 
#   arrange(name_long) %>% 
#   arrange(-player_count)


# ------------------------------------------------------------------------------
# drawing a map
# ------------------------------------------------------------------------------

bins <- c(1,2,5,10,20,30,50,Inf)
pal <- colorBin("YlOrRd", domain = mapData$player_count, bins = bins)
# pal <- colorQuantile("YlOrRd", domain = mapData$player_count, n = 7)

labels <- sprintf(
  "<strong>%s</strong><br/>%g players<br/><strong><small style='font-size: 0.7em;'>%g minutes played</small></strong>",
  mapData$admin, mapData$player_count, mapData$playtime
) %>% lapply(htmltools::HTML)

title <- "<strong><small style='font-size: 1.8em; padding: 0.2em 0.4em; background-color: rgba(255, 255, 255, 0.0); border-radius: 0.5em;'>Number of Premier League players by nationality</small></strong>"

leaflet(mapData) %>% 
  addTiles() %>% 
  setView(18,50,4.2) %>% 
  
  addPolygons(
    
    fillColor = ~pal(player_count),
    weight = 1,
    opacity = 0.5,
    color = "#555555",
    dashArray = "3",
    fillOpacity = 0.7,
    
    highlightOptions = highlightOptions(
      weight = 4,
      color = "#222222",
      opacity = 0.8,
      bringToFront = TRUE),
    
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    
  ) %>% 

  addLegend(
    pal = pal,
    values = ~player_count,
    opacity = 0.7,
    title = NULL,
    position = "bottomright") %>% 
  
  addControl(html = title,
             position = "topright")
  
