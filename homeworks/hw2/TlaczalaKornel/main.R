# Kornel Tłaczała on 2023-11-10
# script plotting premier league table after 10 gameweeks of 23/24 season

# installing these packeges may be necessary
# install.packages("jpeg")
# install.packages("ggimage")
# install.packages("ggchicklet", repos = "https://cinc.rud.is")

library(jpeg)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggimage)
# library(ggchicklet)
cutoff_date = "2023-11-04"

# loading data
results <- read.csv("src/data/EPL-results.csv") %>% 
  select(-Round.Number)

# format Date column
# include only matches played before 4th of November
points <- results %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y %H:%M")) %>% 
  filter(Date < as.Date(cutoff_date))
  
# calculating points based on the result
points <- points %>%
  separate(Result, into = c("Home.Score", "Away.Score"), sep = " - ") %>% 
  mutate(Home.Score = as.integer(Home.Score),
         Away.Score = as.integer(Away.Score))  %>% 
  mutate(
    Home.Points = case_when(
      Home.Score > Away.Score ~ 3,
      Home.Score == Away.Score ~ 1,
      TRUE ~ 0
    ),
    Away.Points = case_when(
      Away.Score > Home.Score ~ 3,
      Away.Score == Home.Score ~ 1,
      TRUE ~ 0
    )
  )
  
# simplify each row
# each game will have two records - one for each time
# each record will specify points scored by the team in this game
points <- points %>% 
  select(Match.Number, Date, Home.Team, Away.Team, Home.Points, Away.Points) %>% 
  rename(home = Home.Team, away = Away.Team) %>% 
  pivot_longer(
    cols = c(home, away),
    names_to = "Team.Type",
    values_to = "Team"
  ) %>% 
  mutate(Points = case_when(
    Team.Type == "home" ~ Home.Points,
    Team.Type == "away" ~ Away.Points
  )) %>% 
  select(-c(Home.Points, Away.Points, Team.Type))
  
# calculate sum of points scored by each team till the cutoff_date
# arrange by points 
points <- points %>% 
  group_by(Team) %>% 
  summarise(Points = sum(Points), Games.Played = n()) %>% 
  arrange(-Points)

# rename teams for consistency
points <- points %>% 
  mutate(Team = case_when(
    Team == "Spurs" ~ "Tottenham",
    Team == "Man City" ~ "Manchester City",
    Team == "Man Utd" ~ "Manchester United",
    Team == "Sheffield Utd" ~ "Sheffield United",
    TRUE ~ Team
  ))

# prepare just the top5 teams
points_top5 <- points %>% 
  head(5)


# plot top5

img <- "src/graphics/background.jpeg"

plot_top5 <- ggplot(points_top5, aes(x = reorder(Team, -Points), y = Points)) +
  # geom_chicklet(fill = "#e0005e", radius = grid::unit(2, "mm"), size = 1) +
  geom_col(fill = "#e0005e", color = "white", linewidth = 1) +
  geom_text(aes(label = Points, fontface = "bold"), vjust = 1.5, size = 16, color = "white", alpha = 0.85) +
  labs(title = 'Premier League "Top Five"', subtitle = "points after 10 games", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(color = "white", face = "bold", size = 12, margin = margin(b = 20)),
        axis.text.y = element_text(color = "white", face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 24, color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, color = "white"),
        legend.position = "none")
  
print(ggbackground(plot_top5, img))
# print(plot_top5)




# plot_all <- ggplot(
#   data = points,
#   mapping = aes(x = reorder(Team, -Points), y = Points)) +
#   geom_col() +
#   labs(title = "Premier League table after 10 games", x = NULL) + 
#   theme(axis.text.x = element_text(angle = 70, hjust = 1))

# print(plot_all)



