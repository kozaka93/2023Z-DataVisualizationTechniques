library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

df <- read.csv("ti-corruption-perception-index.csv")

colnames(df)[4] = "Corruption_index"

df_filtered <- df %>% 
  filter(Year == 2018) %>% 
  mutate(Entity = case_when(
    Entity == "United States" ~ "USA",
    Entity == "United Kingdom" ~ "UK",
    Entity == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    Entity == "Congo" ~ "Republic of Congo",
    Entity == "Cote d'Ivoire" ~ "Ivory Coast",
    Entity == "Czechia" ~ "Czech Republic",
    TRUE ~ Entity
  ))

w1 <- map_data("world")

w <- w1 %>% 
  left_join(df_filtered, by = c("region" = "Entity")) %>% 
  filter(region != "Antarctica")

library(RColorBrewer)

gg <- ggplot() +
  geom_polygon(data = w, aes(x = long, y = lat, fill = Corruption_index, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(3, "cm"),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 8)) +
  scale_fill_distiller(palette = 18,
                       direction = -1,
                       limits = c(0, 100),
                       breaks = seq(0, 100, by = 10)) +
  labs(title = "Wskaźnik postrzegania korupcji, 2018",
       subtitle = "Indeks postrzegania korupcji. Wyniki podaje się w skali od 0 do 100, gdzie 0 oznacza, że a kraj jest postrzegany jako wysoce skorumpowany.",
       fill = "Wskaźnik korupcji")
gg

