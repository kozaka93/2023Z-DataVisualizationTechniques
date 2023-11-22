library(dplyr)
library(ggplot2)
library(maps)


world_map <- map_data("world") %>% 
  filter(long <= 180)
population <- read.csv("world_population.csv", sep = ";") 
world_population <- population %>% 
  select(Population..Millions.of.people., X2028) %>% 
  mutate(population_2028 = as.numeric(gsub(",", "", X2028)) * 1000) %>% 
  mutate(country = Population..Millions.of.people.) %>% 
  select(country, population_2028)

data <- world_map %>% 
  left_join(world_population, join_by(region == country)) %>% 
  mutate(discrete_population = factor((case_when(is.na(population_2028) ~ "brak danych",
                                         population_2028 < 1000000 ~ "[0, 1)",
                                         population_2028 >= 1000000 & population_2028 < 10000000 ~ "[1, 10)",
                                         population_2028 >= 10000000 & population_2028 < 100000000 ~ "[10, 100)",
                                         population_2028 >= 100000000 & population_2028 < 1000000000 ~ "[100, 1000)",
                                         population_2028 >=  1000000000 ~ as.character(paste("[1000, ",
                                                                                             as.character(as.integer(max(.$population_2028, na.rm = TRUE) / 1000000 + 1)),
                                                                                             ")",
                                                                                             sep = "")))),
                                      # przeniesienie NA na sam dol
                                      ordered = TRUE,
                                      levels = c("brak danych", 
                                                 "[0, 1)", 
                                                 "[1, 10)", 
                                                 "[10, 100)", 
                                                 "[100, 1000)", 
                                                 as.character(paste("[1000, ",
                                                                     as.character(as.integer(max(.$population_2028, na.rm = TRUE) / 1000000 + 1)),
                                                                     ")",
                                                                     sep = "")))))


  
mapa <- ggplot() +
  geom_polygon(data = data, aes(x = long, 
                                y = lat, 
                                group = group, 
                                fill = discrete_population),
               color = "black") +
  coord_map("mollweide") +
  expand_limits(x = c(-190,190)) +
  labs(fill = "Populacja w milionach\n(skala logarytmiczna)",
       title = "Przewidywana populacja w państwach na świecie w 2028 roku") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("grey", "#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000")) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "white", 
                                       color = "white"),
        panel.background = element_rect(color = "white"),
        legend.position = c(0.22, 0.71), 
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(color = "black"),
        plot.title = element_text(family = "mono",
                                  face = "bold",
                                  size = 30,
                                  hjust = 0.5,
                                  margin = margin(b = 30)),
        legend.text = element_text(color = "black",
                                   size = 10,
                                   family = "mono",
                                   face = "bold"),
        legend.title = element_text(size = 11,
                                    family = "mono",
                                    face = "bold"),
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(1, "cm"))

ggsave("mapa.png", mapa, width = 16, height = 8)
  
