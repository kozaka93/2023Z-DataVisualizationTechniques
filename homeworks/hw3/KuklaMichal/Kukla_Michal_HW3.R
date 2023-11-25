library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)



df <- read.csv("C:/Users/micha/OneDrive/Desktop/Michal studia-LAPTOP-6IKBDHP7/Techniki wizualizacji danych/Laboratoria/KuklaMichal/final.csv")

df <-
  df %>% mutate(region = country)

w1 <- map_data("world") %>% filter(long <= 180)  # usuwam błędy związane z Alaską - było na laboratoriach o tym

w1 <-
  w1 %>% full_join(df, by = "region")

w1_filtered <- 
  w1 %>% select(long, lat, group, region, suicide_rate)




p1 <-
  ggplot() +
  geom_polygon(data = w1_filtered, aes(x = long, y = lat, fill = suicide_rate, group = group, colour = "")) +
  coord_fixed(xlim = c(-20, 39), ylim = c(37, 70), ratio = 1.4) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.box = "vertical",  # Położenie pionowe legendy
        legend.box.margin = margin(c(0, 0, 10, 0)),  # Margines na dole legendy
        legend.margin = margin(c(0, 0, 0, 0))) +  # Margines na dole całej legendy
  labs(title = "Suicide rate in Europe",
       subtitle = "Year 2022",
       fill = "number of suicides per 100,000 people") +
  scale_fill_distiller(palette = 3,
                       direction = 2,
                       na.value = "#949391") +
  scale_color_manual(values = NA) +
  guides(color = guide_legend("No data", override.aes = list(fill = "#949391", colour = "#61605e")),
         fill = guide_colorbar(barwidth = 1, barheight = 8, nbin = 20, title.position = "top"))

p1

ggsave("Kukla_Michal_HW3.png", plot = p1, width = 10, height = 6, bg = "white")
