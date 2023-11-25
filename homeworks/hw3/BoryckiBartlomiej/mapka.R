library(ggplot2)
library(sf)
library(spData)
library(dplyr)

male <- read.csv("average-height-of-men.csv")
View(male)

male <- male %>% filter(Year == 1996) 
colnames(male)[4] <- "Height"
male <- male %>% arrange(-Height)

world <- spData::world
world = st_transform(world, crs = "+proj=robin")
world <- merge(world,male,by.x="name_long",by.y="Entity",all.x=T)
world[131,"Height"] <- 167.4476
world[133,"Height"] <- 176.4605
world[41,"Height"] <- 166.7953
world <- world %>% filter(continent!="Antarctica")

plot <- ggplot(data = world) +
  geom_sf(aes(fill = Height), color = "black", size = 0.02) +
  labs(title = "Where do the tallest people live?",
       subtitle = "Average height of a male born in 1996",
       fill = "Height [cm]") +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold",hjust=0.1),
    plot.subtitle = element_text(size = 12, hjust = 0.1),
    legend.position = "right",
    legend.margin = margin(l = -1, unit = "cm"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_fill_viridis_c()

plot
