library(dplyr)
library(ggplot2)

df <- read.csv("fastfood.csv")

svg("Wykres2.svg", bg="transparent")
ggplot(df, aes(x = restaurant, y = calories)) +
  geom_violin(trim = T, fill = "tomato3") +
  labs(title = "Distribution of calories in restaurants' meals",
       x = "Restaurant",
       y = "Calories") +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 10, margin = margin(0, 0, 0, 15)),
    axis.title.y = element_text(angle = 90),
    axis.text.x = element_text(size = 10, margin = margin(0, 0, 15, 0)), 
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.grid = element_line(color = "black", size = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.5)
    ) +
  coord_flip() 
dev.off()

