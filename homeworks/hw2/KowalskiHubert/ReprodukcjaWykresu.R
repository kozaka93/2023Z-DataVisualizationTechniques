library(dplyr)
library(ggplot2)
library(ggthemes)

colors <- c("gray80", "red2", "black", "aliceblue", "yellow3", 
            "blue4", "darkred", "forestgreen", "lightgoldenrod4")
kolory <- c("srebrny", "czerwony", "czarny", "biały", "żółty", 
            "niebieski", "burgundowy", "zielony", "złoty")
names(colors) <- kolory
num <- c(2,3,5,4,1,5,1,1,1)
sum <- sum(num)

cars <- data.frame("car_color" = kolory, "num_cars" = num)

cars <- cars %>% 
  mutate(car_color = forcats::fct_reorder(car_color, num_cars))


cars %>%
  ggplot(aes(x = car_color, y = num_cars, fill = car_color)) +
  geom_col(color = "black") + 
  geom_text(label = cars$num_cars, vjust = -0.5, size = 7, color = "grey40") + 
  labs(title = "Liczba samochodów w danym kolorze",
       subtitle = paste0("Łączna liczba posiadanych samochodów: ", sum),
       x = "Kolor samochodu",
       y = "Liczba samochodów") +
  theme_excel_new() +
  theme(
    legend.position = "none",
    plot.title= element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),
    axis.title.y.left = element_text(margin = margin(r = 10, l = 10)),
    axis.title.x.bottom = element_text(margin = margin(t = 10, b = 10)),
    panel.grid.major.x = element_blank(),
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 5.5)) + 
  scale_fill_manual(values = colors[forcats::fct_inorder(cars$car_color)])

