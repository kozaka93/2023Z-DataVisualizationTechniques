library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

df <- read.csv("./Data/Poland-1960.csv")
df <- df %>% pivot_longer(!Age, values_to = "Population", names_to = "Gender") %>%
  mutate(Age = fct_inorder(Age))

Men_Population <- df %>% filter(Gender == "M") %>% summarise(s = sum(Population)) %>% .$s
Women_Population <- df %>% filter(Gender == "F") %>% summarise(s = sum(Population)) %>% .$s
markers_mainstar <- data.frame(x = c(0), y = c("90-94"))
markers_sidestars <- data.frame(x = c(5 ,8, 5, 5, 11 ,11 ,7 ,10 ,9 ,10 ,6 ,8 ,
                                      -8,-7,-11,-12,-10,-11,-6,-6,-8,-11,-9,-8), 
                                y = rep(c("70-74", "80-84", "95-99", "60-64", "45-49", "90-94", "50-54", "15-19"), 3))
markers_baubles <- data.frame(x = c(-10,   7,     -2,    3,       -4,     5.5,      4,       -3,      2,       -2),
                              y = c("0-4", "5-9", "5-9", "15-19", "25-29", "30-34", "50-54", "55-59", "65-69", "40-44"))

ggplot(df) +
  geom_bar(aes(y = Age, 
               x = ifelse(test = Gender=="M",
                          yes = -(Population/Men_Population)*100,
                          no = (Population/Women_Population)*100 )),
           stat = "identity", fill = "darkgreen", linewidth = 40) +
  geom_point(data = markers_mainstar, aes(x = x, y = y), shape = "\u2605", size = 30, color = "yellow") +
  geom_point(data = markers_sidestars, aes(x = x, y = y), shape = "\u2605", size = 7, color = "white") +
  geom_point(data = markers_baubles, aes(x=x, y=y), shape = "\u25CF", size = 8, color = rep(c("red", "gold3"), 5)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "midnightblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "white", hjust = 0.5),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.position = "none"
        ) +
  labs(title = "Świąteczna piramida wieku Polska 1960",
       x = "Mężczyźni              %              Kobiety",
       y = "Wiek")
  

