data_food <- read.csv("nutrition.csv")

library(dplyr)
library(ggplot)
library(tidyr)

data_food2 <- data_food %>%
  mutate(food_name = strsplit(name, ",") %>%
           sapply(function(x) x[1]))



berries <- data_food2 %>% 
  filter(food_name %in% c("Goji berries", "Strawberries", "Raspberries",
                          "Blackberries", "Cranberries", "Gooseberries", 
                          "Mullberries", "Boysenberries")) %>% 
  mutate(type = "Berries")



leafy_greens <- data_food2 %>% 
  filter(food_name %in% c("Spinach", "Kale", "Arugula", "Lettuce", "Chard",
                          "Collards")) %>% 
  mutate(type = "Leafy greens")

nuts_seeds <- data_food2 %>% 
  filter(food_name %in% c("Almonds", "Walnuts", "Seeds", "Nuts", "Peanuts")) %>% 
  mutate(type ="Nuts and seeds")

fish <- data_food2 %>% 
  filter(food_name %in% c("Fish")) %>% 
  mutate(type = "Fish")

vegetables <- data_food2 %>% 
  filter(food_name %in% c("Avocados", "Sweet Potato", "Broccoli", "Arugula",
                          "Brussels sprouts", "Cabbage", "Cauliflower", 
                          "Radishes", "Turnips", "Carrot")) %>% 
  mutate(type = "Vegetables")

fruits <- data_food2 %>% 
  filter(food_name %in% c("Apples", "Kiwifruit", "Grapefruit", "Pomegranates",
                          "Grapes")) %>% 
  mutate(type = "Fruit")

other <- data_food2 %>% 
  filter(food_name %in% c("Seaweed", "Mushrooms", "Oats", "Garlic")) %>% 
  mutate(type = "Other")


super_foods <- bind_rows(leafy_greens, nuts_seeds,
                                  berries,fish, fruits,
                                  vegetables, other)

#write.csv(super_foods, "super_foods.csv")

berries <- berries %>%
  mutate(across(vitamin_b6:zink, ~ as.numeric(sub(" mg", "", .))),
         .names = "numeric_{.col}")

leafy_greens <- leafy_greens %>%
  mutate(across(vitamin_b6:zink, ~ as.numeric(sub(" mg", "", .))),
         .names = "numeric_{.col}")

fish <- fish %>%
  mutate(across(vitamin_b6:zink, ~ as.numeric(sub(" mg", "", .))),
         .names = "numeric_{.col}")

fruits <- fruits %>%
  mutate(across(vitamin_b6:zink, ~ as.numeric(sub(" mg", "", .))),
         .names = "numeric_{.col}")

nuts_seeds <- nuts_seeds %>%
  mutate(across(vitamin_b6:zink, ~ as.numeric(sub(" mg", "", .))),
         .names = "numeric_{.col}")

vegetables <- vegetables %>%
  mutate(across(vitamin_b6:zink, ~ as.numeric(sub(" mg", "", .))),
         .names = "numeric_{.col}")

other <- other %>%
  mutate(across(vitamin_b6:zink, ~ as.numeric(sub(" mg", "", .))),
         .names = "numeric_{.col}")

berries_micro <- berries %>% 
  group_by(name) %>% 
  summarise(micro = sum(across(calcium:potassium))) %>% 
  arrange(desc(micro)) %>% 
  mutate(type = "Berries")


leafy_greens_micro <- leafy_greens %>% 
  group_by(name) %>% 
  summarise(micro = sum(across(calcium:potassium))) %>% 
  arrange(desc(micro)) %>% 
  mutate(type = "Leafy greens")

fish_micro <- fish %>% 
  group_by(name) %>% 
  summarise(micro = sum(across(calcium:potassium))) %>% 
  arrange(desc(micro)) %>% 
  mutate(type = "Fish")

fruits_micro <- fruits %>% 
  group_by(name) %>% 
  summarise(micro = sum(across(calcium:potassium))) %>% 
  arrange(desc(micro)) %>% 
  mutate(type = "Fruit")

nuts_seeds_micro <- nuts_seeds %>% 
  group_by(name) %>% 
  summarise(micro = sum(across(calcium:potassium))) %>% 
  arrange(desc(micro)) %>% 
  mutate(type = "Nuts and seeds")

vegetables_micro <- vegetables %>% 
  group_by(name) %>% 
  summarise(micro = sum(across(calcium:potassium))) %>% 
  arrange(desc(micro)) %>% 
  mutate(type = "Vegetables")

other_micro <- other %>% 
  group_by(name) %>% 
  summarise(micro = sum(across(calcium:potassium))) %>% 
  arrange(desc(micro)) %>% 
  mutate(type = "Other")


micro <- bind_rows(leafy_greens_micro, nuts_seeds_micro,berries_micro,
                     fish_micro, fruits_micro, vegetables_micro, other_micro)
write.csv(micro, file = "micro.csv", sep = ",")
read.csv("micro.csv") -> micro



## wykres boxplot rozkładu skłądników mineralnych od typu superfoods


custom_order <- c("Other", "Vegetables", "Leafy greens", "Fruit", "Fish",
                  "Nuts and seeds", "Berries")

# Tworzymy DataFrame z niestandardową kolejnością
micro$type <- factor(micro$type, levels = custom_order)


ggplot(micro, aes(x = `type`, y = `micro`)) +
  geom_violin(width=1.25, fill = "#9600ff98") +
  geom_boxplot(width=0.3, color="#9600ff", outlier.colour = "#9600ff") +
  labs(title = "Distribution of micronutrients", 
       y = "Total amount of micronutrients, log scale",
       x = "Superfood type") +
  theme_minimal(base_size = 16) +
  coord_flip() +
  scale_y_log10() +
  theme(title = element_text(size = 18),
        axis.title = element_text(size = 14)) +
  theme(
    axis.text = element_text(color = "black", size = 14),
    panel.grid = element_line(linewidth = 0.4), 
    panel.grid.major.y = element_blank(),  # Usuwamy linie poziome
    panel.grid.minor.y = element_blank(),  # Usuwamy linie poziome
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA))-> p
p
ggsave("distribution.png", plot = p, width = 8, height = 6, units = "in")

#ff005a

ggplot(micro, aes(x = `type`, y = `micro`)) +
  geom_violin(width=1.25, fill = "#ff005a98") +
  geom_boxplot(width=0.3, color="#ff005a", outlier.colour = "#ff005a") +
  labs(title = "Distribution of micronutrients", 
       y = "Total amount of micronutrients, log scale",
       x = "Superfood type") +
  theme_minimal(base_size = 16) +
  coord_flip() +
  scale_y_log10() +
  theme(title = element_text(size = 18),
        axis.title = element_text(size = 14)) +
  theme(
    axis.text = element_text(color = "black", size = 14),
    panel.grid = element_line(linewidth = 0.4), 
    panel.grid.major.y = element_blank(),  # Usuwamy linie poziome
    panel.grid.minor.y = element_blank(),  # Usuwamy linie poziome
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA))-> p2

ggsave("distribution2.png", plot = p2, width = 8, height = 6, units = "in")


ggplot(micro, aes(x = `type`, y = `micro`)) +
  geom_violin(width=1.25, fill = "#ff00e798") +
  geom_boxplot(width=0.3, color="#ff00e7", outlier.colour = "#ff00e7") +
  labs(title = "Distribution of micronutrients", 
       y = "Total amount of micronutrients, log scale",
       x = "Superfood type") +
  theme_minimal(base_size = 16) +
  coord_flip() +
  scale_y_log10() +
  theme(title = element_text(size = 18),
        axis.title = element_text(size = 14)) +
  theme(
    axis.text = element_text(color = "black", size = 14),
    panel.grid = element_line(linewidth = 0.4), # grubość linii 
    panel.grid.major.y = element_blank(),  # Usuwamy linie poziome
    panel.grid.minor.y = element_blank(),  # Usuwamy linie poziome
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA))-> p3

ggsave("distribution3.png", plot = p3, width = 8, height = 6, units = "in")
