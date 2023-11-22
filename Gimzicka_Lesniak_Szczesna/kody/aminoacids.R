data <- read.csv("nutrition.csv")
super_foods <- read.csv("super_foods.csv")
library(dplyr)
library(ggplot2)
library(tidyr)

food <- c("Quinoa, uncooked", "Nuts, almonds", "Seeds, dried, chia seeds",
          "Lentils, raw", "Spinach, raw",
          "Fish, raw, wild, Atlantic, salmon", "Peas, raw, green", 
          "Soybeans, raw, green", "Avocados, all commercial varieties, raw",
          "Oats", "Nuts, english, walnuts", "Beans, raw, mature seeds, black",
          "Chickpeas (garbanzo beans, bengal gram), raw, mature seeds",
          "Seeds, hulled, hemp seed")

aminoacids <- c("phenylalanine", "valine", "threonine", "tryptophan", 
                "methionine", "leucine", "isoleucine", "lysine", "histidine")

data_food2 <- data%>%
  mutate(food_name = strsplit(name, ",") %>%
           sapply(function(x) x[1]))

data_food2 %>% 
  filter(food_name %in% c("Potatoes", "Tofu", "Soybean", "Avocado", "Beans",
                          "Lentils", "Peas", "Chickpeas", "Quinoa", "Spinach",
                          "Broccoli", "Spirulina")) %>% 
  mutate(type = "vegetables") -> vegetables

data_food2 %>% 
  filter(food_name %in% c("Almonds", "Walnuts", "Seeds", "Nuts", "Peanuts")) %>% 
  mutate(type = "nuts") -> nuts

data_food2 %>% 
  filter(name %in% c("Fish, raw, bluefin, fresh, tuna", 
                     "Fish, drained solids with bone, canned in oil, Atlantic, sardine",
                     "Fish, raw, Atlantic, cod",
                     "Fish, raw, Atlantic, herring",
                     "Fish, raw, farmed, rainbow, trout", 
                     "Fish, raw, wild, coho, salmon",
                     "ish, raw, Atlantic, ocean perch")) %>% 
  mutate(type = "fish") -> fish
    
data_amino <- rbind(vegetables, nuts, fish) %>% 
  select(type, aminoacids) %>% 
  mutate(across(phenylalanine:histidine, ~ as.numeric(sub(" g", "", .)))) %>% 
  filter(if_all(all_of(aminoacids), ~ . > 0.05)) %>% 
  mutate(sum = rowSums(select(., all_of(aminoacids)))) %>% 
  select(type, sum)

data_amino_boxplot <- data_amino <- rbind(vegetables, nuts, fish) %>% 
  select(type, aminoacids) %>% 
  mutate(across(phenylalanine:histidine, ~ as.numeric(sub(" g", "", .)))) %>% 
  filter(if_all(all_of(aminoacids), ~ . > 0.05)) %>% 
  mutate(sum = rowSums(select(., all_of(aminoacids)))) %>% 
  select(type, sum)

data %>% 
  filter(name %in% food) %>% 
  select(name, aminoacids) %>% 
  mutate(across(phenylalanine:histidine, ~ as.numeric(sub(" g", "", .)))) %>% 
  pivot_longer(cols = -name, names_to = 'variable', values_to = 'value' )-> data2

super_foods %>% 
  filter(if_all(all_of(aminoacids), ~ . > 0.05)) %>% 
  select(type, aminoacids) %>% 
  mutate(across(phenylalanine:histidine, ~ as.numeric(sub(" g", "", .)))) %>% 
  mutate(sum = rowSums(select(., all_of(aminoacids)))) %>% 
  select(type, sum) -> data3

  summarise(m = mean(across(phenylalanine:histidine))) %>% 
  View()
  pivot_longer(cols = -type, names_to = 'variable', values_to = 'value' )-> data3




ggplot(data_amino, aes(x = sum, fill = type)) +
  geom_density(alpha = 0.85) +
  labs(title = 'Density Plot of Amino Acid Content in Different Foods',
       x = 'Amino Acid Amount [g]',
       fill = 'Type of superfood') +
  scale_fill_manual(values  = c("#9600ff", "#ff00e7", "#ff005a")) +
  theme(
    line = element_line(linewidth = 0),
    legend.text = element_text(size = 14),
    axis.text = element_text(color = "black", size = 12),
    panel.grid = element_line(linewidth = 0.2),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
  ) -> p
  
p
ggsave("gestosc4.png", plot = p, width = 10, height = 6, units = "in")

ggplot(data_amino_boxplot, aes(x = sum, y = type)) +
  geom_violin(width=1.5, fill = "#7ca3ff60") +
  labs(title = 'Violin Plot of Amino Acid Content in Different Foods',
       x = 'Amino Acid Amount',
       y = 'type') +
  theme_minimal()


  