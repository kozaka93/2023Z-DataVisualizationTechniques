library(tidyr)
library(dplyr)
library(ggplot2)
library(SmarterPoland)

data2 <- read.csv("data/per-capita-sources-of-protein.csv")

data21 <- data2 %>% 
  filter(Entity %in% c("Africa","Europe","Oceania","Americas (FAO)","Asia") & Year == 2020) %>%
  rename(Plant_protein = Vegetal.Products...00002903....Food.available.for.consumption...0674pc....grams.of.protein.per.day.per.capita,
            Meat = Meat..total...00002943....Food.available.for.consumption...0674pc....grams.of.protein.per.day.per.capita,
            Eggs = All.egg.products...00002744....Food.available.for.consumption...0674pc....grams.of.protein.per.day.per.capita,
            Dairy = Milk...00002948....Food.available.for.consumption...0674pc....grams.of.protein.per.day.per.capita,
            Fish_and_seafood = Fish.and.seafood...00002960....Food.available.for.consumption...0674pc....grams.of.protein.per.day.per.capita) %>% 
  mutate(total = Plant_protein + Meat + Eggs + Dairy + Fish_and_seafood,
         Entity = forcats::fct_reorder(Entity, total)) %>% 
  select(-Code, -Year, -total)
  
data21_long <- data21 %>%
  pivot_longer(cols = -Entity, names_to = "Variable", values_to = "value")

ggplot(data21_long, aes(x = Entity, y = value, fill = Variable)) +
  geom_bar(width = 0.5, position = "stack", stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#2bc2bd", "#c9752a", "#9e884f", "#7d4754", "#477d65")) +
  geom_text(aes(label = ifelse(value > 5, paste(round(value), "g"), " ")), hjust = -0.1, size = 4, position = position_stack(vjust = 0.5)) +
  labs(title = "Per capita sources of protein, 2020",
       subtitle = "Daily protein sources are measured as the average supply of protein, in grams per capita per day.",
       x = "",
       y = "grams") +
  theme_minimal(base_size = 12, base_family = "") +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 30),
        plot.subtitle = element_text(size = 18),
        #legend.title = element_blank(),
        plot.background = element_rect(fill = "#afc1d0"),
        panel.background = element_rect(fill = "#afc1d0"),
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),  
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 0, face = "bold")) + 
  guides(fill = guide_legend(reverse = TRUE))

