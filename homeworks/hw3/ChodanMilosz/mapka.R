Data <- read.csv('Food_Security_Data_E_All_Data_(Normalized).csv')

library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(sf)

d <- Data %>% 
  select(c("Area", "Item", "Element", "Year", "Value")) %>%
  filter(Item == "Prevalence of severe food insecurity in the total population (percent) (3-year average)") %>% 
  filter(Element == "Value") %>% 
  filter(Year == "2020-2022") %>% 
  select(c("Area", "Value")) %>%
  mutate(Value = ifelse(Value == "<0.5", "0.5", Value)) %>% 
  mutate(Value = as.numeric(Value)) %>%
  na.omit() %>%
  mutate(ID = row_number()) %>% 
  rename(name = Area) %>% 
  slice(1:148) %>%
  mutate(name = case_when(
    ID == 17 ~ "Bosnia and Herz.",
    ID == 26 ~ "Central African Rep.",
    ID == 31 ~ "Côte d'Ivoire",
    ID == 34 ~ "Dem. Rep. Congo",
    ID == 37 ~ "Dominican Rep.",
    ID == 42 ~ "eSwatini",
    ID == 61 ~ "Iran",
    ID == 72 ~ "Laos",
    ID == 94 ~ "Netherlands",
    ID == 106 ~ "South Korea",
    ID == 107 ~ "Moldova",
    ID == 109 ~ "Russia",
    ID == 124 ~ "S. Sudan",
    ID == 139 ~ "United Kingdom",
    ID == 140 ~ "Tanzania",
    ID == 141 ~ "United States of America",
    ID == 145 ~ "Vietnam",
    TRUE ~ name
  ))


world <- ne_countries(returnclass = "sf")
filtered_d1 <- semi_join(d, world, by = c("name"))
filtered_d2 <- semi_join(world, d, by = c("name"))
merged_data <- left_join(world, d, by = c("name"))


ggplot(merged_data) +
  geom_sf(aes(fill = Value), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#fee0d2", high = "darkred") +
  labs(title = "Percentage of the Population Suffering from Malnutrition") +
  theme_minimal()

ggsave("mapka.png", width = 10, height = 6, dpi = 300)