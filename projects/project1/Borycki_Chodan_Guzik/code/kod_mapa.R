Data <- read.csv('Food_Security_Data_E_All_Data_(Normalized).csv')
AreaCodes <- read.csv('Food_Security_Data_E_AreaCodes.csv')
Flags <- read.csv('Food_Security_Data_E_Flags.csv')

library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(sf)

d <- Data %>% 
  select(c("Area", "Item", "Element", "Year", "Value")) %>%
  filter(Item == "Prevalence of severe food insecurity in the total population (percent) (3-year average)")

d1 <- d %>% 
  filter(Element == "Value")

View(d1)

d2 <- d %>% 
  filter(Element == "Confidence interval: Lower bound") %>%
  rename(ValueLow = Value)

View(d2)

d3 <- d %>% 
  filter(Element == "Confidence interval: Upper bound") %>% 
  rename(ValueHigh = Value)

View(d3)

d1 <- left_join(d1, d2[, c("Area", "Year", "ValueLow")], by = c("Area", "Year"))
d1 <- left_join(d1, d3[, c("Area", "Year", "ValueHigh")], by = c("Area", "Year"))


d4 <- d1 %>% 
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

mean <- d4 %>% 
  summarize(mean = mean(Value))
print(mean)


View(d4)
print(d4)
world <- ne_countries(returnclass = "sf")
filtered_d1 <- semi_join(d4, world, by = c("name"))
filtered_d2 <- semi_join(world, d4, by = c("name"))
merged_data <- left_join(world, d4, by = c("name"))


ggplot(merged_data) +
  geom_sf(aes(fill = Value), color = "white", size = 0.2) +
  scale_fill_gradient(low = "lightblue1", high = "darkblue") +
  theme_minimal()

ggsave("mapka.png", width = 10, height = 6, dpi = 300)


d5 <- d1 %>% 
  filter(Area == "World" | Area == "Europe" | Area == "Asia" | Area == "Africa" | Area == "South America" | Area == "Northern America" | Area == "Australia and New Zealand") %>% 
  mutate(Value = as.numeric(Value)) %>% 

  select(c("Area", "Year", "Value", "ValueLow", "ValueHigh"))

ggplot(d5, aes(x = Year, y = Value, group = Area, color = Area)) +
  geom_line() +
  geom_point() +
  labs(title = "Zmiany wartości w czasie dla różnych regionów świata",
       x = "Rok",
       y = "Wartość (w %)") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_color_brewer(palette = "Set1")

ggsave("wykres.png", width = 10, height = 6, dpi = 300)

  

