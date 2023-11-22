library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)

#Data source: https://www.fao.org/faostat/en/#data/FBS

meat_consumption <- read.csv("FoodBalanceSheets_E_All_Data_(Normalized).csv")

meat_map <- meat_consumption %>% 
  filter(Item %in% c("Bovine Meat", "Mutton & Goat Meat", "Pigmeat", "Poultry Meat", "Meat Other")
         & Element == "Food supply quantity (kg/capita/yr)") %>% 
  select(Area, Item, Element, Year, Value) %>% 
  group_by(Area, Item) %>% 
  summarise(Value = mean(Value)) %>% 
  top_n(1, wt=Value)

mapdata <- map_data("world")

mapdata_countries <- unique(mapdata$region)
meat_map_countries <- unique(meat_map$Area)
missing_countries <- setdiff(meat_map_countries, mapdata_countries)

patterns <- c("Bolivia \\(Plurinational State of\\)", "C\xf4te d'Ivoire",
              "Cabo Verde", "Congo",
              "Czechia", "Democratic People's Republic of Korea",
              "Eswatini", "Iran \\(Islamic Republic of\\)",
              "Micronesia \\(Federated States of\\)",
              "Netherlands \\(Kingdom of the\\)",
              "Polynesia", "Republic of Korea", "Republic of Moldova",
              "Russian Federation", "Saint Kitts and Nevis",
              "Saint Vincent and the Grenadines", "Syrian Arab Republic",
              "Trinidad and Tobago", "United Kingdom of Great Britain and Northern Ireland",
              "United Republic of Tanzania", "United States of America",
              "Venezuela \\(Bolivarian Republic of\\)", "Viet Nam")


replacements <- c("Bolivia","Ivory Coast",
                  "Cape Verde", "Republic of Congo", "Czech Republic",
                  "North Korea", "Swaziland",
                  "Iran",
                  "Micronesia", "Netherlands", 
                  "French Polynesia", "South Korea", "Moldova",
                  "Russia", "Saint Kitts",
                  "Saint Vincent", "Syria",
                  "Trinidad", "UK",
                  "Tanzania", "USA", 
                  "Venezuela", "Vietnam")

meat_map$Area <- str_replace_all(meat_map$Area, setNames(replacements, patterns))

mapdata1 <- left_join(mapdata, meat_map, by = c("region" = "Area"))

kolorki <- c("brown1", "chocolate2", "palevioletred1", "gold")

legend_labels <- setNames(c("Bovine Meat", "Mutton & Goat Meat", "Pigmeat", "Poultry Meat", "No Data"), 
                          c("Bovine Meat", "Mutton & Goat Meat", "Pigmeat", "Poultry Meat", NA))


mapdata1 %>% 
  filter(region != "Antarctica") %>% 
  select(long, lat, group, order, region, Item) -> mapdata1

ggplot(mapdata1, mapping = aes(x = long, y = lat, group = group, fill = Item)) +
  geom_polygon(color = "black", linewidth = 0.1) +
  coord_quickmap() +
  scale_fill_manual(values = kolorki,
                    labels = legend_labels) +
  labs(title = "The most popular type of meat in each country") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  coord_fixed(ratio = 1.2)


