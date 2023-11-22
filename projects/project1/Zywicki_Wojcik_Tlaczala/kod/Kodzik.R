library(dplyr)
library(ggplot2)
library(fmsb)
library(readxl)

df2 <- read_excel("MyFoodData-Nutrition-Facts-SpreadSheet-Release-1-4.xlsx")
d <- df2
colnames(d) <- d[3, ]
d <- d[-c(1:3), ]

for (col in colnames(d)[4:98]) {
  d[[col]] <- gsub("NULL", "0", d[[col]])
  d[[col]] <- as.numeric(d[[col]])
}


d %>%
  group_by(`Food Group`) %>%
  summarise(Protein = round(mean(`Protein (g)`), 1),
            Carbs = round(mean(`Carbohydrate (g)`), 1),
            Fat = round(mean(`Fat (g)`), 1),
            Sugar = round(mean(`Sugars (g)`), 1),
            AvgKcal = round(mean(Calories))) -> av

av_rc <- av[av$`Food Group` %in% c("Dairy and Egg Products",
                                   "Nuts and Seeds",
                                   "Grains and Pasta",
                                   "Meats"), ]

z <- data.frame(rbind(c("Max", 22, 51, 41, 20, 520), c("Min", rep(0, 5)), av_rc))
rownames(z) <- z[, 1]
z <- z[, -1]
z[] <- sapply(z, as.numeric)


Cairo::CairoPNG("Dairy.png", width = 8000, height = 8000, bg = "transparent")
# Dairy
radarchart(z[c(1,2,3), ],
           seg = 3,     
           plwd = 50,
           plty = 1,
           pcol = rgb(0.173, 0.059, 0.749, 1),
           pfcol = rgb(0.314, 0.18, 0.984, 0.7),
           axistype = 0, # 0:5
           cglty = 1,
           cglcol = rgb(0.714, 0.718, 0.8, 1),
           cglwd = 35,
           vlabels = c(rep("", 5)))
dev.off()

Cairo::CairoPNG("Grains.png", width = 8000, height = 8000, bg = "transparent")
# Grains and pasta
radarchart(z[c(1,2,4), ],
           seg = 3,     
           plwd = 50,
           plty = 1,
           pcol = rgb(0.173, 0.059, 0.749, 1),
           pfcol = rgb(0.314, 0.18, 0.984, 0.7),
           axistype = 0, # 0:5
           cglty = 1,
           cglcol = rgb(0.714, 0.718, 0.8, 1),
           cglwd = 35,
           vlabels = c(rep("", 5)))
dev.off()

Cairo::CairoPNG("Meats.png", width = 8000, height = 8000, bg = "transparent")
# Meats
radarchart(z[c(1,2,5), ],
           seg = 3,     
           plwd = 50,
           plty = 1,
           pcol = rgb(0.173, 0.059, 0.749, 1),
           pfcol = rgb(0.314, 0.18, 0.984, 0.7),
           axistype = 0, # 0:5
           cglty = 1,
           cglcol = rgb(0.714, 0.718, 0.8, 1),
           cglwd = 35,
           vlabels = c(rep("", 5)))
dev.off()

Cairo::CairoPNG("Nuts.png", width = 8000, height = 8000, bg = "transparent")
# Nuts and seeds
radarchart(z[c(1,2,6), ],
           seg = 3,     
           plwd = 50,
           plty = 1,
           pcol = rgb(0.173, 0.059, 0.749, 1),
           pfcol = rgb(0.314, 0.18, 0.984, 0.7),
           axistype = 0, # 0:5
           cglty = 1,
           cglcol = rgb(0.714, 0.718, 0.8, 1),
           cglwd = 35,
           vlabels = c(rep("", 5)))
dev.off()

########################################################################
# Tutaj ta mapka 
########################################################################

library(readr)
library(stringr)
library(RColorBrewer)

meat_consumption <- read.csv("Mapka/FoodBalanceSheets_E_All_Data_(Normalized).csv")
item_codes <- read.csv("Mapka/FoodBalanceSheets_E_ItemCodes.csv")

meat_map <- meat_consumption %>% 
  filter(Item %in% c("Bovine Meat", "Mutton & Goat Meat", "Pigmeat", "Poultry Meat", "Meat Other")
         & Element == "Food supply quantity (kg/capita/yr)") %>% 
  select(Area, Item, Element, Year, Value) %>% 
  group_by(Area, Item) %>% 
  summarise(Value = mean(Value)) %>% 
  top_n(1, wt=Value)

mapdata <- map_data("world")

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


mapdata2 <- mapdata1 %>% 
  filter(region != "Antarctica") %>% 
  select(long, lat, group, order, region, Item)


kolorki <- c("Pigmeat" = "#ed5896",
             "Bovine Meat" = "#c40658",
             "Mutton & Goat Meat" = "#9a39db",
             "Poultry Meat" = "#601efa")
# Jak to czytacie to szacun
Cairo::CairoPNG("Mapunia.png", width = 7000, height = 4500, bg = "transparent")
ggplot(mapdata2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Item), color = "black", linewidth = 0.1) +
  coord_quickmap() +
  scale_fill_manual(values = kolorki, name = "Type of meat") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        text = element_text(size = 18),  
        plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 16)) +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1.2)
dev.off()


################################################################################
# A tu jakieś kreski
################################################################################

commodity_prices <- read_excel("Real commodity prices, 1850-2020.xlsx")

colnames(commodity_prices) <- commodity_prices[1, ]
commodity_prices <- commodity_prices[-1, ]

food_prices <- commodity_prices %>%
  mutate(Year = as.numeric(`(1900=100)`),
         Beef = as.numeric(Beef),
         Lamb = as.numeric(Lamb),
         Pork = as.numeric(Pork)) %>%  
  filter(Year >= 1919) %>%
  select(Year, Beef, Lamb, Pork)

meat_colors <- c("Beef" = "#bd08bd", "Lamb" = "#c1ed0e", "Pork" = "#502efb")

Cairo::CairoPNG("MeatPrices.png", width = 8000, height = 6000, bg = "transparent")
ggplot(food_prices, aes(x = Year)) +
  geom_line(aes(y = Beef, color = "Beef"), linewidth = 20) +
  geom_line(aes(y = Lamb, color = "Lamb"), linewidth = 20) +
  geom_line(aes(y = Pork, color = "Pork"), linewidth = 20) +
  scale_color_manual(values = meat_colors) +
  scale_x_continuous(limits = c(1919, 2025)) +
  theme(
    panel.border = element_rect(size = 20, fill = NA),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 180, color = "white"),
    axis.text.y = element_text(size = 180, color = "white"),
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())
dev.off()


## wykres cen białka dla różnych produktów
polish_products <- read_excel("Custom data.xlsx")

per100g <- data %>% 
  mutate(cost_per100g_protein = (100/`protein(per100g)`) * (100 * price / mass(g))) %>% 
  group_by(product, category) %>% 
  summarise(cost_per100g_protein = round(mean(cost_per100g_protein),2)) %>% 
  arrange(-cost_per100g_protein)

per100g$product <- factor(per100g$product, levels = per100g$product)


ggplot(per100g, aes(x = as.factor(product), y = cost_per100g_protein))+
  geom_col(fill = "#502efb") +
  theme_minimal() +
  labs(title = "Cost of 100g of protein",
       x = "Product",
       y = "Cost (PLN)",
       fill = "Category") +
  theme_minimal()  +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "none",
        title = element_blank()) +
  coord_flip() +
  theme_void() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, color = "white", size = 140, face = "bold"),
        axis.text.x = element_text(color = "white", size = 140, face = "bold"),
        panel.grid.major = element_line(color = "grey77", linewidth = 0.7),
        title = element_blank())
