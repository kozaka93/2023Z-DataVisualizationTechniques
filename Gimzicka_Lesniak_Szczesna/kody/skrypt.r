#biblioteki
library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)
library(Cairo)
library(RColorBrewer)

#wczytanie danych
data <- read.csv("nutrition.csv")

#wczytanie czcionek
font_import()
loadfonts()


#przygotowanie ramki danych do pierwszego wykresu
antioxidants_names <-
  c(
    "vitamin_a",
    "vitamin_c",
    "vitamin_e",
    "carotene_alpha",
    "carotene_beta",
    "cryptoxanthin_beta",
    "lutein_zeaxanthin",
    "riboflavin",
    'manganese',
    "selenium",
    "zink"
  )


#zamiana danych w kolumnach na liczby
antioxidants_in_units <- data %>%
  select(name, all_of(setdiff(antioxidants_names, "vitamin_a")), vitamin_a_rae)
antioxidants_in_units$vitamin_c <-
  as.numeric(gsub(" mg", "", antioxidants_in_units$vitamin_c))
antioxidants_in_units$vitamin_e <-
  as.numeric(gsub(" mg", "", antioxidants_in_units$vitamin_e))
antioxidants_in_units$carotene_alpha <-
  as.numeric(gsub(" mcg", "", antioxidants_in_units$carotene_alpha))
antioxidants_in_units$carotene_beta <-
  as.numeric(gsub(" mcg", "", antioxidants_in_units$carotene_beta))
antioxidants_in_units$cryptoxanthin_beta <-
  as.numeric(gsub(" mcg", "", antioxidants_in_units$cryptoxanthin_beta))
antioxidants_in_units$lutein_zeaxanthin <-
  as.numeric(gsub(" mcg", "", antioxidants_in_units$lutein_zeaxanthin))
antioxidants_in_units$riboflavin <-
  as.numeric(gsub(" mg", "", antioxidants_in_units$riboflavin))
antioxidants_in_units$manganese <-
  as.numeric(gsub(" mg", "", antioxidants_in_units$manganese))
antioxidants_in_units$selenium <-
  as.numeric(gsub(" mcg", "", antioxidants_in_units$selenium))
antioxidants_in_units$zink <-
  as.numeric(gsub(" mg", "", antioxidants_in_units$zink))
antioxidants_in_units$vitamin_a_rae <-
  as.numeric(gsub(" mcg", "", antioxidants_in_units$vitamin_a_rae))

#wybór popularnych superfoods
antioxidant_food <-
  c(
    "Goji berries, dried",
    "Strawberries, raw",
    'Blackberries, raw',
    'Raspberries, raw',
    "Acerola, raw, (west indian cherry)",
    'Pomegranates, raw',
    "Avocados, all commercial varieties, raw",
    "Kale, raw",
    "Spinach, raw",
    "Brussels sprouts, raw",
    "Broccoli, raw",
    'Sweet potato, without skin, boiled, cooked',
    "Peas, raw, edible-podded",
    "Lentils, raw, sprouted",
    "Chickpeas (garbanzo beans, bengal gram), raw, mature seeds",
    "Nuts, english, walnuts",
    'Nuts, hazelnuts or filberts',
    'Nuts, unblanched, dried, brazilnuts',
    'Nuts, almonds',
    "Seeds, dried, chia seeds",
    'Quinoa, cooked',
    'Chocolate, 70-85% cacao solids, dark',
    "Seaweed, dried, spirulina",
    "Ginger root, raw",
    'Spices, ground, turmeric',
    "Egg, fried, cooked, whole",
    'Fish, raw, chinook, salmon'
  )

#tworzenie ramki danych z wybranymi superfoods i antyoksydantami
antioxidants_superfoods <- antioxidants_in_units %>%
  filter(name %in% antioxidant_food) %>%
  arrange(factor(name, levels = antioxidant_food)) %>%
  select(
    c(
      "name",
      "carotene_alpha",
      "carotene_beta",
      "cryptoxanthin_beta",
      "lutein_zeaxanthin",
      "riboflavin",
      "vitamin_a_rae",
      "vitamin_c",
      "vitamin_e",
      "manganese",
      "selenium",
      "zink"
    )
  )
#zamiana nazw na bardziej czytelne
antioxidants_superfoods$name <-
  c(
    "Goji berries",
    "Strawberries",
    'Blackberries',
    'Raspberries',
    "Acerola",
    'Pomegranate',
    "Avocado",
    "Kale",
    "Spinach",
    "Brussels sprouts",
    "Broccoli",
    'Sweet potato',
    "Peas",
    "Lentils",
    "Chickpeas",
    "Walnuts",
    'Hazelnuts',
    'Brazil nuts',
    'Almonds',
    "Chia seeds",
    'Quinoa',
    'Dark chocolate',
    "Spirulina",
    "Ginger",
    'Turmeric',
    "Eggs",
    'Salmon'
  )

#tworzenie ramki danych odpowiedniej do heatmapy, wyszukiwanie produktu o największej
#zawartości danego antyoksydantu, normalizowanie pozostałych względem niego
max_values <- apply(antioxidants_superfoods[,-1], 2, max)
col_names <- setdiff(names(antioxidants_superfoods), "name")

normalized_antioxidants <- antioxidants_superfoods
for (col in col_names) {
  normalized_antioxidants[[col]] <-
    antioxidants_superfoods[[col]] / max_values[[col]]
}

#zamiana ramki danych na format long
heatmap_data <- normalized_antioxidants %>%
  pivot_longer(
    cols = c(
      vitamin_c,
      vitamin_e,
      vitamin_a_rae,
      carotene_alpha,
      carotene_beta,
      cryptoxanthin_beta,
      lutein_zeaxanthin,
      riboflavin,
      manganese,
      selenium,
      zink
    ),
    names_to = "Antioxidant",
    values_to = "Intensity"
  )


#kolejność antyoksydantów na wykresie
order <-
  c(
    "name",
    "carotene_alpha",
    "carotene_beta",
    "cryptoxanthin_beta",
    "lutein_zeaxanthin",
    "riboflavin",
    "vitamin_a_rae",
    "vitamin_c",
    "vitamin_e",
    "manganese",
    "selenium",
    "zink"
  )

#tworzenie heatmapy
h_plot <-
  ggplot(heatmap_data, aes(
    x = factor(Antioxidant, levels = order),
    y = name,
    fill = Intensity
  )) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#9600ff",
    mid = "#ff00e7",
    high = "#ff008d",
    midpoint = 0.5
  )  +
  theme_minimal() +
  scale_y_discrete(limits = unique(heatmap_data_long$name)) +
  scale_x_discrete(
    labels = c(
      "α-Carotene",
      "β-Carotene",
      "β-Cryptoxanthin",
      "Lutein & Zeaxanthin",
      "Manganese",
      "Riboflavin",
      "Selenium",
      "Vitamin A",
      "Vitamin C",
      "Vitamin E",
      "Zink"
    ),
    breaks = c(
      "carotene_alpha",
      "carotene_beta",
      "cryptoxanthin_beta",
      "lutein_zeaxanthin",
      "manganese",
      "riboflavin",
      "selenium",
      "vitamin_a_rae",
      "vitamin_c",
      "vitamin_e",
      "zink"
    )
  ) +
  theme(
    text = element_text(family = "Raleway Medium"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_blank(),
    legend.key.size = unit(0.3, "cm"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5, "mm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    rect = element_rect(fill = "transparent")
  ) +
  labs(y = "Product",
       x = "Antioxidant",
       fill = "Intensity",
       title = "Antioxidant Content Heatmap of Superfoods") +
  coord_flip()

#zapis do pliku
ggsave(
  filename = "h_plot1.png",
  plot = h_plot,
  width = 1180 / 96,
  height = 450 / 96,
  units = "in",
  device = png,
  type = "cairo",
  family = "Raleway",
  bg = "transparent"
)

##Przygotowywanie ramki danych do drugiego wykresu

#wybór produktów (superfoods) zawierających węglowodany
carbohydrate_food <-
  c(
    "Goji berries, dried",
    "Strawberries, raw",
    "Blueberries, raw",
    'Raspberries, raw',
    "Acerola, raw, (west indian cherry)",
    'Pomegranates, raw',
    'Grapefruit, all areas, pink and red, raw',
    "Tomatoes, year round average, raw, ripe, red",
    'Sweet potato, without skin, boiled, cooked',
    "Chickpeas (garbanzo beans, bengal gram), raw, mature seeds",
    'Nuts, hazelnuts or filberts',
    'Nuts, almonds',
    "Seeds, dried, chia seeds",
    'Quinoa, cooked',
    'Oats',
    'Chocolate, 70-85% cacao solids, dark',
    'Spices, ground, turmeric'
  )

#tworzenie ramki danych z wybranymi produktami i iloscia weglowodanow
superfoods_carbohydrates <- data %>%
  filter(name %in% carbohydrate_food) %>%
  arrange(factor(name, levels = carbohydrate_food)) %>%
  select(c("name", "carbohydrate", "fiber", "sugars"))

#zamiana danych w kolumnach na numeryczne
superfoods_carbohydrates$sugars <-
  as.numeric(gsub(" g", "", superfoods_carbohydrates$sugars))
superfoods_carbohydrates$fiber <-
  as.numeric(gsub(" g", "", superfoods_carbohydrates$fiber))
superfoods_carbohydrates$carbohydrate <-
  as.numeric(gsub(" g", "", superfoods_carbohydrates$carbohydrate))

#dodanie kolumny starch (cześć węglodowanów nie będąca cukrami i błonnikiem)
superfoods_carbohydrates <- superfoods_carbohydrates %>%
  mutate(starch = ifelse(
    carbohydrate - sugars - fiber < 0,
    0,
    round(carbohydrate - sugars - fiber, 2)
  ))

#Zmiana nazw na bardziej czytelne
superfoods_carbohydrates$name <-
  c(
    "Goji berries",
    "Strawberries",
    "Blueberries",
    'Raspberries',
    "Acerola",
    'Pomegranate',
    "Grapefruit",
    'Tomatoes',
    'Sweet potato',
    "Chickpeas",
    'Hazelnuts',
    'Almonds',
    "Chia seeds",
    'Quinoa',
    'Oats',
    'Dark chocolate',
    'Turmeric'
  )

#zmiana formatu danych na długi
superfoods_carbohydrates_long <- superfoods_carbohydrates %>%
  pivot_longer(
    cols = c(sugars, fiber, starch),
    names_to = "type",
    values_to = "quantity"
  )

superfoods_carbohydrates_long$type <-
  factor(superfoods_carbohydrates_long$type,
         levels = c("sugars", "fiber", "starch"))

#tworzenie wykresu
s_plot <-
  ggplot(superfoods_carbohydrates_long,
         aes(
           x = reorder(name, quantity) ,
           y = quantity,
           fill = type
         )) +
  geom_col() +
  scale_fill_manual(values = c("#9600ff", "#ff00e7", "#ff005a")) +
  labs(x = "SUPERFOOD",
       y = "QUANTITY",
       size = "Carbohydrate") +
  theme(
    text = element_text(family = "Raleway Medium"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    rect = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(linewidth = 0.1),
    panel.grid.minor.y = element_line(linewidth = 0.1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key =  element_rect(fill = "transparent"),
    legend.key.size = unit(0.5, "cm"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(
      margin = margin(
        t = 0,
        r = 20,
        b = 0,
        l = 0
      ),
      family = "Raleway Medium",
      face = "bold"
    ),
    axis.title.x = element_text(
      margin = margin(
        t = 20,
        r = 0,
        b = 0,
        l = 0
      ),
      family = "Raleway Medium",
      face = "bold"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = 0))


ggsave(
  filename = "s_plot1.png",
  plot = s_plot,
  width = 626 / 96,
  height = 425 / 96,
  units = "in",
  bg = "transparent",
  device = png,
  type = "cairo",
  family = "Raleway"
)


