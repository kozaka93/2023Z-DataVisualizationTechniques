##wczytanie pakietów
library(dplyr)
library(readxl)
library(tidyr)
library(maps)
library(mapdata)
library(ggplot2)
library(plotly)

#wczytanie i poprawa danych
data <- read_excel("GLOBAL_DATAFLOW_2016-2023.xlsx")
data <- data %>%
  rename(Geographic_area = "Geographic area")

#filtrowanie wyników
data <- data %>%
  select(c(
    "Geographic_area",
    "Indicator",
    "Sex",
    "TIME_PERIOD",
    "OBS_VALUE"
  )) %>%
  filter(Indicator == "Educational attainment of the population (aged 25 years and older)")

#wybór danych z najnowszych lat
data <- data %>%
  mutate(TIME_PERIOD = as.numeric(TIME_PERIOD),
         OBS_VALUE = as.numeric(OBS_VALUE)) %>%
  group_by(Geographic_area) %>%
  slice_max(order_by = TIME_PERIOD) %>%
  ungroup()

#przygotowanie ramki danych
df_analysis <- data %>%
  pivot_wider(names_from = Sex, values_from = OBS_VALUE) %>%
  mutate(Difference = Male - Female) %>%
  rename(Female_OBS_VALUE = Female, Male_OBS_VALUE = Male)

#zamiana nazw krajów, w celu złączenia z ramką potrzebną do stworzenia mapy
df_analysis <- df_analysis %>%
  mutate(
    Geographic_area = ifelse(Geographic_area == "United States", "USA", Geographic_area),
    Geographic_area = ifelse(Geographic_area == "United Kingdom", "UK", Geographic_area),
    Geographic_area = ifelse(
      grepl("China", Geographic_area, ignore.case = TRUE),
      "China",
      Geographic_area
    ),
    Geographic_area = ifelse(
      grepl("Venezuela", Geographic_area, ignore.case = TRUE),
      "Venezuela",
      Geographic_area
    ),
    Geographic_area = ifelse(
      grepl("Bolivia", Geographic_area, ignore.case = TRUE),
      "Bolivia",
      Geographic_area
    ),
    Geographic_area = ifelse(Geographic_area == "Czechia", "Czech Republic", Geographic_area),
    Geographic_area = ifelse(
      grepl("Netherlands", Geographic_area, ignore.case = TRUE),
      "Netherlands",
      Geographic_area
    ),
    Geographic_area = ifelse(
      grepl("Palestine", Geographic_area, ignore.case = TRUE),
      "Palestine",
      Geographic_area
    ),
    Geographic_area = ifelse(
      grepl("Moldova", Geographic_area, ignore.case = TRUE),
      "Moldova",
      Geographic_area
    ),
    Geographic_area = ifelse(Geographic_area == "Viet Nam", "Vietnam", Geographic_area),
    Geographic_area = ifelse(
      Geographic_area == "Iran (Islamic Republic of)",
      "Iran",
      Geographic_area
    ),
    Geographic_area = ifelse(Geographic_area == "Curaçao", "Curacao", Geographic_area),
    Geographic_area = ifelse(Geographic_area == "Türkiye", "Turkey", Geographic_area)
  )

row_to_duplicate <- df_analysis %>%
  filter(Geographic_area == "Middle East and North Africa")
duplicated_rows <- rep(list(row_to_duplicate), 7)
df_analysis <- do.call(rbind, c(list(df_analysis), duplicated_rows))

rows_to_change <-
  df_analysis$Geographic_area %in% "Middle East and North Africa"
df_analysis$Geographic_area[rows_to_change] <-
  c("Morocco",
    "Algeria",
    "Tunisia",
    "Libya",
    "Egypt",
    "Sudan",
    "Yemen",
    "Syria")

row_to_duplicate2 <- df_analysis %>%
  filter(Geographic_area == "West and Central Africa")
duplicated_rows2 <- rep(list(row_to_duplicate2), 7)
df_analysis <- do.call(rbind, c(list(df_analysis), duplicated_rows2))

rows_to_change2 <-
  df_analysis$Geographic_area %in% "West and Central Africa"
df_analysis$Geographic_area[rows_to_change2] <-
  c(
    "Mauritania",
    "Niger",
    "Nigeria",
    "Chad",
    "Central African Republic",
    "Cameroon",
    "Ghana",
    "Cote d'Ivoire"
  )
countries <- map_data("world") %>%
  filter(region != "Antarctica")

row_to_duplicate3 <- df_analysis %>%
  filter(Geographic_area == "Eastern and Southern Africa")
duplicated_rows3 <- rep(list(row_to_duplicate2), 8)
df_analysis <- do.call(rbind, c(list(df_analysis), duplicated_rows3))

rows_to_change3 <-
  df_analysis$Geographic_area %in% "West and Central Africa"
df_analysis$Geographic_area[rows_to_change3] <-
  c(
    "Ethiopia",
    "Somalia",
    "Kenya",
    "South Sudan",
    "Tanzania",
    "Mozambique",
    "Uganda",
    "Zambia",
    "Malawi"
  )

#przygotowanie ostatecznej ramki do stworzenia mapy
countries <- map_data("world") %>%
  filter(region != "Antarctica")

countries_analysis <- countries %>%
  left_join(df_analysis, by = c("region" = "Geographic_area"))

#tworzenie mapy
world_map <- ggplot() +
  geom_polygon(data = countries, aes(x = long, y = lat, group = group)) +
  coord_fixed()

map <- world_map +
  geom_polygon(data = countries_analysis, aes(
    x = long,
    y = lat,
    group = group,
    fill = Difference
  )) +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(title = "Gender Disparities in Educational Attainment",
       subtitle = "Point Percentage Difference [male - female]",
       fill = "Difference") +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.20),
    legend.margin = margin(
      t = 0,
      r = 0,
      b = 10,
      l = 0
    ),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
#tworzenie interaktywnej mapy
interactive_map <- ggplotly(map) %>%
  layout(
    title = list(text = "<b>Gender Disparities in Educational Attainment</b>", x = 0.5),
    margin = list(t = 120),
    annotations = list(
      text = "<b>Point Percentage Difference [male - female]</b>",
      showarrow = FALSE,
      x = 0.5,
      y = 1.06,
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      font = list(size = 14)
    )
  )


  