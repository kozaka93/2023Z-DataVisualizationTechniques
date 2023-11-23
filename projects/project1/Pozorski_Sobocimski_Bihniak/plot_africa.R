library(readxl)
library(ggplot2)
library(dplyr)
library(countrycode)
library(class)

data <- as.data.frame(read_excel("dane_glod.xlsx"))

get_continent <- function(data) {
  iso_codes <- countrycode(data$region, "country.name", "iso3c")
  continent_info <- countrycode(iso_codes, "iso3c", "continent")
  return(data %>% mutate(Continent = continent_info))
}

colnames(data) <- data[3,]
data <- data[-c(1:3), ]
data <-
  data %>% mutate(`Continent` = countrycode(
    `Country ISO-3 Code`,
    origin = "iso3c",
    destination = "continent"
  ))

world_map <- map_data("world")
world_map <- get_continent(world_map) %>%
  filter(`Continent` == "Africa")

processed_data <- data %>%
  mutate(`Wasting` = as.numeric(`Wasting`),
         `Median Year` = as.numeric(`Median Year`)) %>%
  filter(`Median Year` >= 2005) %>%
  group_by(`Continent`, `Country Short Name`) %>%
  summarize(`MeanWaste` = mean(`Wasting`, na.rm = TRUE)) %>%
  filter(`Continent` == "Africa") %>%
  select(`Country` = `Country Short Name`, `MeanWaste`) %>%
  right_join(world_map, by = c("Country" = "region"))

plot_africa <-
  ggplot(processed_data,
         aes(
           x = `long`,
           y = `lat`,
           group = `group`,
           fill = `MeanWaste`
         )) +
  geom_polygon(color = "white",
               size = 0.1) +
  scale_fill_gradient2(
    low = "lightblue",
    mid = "#FFD4D4",
    high = "brown",
    midpoint = 10,
    name = "%",
    na.value = "#808080"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(
      face = "bold",
      size = 20,
      hjust = 0.5,
      vjust = 1
    ),
    plot.subtitle = element_text(
      size = 10,
      hjust = 0.5,
      vjust = 1
    )
  ) +
  labs(title = "Cachexia through starvation in African countries", subtitle = "Percentage of population suffering from cachexia through starvation. Years 2005-2020")
plot_africa

ggsave("plot_africa.png", plot = plot_africa, dpi = 300)
