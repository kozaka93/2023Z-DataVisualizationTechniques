library(dplyr)
library(maps)
library(ggplot2)
library(countrycode)

# Load data
procent <- as.data.frame(read.csv("dane_affordable.csv"))

# Function to encode country names
get_continent <- function(data) {
  iso_codes <- countrycode(data$Entity, "country.name", "iso3c")
  continent_info <- countrycode(iso_codes, "iso3c", "continent")
  return(data %>% mutate(Continent = continent_info))
}

# Hard-coded names of countries that countrycode did not encode
procent <- procent %>%
  mutate(
    Entity = case_when(
      Entity == "United States" ~ "USA",
      Entity == "United Kingdom" ~ "UK",
      Entity == "Czechia" ~ "Czech Republic",
      Entity == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
      TRUE ~ Entity
    )
  )

# Join data with country data and encode country names
world_map <- map_data("world")
mapped_data <- procent %>%
  right_join(world_map, by = c("Entity" = "region"))
mapped_data <- get_continent(mapped_data)

names(mapped_data)[4] <- "wartosc"

# Filter data so it contains World but Africa with smaller alpha
filtered_mapped_data <- mapped_data %>%
  filter(Continent != "Africa" & !is.na(Continent))
africa_mapped_data <- mapped_data %>%
  filter(Continent == "Africa" & !is.na(Continent))

# First plot - background that shows the entire World but Africa with smaller alpha
world_map <-
  ggplot(filtered_mapped_data,
         aes(
           x = long,
           y = lat,
           group = group,
           fill = wartosc
         )) +
  geom_polygon(color = "white",
               size = 0.1,
               alpha = 0.5) +
  
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
    legend.position = "bottom",
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      vjust = 1
    ),
    plot.subtitle = element_text(
      size = 10,
      hjust = 0.5,
      vjust = 1
    )
  ) +
  labs(title = "Share of population that cannot afford a calorie-sufficient diet", subtitle =
         "Data from 2017")

# Focus plot - Africa
africa_map <-
  ggplot(africa_mapped_data,
         aes(
           x = long,
           y = lat,
           group = group,
           fill = wartosc
         )) +
  geom_polygon(color = "white", size = 0.1) +
  xlim(min(filtered_mapped_data$long),
       max(filtered_mapped_data$long)) +
  ylim(min(filtered_mapped_data$lat),
       max(filtered_mapped_data$lat)) +
  scale_fill_gradient2(
    low = "lightblue",
    mid = "#FFD4D4",
    high = "brown",
    midpoint = 10,
    name = "%",
    na.value = "#808080"
  ) +
  theme_void() +
  theme(legend.position = "none")

# Join plots together
plot_map <- world_map +
  annotation_custom(ggplotGrob(africa_map))

ggsave("plot_map.png", plot = plot_map, dpi = 300)
