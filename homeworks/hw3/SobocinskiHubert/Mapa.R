library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(readxl)

data <- read_excel("C:/studia/2rok/TWD/homeworkMapa/Dataset.xlsx")

europeCountries <- c("Albania",
                     "Latvia",
                     "Andorra",
                     "Liechtenstein",
                     "Armenia",
                     "Lithuania",
                     "Austria",
                     "Luxembourg",
                     "Azerbaijan",
                     "Malta",
                     "Belarus",
                     "Moldova",
                     "Belgium",
                     "Monaco",
                     "Bosnia and Herzegovina",
                     "Montenegro",
                     "Bulgaria",
                     "Netherlands",
                     "Croatia",
                     "Norway",
                     "Cyprus",
                     "Poland",
                     "Czech Republic",
                     "Portugal",
                     "Denmark",
                     "Romania",
                     "Estonia",
                     "Russia",
                     "Finland",
                     "San Marino",
                     "Republic of Macedonia",
                     "Serbia",
                     "France",
                     "Slovakia",
                     "Georgia",
                     "Slovenia",
                     "Germany",
                     "Spain",
                     "Greece",
                     "Sweden",
                     "Hungary",
                     "Sweden",
                     "Iceland",
                     "Switzerland",
                     "Ireland",
                     "Turkey",
                     "Italy",
                     "Ukraine",
                     "Kosovo",
                     "United Kingdom")

data <- data %>% 
  select(c("Country", "Average IQ")) %>% 
  filter(Country %in% europeCountries) %>% 
  mutate(Country = ifelse(Country == "United Kingdom", "UK", Country)) %>% 
  mutate(Country = ifelse(Country == "Republic of Macedonia", "North Macedonia", Country))

data <- as.data.frame(data)
world <- map_data("world")
europe <- data %>% 
  left_join(world, by = c("Country" = "region")) %>% 
  mutate(`Average IQ` = as.numeric(`Average IQ`))

europeIQ <- europe %>% 
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = `Average IQ`)) +
  coord_map("mollweide", xlim = c(-10, 38), ylim = c(35, 70)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(
    low = "grey",
    high = "blue") +
  labs(title = "Average IQ in Europe",
       subtitle = "in 2023",
       caption = "Źródło: https://wisevoter.com/country-rankings/average-iq-by-country/") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 14, hjust = 0.5, vjust = 0.5)) 

europeIQ
ggsave("IQMap.png", width = 20, height = 20, units = "cm",bg="white")
