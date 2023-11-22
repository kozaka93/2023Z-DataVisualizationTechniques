library(dplyr)
library(ggplot2)
data <- read.csv("cause_of_deaths.csv")
data_1 <- read.csv("world_population.csv")
data_population <-
  data_1 %>% select(
    Country.Territory,
    Continent,
    X2020.Population,
    X2015.Population,
    X2010.Population,
    X2000.Population,
    X1990.Population
  )
data_alcohol <-
  data %>%
  filter(Country.Territory != "United States Virgin Islands") %>%
  filter(Country.Territory != "Saint Kitts and Nevis") %>%
  select(Country.Territory, Year, Alcohol.Use.Disorders)


data_population <- data_population %>%
  mutate(Mean_population_1990_2020 = rowMeans(
    select(
      .,
      X1990.Population,
      X2000.Population,
      X2010.Population,
      X2015.Population,
      X2020.Population
    ),
    na.rm = TRUE
  ))

data_alcohol <- data_alcohol %>%
  group_by(Country.Territory) %>%
  summarise(mean_death = mean(Alcohol.Use.Disorders))

data_population_alcohol_deaths <-
  merge(data_alcohol, data_population, by = "Country.Territory", all = TRUE)

data_population_alcohol_deaths <-
  data_population_alcohol_deaths %>%
  mutate(deaths_per_100000 = mean_death / Mean_population_1990_2020 * 100000) %>%
  arrange(desc(deaths_per_100000))

data_top_20 = data_population_alcohol_deaths %>%
  head(20)

p2 <-
  ggplot(data_top_20, aes(
    x = reorder(Country.Territory, deaths_per_100000),
    y = deaths_per_100000
  )) +
  #800000
  geom_col(fill = "#ffcc00") +
  geom_text(
    aes(label = sprintf("%.2f", deaths_per_100000)),
    vjust = 0.3,
    hjust = 1,
    family = "Georgia", 
    size=4.5
  ) +
  labs(#title = "Yearly deaths per 100,000",
    x = "Country/Territory",
    y = "Deaths per 100,000")+
  theme_minimal() +
  theme(
    title = element_blank(),
    axis.text = element_text(family = "Georgia", color="white", size = 15),
    axis.title.y  = element_blank()
  ) +
  theme(
    panel.grid.major = element_line(color =  alpha("grey", 1), linetype = "dotted", size = 0.7), 
    panel.grid.minor = element_line(color =  alpha("grey", 1), linetype = "dotted", size = 0.7), 
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA_character_),
    plot.background = element_rect(fill = "transparent", color = NA_character_)
  ) +
  coord_flip()
p2

ggsave(
  plot = p2,
  filename = "barplot.png",
  bg = "transparent"
)

