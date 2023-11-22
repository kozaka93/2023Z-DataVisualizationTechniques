library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv("cause_of_deaths.csv")
data_1 <- read.csv("world_population.csv")

data_population <- data_1 %>%
  select(
    Country.Territory,
    Continent,
    X2020.Population,
    X2015.Population,
    X2010.Population,
    X2000.Population,
    X1990.Population
  ) %>%
  filter(Continent == "Europe")

data_alcohol <- data %>%
  filter(Country.Territory != "United States Virgin Islands") %>%
  select(Country.Territory, Year, Alcohol.Use.Disorders)

data_population_alcohol_deaths <- merge(data_alcohol, data_population, by = "Country.Territory")

f <- data_population_alcohol_deaths %>%
  mutate(
    deaths_per_100000 = case_when(
      Year %in% c(1990, 1991, 1992, 1993, 1994, 1995) ~ Alcohol.Use.Disorders / X1990.Population * 100000,
      Year %in% c(1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005) ~ Alcohol.Use.Disorders / X2000.Population * 100000,
      Year %in% c(2006, 2007, 2008, 2009, 2010, 2011, 2012) ~ Alcohol.Use.Disorders / X2010.Population * 100000,
      Year %in% c(2013, 2014, 2015, 2016, 2017) ~ Alcohol.Use.Disorders / X2015.Population * 100000,
      Year %in% c(2018, 2019, 2020) ~ Alcohol.Use.Disorders / X2020.Population * 100000
    )
  ) %>%
  select(Country.Territory, Year, deaths_per_100000) %>%
  filter(
    Country.Territory %in% c(
      "Russia", "Belarus", "Lithuania", "Ukraine", "Estonia", "Poland",
      "Latvia", "Denmark", "Moldova", "Finland"
    )
  ) %>%
  arrange(desc(Year))

first_points <- f[!duplicated(f$Country.Territory), ]

p1 <- ggplot(
  f,
  aes(
    x = Year,
    y = deaths_per_100000,
    group = Country.Territory,
    color = Country.Territory,
    label = Country.Territory
  )
) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Slope Chart of the change in alcohol-related deaths by Country",
    x = "Year",
    y = "Number of deaths per 100,000",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(
    xintercept = unique(f$Year),
    linetype = "dotted",
    color = alpha("grey", 0.6)
  ) +
  geom_hline(
    yintercept = seq(0, 40, by = 5),
    linetype = "solid",
    color = alpha("grey", 0.4)
  ) +
  scale_x_continuous(
    breaks = c(1990, 1995, 2000,2005, 2010, 2015,2019),
    limits = c(1990, 2019),
    labels = c("1990", "1995", "2000","2005", "2010", "2015","2019")
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40),
    limits = c(0, 40),
    labels = c("0", "10", "20","30", "40")
  ) +
  theme(
    title = element_blank(),
    axis.text = element_text(family = "Georgia", color = "white", size = 15),
    axis.title.y = element_text(family = "Georgia", color = "white", size = 15),
    legend.text = element_text(family = "Georgia", color = "white", size = 15),
    legend.title = element_text(family = "Georgia", color = "white", size = 15),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA_character_),
    plot.background = element_rect(fill = "transparent", color = NA_character_),
    panel.grid = element_blank()
  )


ggsave(
  plot = p1,
  filename = "changingDeaths.png",
  bg = "transparent"
)

