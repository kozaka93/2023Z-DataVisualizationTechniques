library(dplyr)
library(ggplot2)
library(ggrepel)

data <- read.csv("data1.csv")
continents <- read.csv("continents2.csv")
data <- data %>% filter(Year == 2016)

data_for_plotting <-
  inner_join(data, continents, by = c("Entity" = "name"))

data_for_plotting <- data_for_plotting %>%
  filter(
    !is.na(Entity) & !is.na(Code) &
      !is.na(Indicator.Alcohol..consumers.past.12.months.......Sex.Male) &
      !is.na(
        Indicator.Alcohol..consumers.past.12.months.......Sex.Female
      ) &
      !is.na(Population..historical.estimates.) &
      !is.na(sub.region)
  ) %>%
  select(
    Entity,
    Code,
    Indicator.Alcohol..consumers.past.12.months.......Sex.Male,
    Indicator.Alcohol..consumers.past.12.months.......Sex.Female,
    Population..historical.estimates.,
    region
  )


countries_labeled = c(
  "Albania",
  "Argentina",
  "Armenia",
  "Australia",
  "Austria",
  "Belarus",
  "Belgium",
  "Bosnia and Herzegovina",
  "Brazil",
  "Canada",
  "Chad",
  "China",
  "Croatia",
  "Czech Republic",
  "Egypt",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Iceland",
  "India",
  "Indonesia",
  "Iran",
  "Iraq",
  "Ireland",
  "Israel",
  "Italy",
  "Japan",
  "Kazakhstan",
  "Kenya",
  "Liechtenstein",
  "Luxembourg",
  "Madagascar",
  "Malaysia",
  "Maldives",
  "Mexico",
  "Monaco",
  "Mongolia",
  "New Zealand",
  "Niger",
  "Nigeria",
  "North Macedonia",
  "Norway",
  "Oman",
  "Pakistan",
  "Peru",
  "Poland",
  "Portugal",
  "Russia",
  "San Marino",
  "South Sudan",
  "Switzerland",
  "Syria",
  "Taiwan",
  "Thailand",
  "Tunisia",
  "Turkey",
  "Turkmenistan",
  "Uganda",
  "Ukraine",
  "United Arab Emirates",
  "United Kingdom",
  "United States",
  "Uruguay",
  "Vatican City",
  "Vietnam",
  "Zambia",
  "Zimbabwe",
  "Libya"
)
p <- ggplot(
  data_for_plotting,
  aes(
    x = Indicator.Alcohol..consumers.past.12.months.......Sex.Male,
    y = Indicator.Alcohol..consumers.past.12.months.......Sex.Female,
    label = Entity,
    color = region,
    size = Population..historical.estimates. / 1000000
  )
) +
  geom_point(aes(size = 2), color = "white") +  # Black border around points
  geom_point() +
  geom_text(
    size = 0,
    nudge_x = -0.5,
    nudge_y = -0.5,
    color = "white"
  ) +
  labs(
    color = "Region",
    size = "Population in \nmillions(2016)",
    title = "Percentage of men vs. percentage of women who drank alcohol in the last year (year 2016)",
    x = "Percentage of men who drank alcohol in the last year (year 2016)",
    y = "Percentage of women who drank alcohol in the last year (year 2016)"
    
  #  "Death rate because of excessive alcohol use depending on Country and Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = 14, family = "Georgia", color = "white"),
    axis.title = element_text(
      size = 13,
      hjust = 0.5,
      family = "Georgia",
      color = "white"
    ),
    legend.text = element_text(size = 15, family = "Georgia", color = "white"),
    legend.title = element_text(size = 15, family = "Georgia", color = "white")
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "white",
    alpha = 0.5
  ) +
  geom_text_repel(
    data = filter(data_for_plotting, Entity %in% countries_labeled),
    family = "Georgia",
    size = 3.5,
    box.padding = 0.5,
    max.overlaps = Inf,
    color = "white"
  ) + 
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color =  alpha("grey", 0.5)), 
    panel.grid.minor = element_line(color =  alpha("grey", 0.7)), 
    panel.background = element_rect(fill = "transparent", color = NA_character_),
    plot.background = element_rect(fill = "transparent", color = NA_character_)
  ) +
  scale_size(guide = guide_legend(override.aes = list(colour = "white", fill = "white")))+
  scale_color_manual(
    values = c(
      "Europe" = "#0059FF",
      "Africa" = "#FFAA00",
      "Oceania" = "#FF0000",
      "Americas" = "#8A2BE2",
      "Asia" = "#00FF66"
    )
  )

ggsave(
  plot = p,
  filename = "scatterplot.png",
  bg = "transparent"
)


