# Load required libraries
library(tidyverse)
library(countrycode)
library(ggplot2)

# Read data
population_growth <- read_csv("natural-population-growth.csv")
unaffordable_diet <- read_csv("1- share-healthy-diet-unaffordable.csv")

# Filter and rename columns for population_growth
population_growth <- population_growth %>% 
  filter(Year > 2016 & Year < 2022) %>% 
  rename("Natural growth" = "Natural growth rate - Sex: all - Age: all - Variant: estimates") %>% 
  select(-5)

# Filter and rename columns for unaffordable_diet
unaffordable_diet_2021 <- unaffordable_diet %>% 
  filter(Year == "2021") %>% 
  rename("Share" = `Share of the population who cannot afford a healthy diet`)

# Add Continent column using countrycode
population_growth$Continent <- countrycode(population_growth$Entity, origin = "country.name", destination = "continent")

# Join the two dataframes
pop_growth_vs_unaffordable_diet <- inner_join(population_growth,
                                              unaffordable_diet_2021, 
                                              by = c("Year", "Code"), 
                                              relationship = "many-to-many") %>% 
  filter(!is.na(Code), !is.na(Continent)) %>% 
  select("Natural growth", "Share", "Continent")

# Define colour palette
palette <- c("Africa" = "#FFD29D", "other\ncontinents" = "grey")
floor_min <- floor(min(pop_growth_vs_unaffordable_diet$`Natural growth`))
ceiling_max <- ceiling(max(pop_growth_vs_unaffordable_diet$`Natural growth`))

# Create the ggplot
plot <- ggplot(pop_growth_vs_unaffordable_diet, 
               aes(x = `Natural growth`, y = `Share`, 
                   colour = ifelse(Continent == "Africa", "Africa", 
                                   "other\ncontinents"))) + 
  geom_point(size = 5.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill="transparent"),
    plot.background = element_rect(fill="transparent", colour=NA),
    legend.background = element_rect(fill="transparent"),
    panel.grid.major = element_line(colour = "white", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(colour = "white", size = 10),
    axis.text.y = element_text(colour = "white", size = 10),
    axis.title.x = element_text(size = 20, colour = "white"),
    axis.ticks.x = element_line(colour = "white",linewidth = 0.3),
    axis.ticks.y = element_line(colour = "white", linewidth = 0.3),
    axis.text.y.left = element_text(size = 20, colour = "white"),
    axis.text.x.bottom = element_text(size = 20, colour = "white"),
    legend.key=element_rect(colour= NA, fill = NA),
    legend.position = c(0.87, 0.2)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_colour_manual(values = palette) +
  labs(colour = "") +
  ylab("") +
  xlab("Natural growth (%)") +
  guides(colour = guide_legend(override.aes = list(size = 17))) +
  theme(legend.text = element_text(size = 17, colour = "white")) +
  scale_x_continuous(breaks = round(seq(floor_min, ceiling_max, by = 1),1))+
  expand_limits(x = floor_min, y = 0)

print(plot)

# Save the plot
ggsave("natural-growth-plot.png", plot, bg = "transparent")

