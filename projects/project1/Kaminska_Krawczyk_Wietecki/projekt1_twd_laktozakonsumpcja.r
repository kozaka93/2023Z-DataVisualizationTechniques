install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

df1 <- read.csv("lactose-intolerance-by-country-2023.csv")
df2 <- read.csv("cheese-consumption-by-country-2023.csv")

lactose_intolerant_countries <- df1 %>%
  filter(!is.na(LactoseIntolerance.PercofPopulation.)) %>%
  select(country)

merged_data <- df2 %>%
  inner_join(df1, by = "country")

sorted_data <- merged_data %>%
  arrange(desc(CheeseConsumptionPerCapita)) %>% 
  select(country, CheeseConsumptionPerCapita, LactoseIntolerance.PercofPopulation., pop2023.x, region.x)

df <- sorted_data

country_mapping <- c(
  "France" = "Francja",
  "Germany" = "Niemcy",
  "Greece" = "Grecja",
  "Italy" = "Włochy",
  "Israel" = "Izrael",
  "Russia" = "Rosja",
  "Ukraine" = "Ukraina",
  "Australia" = "Australia",
  "Canada" = "Kanada"
)

continent_mapping <- c(
  "Europe" = "Europa",
  "Asia" = "Azja",
  "Oceania" = "Oceania",
  "North America" = "Ameryka Północna",
  "South America" = "Ameryka Południowa",
  "Africa" = "Afryka"
)

df <- df %>%
  mutate(
    country = ifelse(country %in% names(country_mapping), country_mapping[country], country),
    region.x = ifelse(region.x %in% names(continent_mapping), continent_mapping[region.x], region.x)
  )

custom_colors <- c("Azja" = "#f4c430", "Europa" = "#9e7110", "Afryka" = "yellow", "Ameryka Północna" = "darkorange", "Ameryka Południowa" = "#540606", "Oceania" = "brown")

plot4 <- ggplot(df, aes(x = LactoseIntolerance.PercofPopulation., y = CheeseConsumptionPerCapita * 0.454, color = region.x)) +
  geom_point(size = 6) + 
  geom_text(data = df %>% filter(country %in% c("Francja", "Niemcy", "Grecja", "Włochy", "Izrael", "Rosja", "Ukraina", "Australia", "Kanada")), aes(label = country), nudge_x = -9, nudge_y = 0, size = 4) +
  geom_vline(xintercept = 0, color = "darkgoldenrod", linewidth = 0.7) +
  labs(x = "Procent populacji z nietolerancją laktozy (%)", 
       y = "Ilość skonsumowanego sera per capita (kg)") +
  scale_color_manual(values = custom_colors, name = "") +  
  theme(
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    legend.background = element_rect(fill = 'transparent', color = 'transparent'),
    legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
    legend.position = "right",
    legend.text = element_text(color = "#540606", size = 11),
    axis.text = element_text(color = "#540606", size = 10),
    axis.title = element_text(size = 14, color = "#540606"),
    panel.grid.major = element_line(color = "darkgoldenrod", linewidth = 0.7),
    panel.grid.minor = element_line(color = "darkgoldenrod", linewidth = 0.2)
  )

print(plot4)
ggsave('imp.png', plot4, bg='transparent',units = "px",height=1500,width=2000)

