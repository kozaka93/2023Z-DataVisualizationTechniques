library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(countrycode)
library(sf)

#Pobranie
df <- read.csv("./Data/consumptionpercapita10_21.csv", header = TRUE)

#Czyszczenie
names(df) <- df[1, ]
df <- df[-1, ]
df <- tibble(df)
df <- df %>%
  select(-`Data Source`) %>%
  pivot_longer(!c(`Beverage Types`, `Countries, territories and areas`), names_to = "Year", values_to = "litres") %>%
  mutate(Year = as.integer(Year)) %>%
  mutate(`Countries, territories and areas` = recode(`Countries, territories and areas`,
                                                     "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom"))
colnames(df)[c(1, 2)] <-  c("Country", "BeverageType")

# Get world coords
world <- ne_countries(scale = "small", returnclass = "sf")

# iso3n country code (numeric)
df <- df %>%
  mutate(Iso3 = countrycode(
    sourcevar = Country,
    origin = "country.name",
    destination = "iso3c"
  ))

# Join world cords with alcohol data and filter beer type in 2017
df <- world %>%
  select(geometry, name, iso_a3_eh) %>%
  left_join(df, by = c("iso_a3_eh" = "Iso3")) %>%
  filter(Year == 2017, BeverageType == "Beer")

map <- world %>%
  filter(admin != "Antarctica") %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf(color = "white") +
  geom_sf(data = df, aes(fill = litres)) +
  coord_sf(datum = NA) +
  labs(
    title = "World beer consumption",
    subtitle = "in litres of pure alcohol in 2017 per capita",
    fill = "Litres"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    legend.key.width = unit(1.5, "cm"),
    legend.title = element_text(hjust = 0.33),
    legend.text = element_text()
  ) +
  scale_fill_distiller(palette = 8, direction = 1)

ggsave("mapa.jpg", map, width = 10, height = 5, dpi = 300)



