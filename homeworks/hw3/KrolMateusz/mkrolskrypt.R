library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(tidyr)

wrld <- map_data("world") %>% filter(long <= 180)

co2 <- read.csv("C:\\Users\\11mat\\Desktop\\P_Data_Extract_From_World_Development_Indicators\\plik.csv")

co2 <- co2 %>%
  rename("region" = "Country.Name") %>%
  select(c(region, X2020..YR2020.)) %>%
  naniar::replace_with_na(replace = list(X2020..YR2020. = ".."))

# Standaryzacja danych - wiele nazw państw się nie zgadza

co2$region[co2$region == "Bahamas, The"] <- "Bahamas"
co2$region[co2$region == "Russian Federation"] <- "Russia"
co2$region[co2$region == "United States"] <- "USA"
co2$region[co2$region == "Cote d'Ivoire"] <- "Ivory Coast"
co2$region[co2$region == "Iran, Islamic Rep."] <- "Iran"
co2$region[co2$region == "Turkiye"] <- "Turkey"
co2$region[co2$region == "Venezuela, RB"] <- "Venezuela"
co2$region[co2$region == "Yemen, Rep."] <- "Yemen"
co2$region[co2$region == "Kyrgyz Republic"] <- "Kyrgyzstan"
co2$region[co2$region == "Korea, Rep."] <- "South Korea"
co2$region[co2$region == "Korea, Dem. People's Rep."] <- "North Korea"
co2$region[co2$region == "Lao PDR"] <- "Laos"
co2$region[co2$region == "Gambia, The"] <- "Gambia"
co2$region[co2$region == "Viet Nam"] <- "Vietnam"
co2$region[co2$region == "United Kingdom"] <- "UK"
co2$region[co2$region == "Czechia"] <- "Czech Republic"
co2$region[co2$region == "Slovak Republic"] <- "Slovakia"
co2$region[co2$region == "Syrian Arab Republic"] <- "Syria"
co2$region[co2$region == "Egypt, Arab Rep."] <- "Egypt"
co2$region[co2$region == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
co2$region[co2$region == "Congo, Rep."] <- "Republic of Congo"

tmp <- wrld %>% left_join(co2, by = "region")

tmp <- tmp %>%
  mutate(X2020..YR2020. = as.numeric(X2020..YR2020.)) %>%
  rename("Emisje" = "X2020..YR2020.")

katar <- data.frame(
  long = c(51.20, -135),
  lat = c(25.20, -60),
  names = c("×", "(×) Największa wartość:\nKatar (31.73)"),
  stringsAsFactors = FALSE)

ggplot() + 
  geom_polygon(data = tmp, aes(x=long, y = lat, group = group, fill = Emisje), 
               color = "black") + 
  coord_map("mercator") + 
  theme_bw() + 
ggtitle(expression(Emisje~CO^{2}~na~mieszkańca~w~roku~2020)) +
  guides(fill = guide_legend(title = "Roczne emisje CO^2\n(tony per capita)")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        legend.position = "bottom", legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 25, hjust = 0.5)) + 
  scale_fill_distiller(palette = "YlOrBr", limits = c(0, 22)) + 
  xlab(" ") + ylab(" ") + 
  geom_point(data = katar, aes(x = long, y = lat), 
             color = c("lightgoldenrodyellow", "white"), size = 3.5) +
  geom_text(data = katar, aes(x = long, y = lat, label = names), color = "black")
