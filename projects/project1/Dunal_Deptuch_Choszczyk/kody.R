library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)
library(fmsb)
library(showtext)
library(tidytext)


# wczytywanie ramek
arabica_data <- read.csv('arabica_data_cleaned.csv')
robusta_data <- read.csv('robusta_data_cleaned.csv')
coffee_analysis <- read.csv('simplified_coffee.csv')
prices_years <-  read.csv('prices_historical.csv')
exports_crop <- read.csv('exports_crop.csv')
total_production <- read.csv('total_production.csv')
top_countries <- read.csv("TopCountries.csv")
coffee_producing_countries <- read.csv("coffee-producing-countries-2023.csv")

names(robusta_data) <-names(arabica_data)
coffee_data <-  rbind(arabica_data, robusta_data)


# czcionki
font_import() 
loadfonts() 
showtext_auto()
font_add("Bakso Sapi", "/Users/Karolina/Library/Fonts/BaksoSapi-4BmlB.ttf")


# cups per capita
top_countries %>%
  separate(X.Country.CupsPerCapita.PoundsPerCapita, 
           c("X", "Country", "CupsPerCapita", "LbsPerCapita"), sep = ";") %>%
  filter(!Country == "") %>% 
  arrange(desc(as.numeric(CupsPerCapita))) %>% 
  mutate(Country = factor(Country, levels = rev(unique(Country)))) %>%
  ggplot(aes(x = Country, y = as.numeric(CupsPerCapita))) +
  geom_col(width = 0.8, fill = "#7f5539") +
  labs(title = "10 countries with largest daily coffee consumption",
       y = "Cups Per Capita",
       x = "") +
  scale_y_continuous(breaks=seq(0, 4, 0.5)) +
  theme(plot.title = element_text(face = "bold", size = 27, hjust = 0.5, 
                                  color = "white", family = "Bakso Sapi"),
        axis.text.x = element_text(face = "bold", size = 19, color = "white", 
                                   family = "Bakso Sapi"),
        axis.title.x =  element_text(face = "bold", size = 19, color = "white", 
                                     family = "Bakso Sapi"),
        #axis.text.y = element_text(face = "bold", size = 15, color = "white", 
                                   #family = "Bakso Sapi"),
        # zakomentowane, aby łatwiej było wkleić flagi na osi OY
        # normalnie na osi OY znajdowałyby się z tym kodem nazwy państw
        axis.text.y = element_blank(), 
        panel.grid = element_line(color = "white", size = 0.3,
                                        linetype = "dotted"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent")) +
  geom_text(aes(label = CupsPerCapita), size = 8, color = "white",
            family = "Bakso Sapi", hjust=-0.2) +
  coord_flip() 


ggsave("plot.png", path = "/Users/Karolina/Desktop/uni", device = png, height=7,width=12, limitsize = FALSE) 


# total cup points
top_7_countries <- c('Brazil', 'Vietnam', 'Colombia', 'Indonesia', 'Ethiopia', 'India', 'Honduras')

top_7_countries_data <- coffee_data[coffee_data$Country.of.Origin %in% top_7_countries, ] %>% 
  filter(Total.Cup.Points != 0)
top_7_countries_data$Country.of.Origin <- factor(
  top_7_countries_data$Country.of.Origin,
  levels = rev(top_7_countries)
)

ggplot(top_7_countries_data, aes(
  x = Country.of.Origin,
  y = as.numeric(Total.Cup.Points))) +
  coord_flip() +
  labs(y = "Rating",
       x = 'Country of Origin') + 
  geom_violin(fill = "#b08968",
              color = "#b08968") + 
  scale_y_continuous(breaks = seq(65, 95, 5)) +
  labs(title = "Ratings of coffee from top 7 producers") +
  theme(panel.grid = element_line(colour = "white",linetype = "dotted"),
        panel.background = element_rect(fill = "transparent", color = 'transparent'),
        plot.background = element_rect(fill = "transparent", color = 'transparent'),
        plot.title = element_text(face = "bold", size = 25, hjust = 0.5,
                                  color = "white", family = "Bakso Sapi"),
        axis.text = element_text(color = "white", size = 15, family = "Bakso Sapi"),
        axis.title = element_text(color = "white", size = 18, hjust = 0.5, family = "Bakso Sapi")) +
  stat_summary(fun = 'mean',
               geom = 'point',
               color = '#845431',
               size = 3.5)

ggsave("plot1.png", path = "/Users/Karolina/Desktop/uni", device = png, height=8,width=12, limitsize = FALSE) 


# total production map
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  filter(!admin %in% c("Antarctica"))

coffee_production <- left_join(coffee_producing_countries, world, by = c("country" = "sovereignt"))

coffee_production_kg <- coffee_production %>% 
  mutate(CoffeeKg = CoffeePounds * 0.4536) %>% 
  mutate(CoffeeKg_log =
           factor(case_when(
             CoffeeKg <= 1000000 & CoffeeKg > 0 ~ '< 1mln',
             CoffeeKg <= 10000000 & CoffeeKg > 10 ~ '1 - 10mln',
             CoffeeKg <= 100000000 & CoffeeKg > 20 ~ '10 - 100mln',
             CoffeeKg <= 1000000000 & CoffeeKg > 20 ~ '100 - 1 000mln',
             CoffeeKg > 1000000000 ~ '> 1 000mln'),
             levels = c('> 1 000mln', '100 - 1 000mln', '10 - 100mln',  '1 - 10mln', '< 1mln')))


production_map <- ggplot() +
  
  geom_sf(data = world, fill = "#705d56", color = "#210f03", size = 0.5) +
  
  geom_sf(data = coffee_production_kg, aes(fill = CoffeeKg_log, geometry = geometry), color = "#342B25", size = 0.5) +
  
  scale_fill_manual(values = c("#621b00","#96582a","#ac7e5c", "#d2a975","#e1cfbb"),
                    name = "Production (kg)") +
  
  labs(title = "Global coffee production in 2023") +
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
        plot.title = element_text(hjust = 0.5, size = 18, family = "Bakso Sapi", color = "white"),
        plot.caption = element_text(hjust = 1, size = 10, family = "Bakso Sapi", color = "white"),
        legend.position = "right",
        legend.title = element_text(family = "Bakso Sapi", color = "white"),
        legend.text = element_text(family = "Bakso Sapi", color = "white"),
        axis.text = element_text(family = "Bakso Sapi", color = "white"))

production_map

ggsave("plot2.png", path = "/Users/Karolina/Desktop/uni", device = png, height=8,width=12, limitsize = FALSE) 






