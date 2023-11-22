library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
# data from eurostat
data <- read_csv("D:/Studia/Semestr 3/TWD/changes-in-food-prices---data-analysis/prc_fsc_idx__custom_8184163_linear.csv.gz")
data  <- data.frame(data)
#--------------------------------------------------------------------
# map of products, which price increase the most
# filter data
map1 <- data %>%
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  filter(Year == 2017) %>% 
  group_by(coicop, geo) %>% 
  summarise(cen2017 = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
# filter data
map2 <- data %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  filter(Year == 2022) %>% 
  group_by(coicop, geo) %>% 
  summarise(cen2022 = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
# merge two data frames
merged_map <- merge(x = map1, y = map2, on = (c(map1$coicop, map1$geo) == c(map2$coicop, map2$geo)))
# removing redundant values and calculating inflation
merged_map <- merged_map %>% 
  filter(!(geo %in% c("EA19", "EA20", "EU27_2020"))) %>% 
  mutate(Inflation = ((cen2022 -cen2017)/cen2017 )* 100) %>% 
  group_by(geo) %>% 
  slice(which.max(Inflation)) %>% 
  select(coicop, geo, Inflation)
# dictionary of country names
dict <- data.frame(
  SKR = c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU",
          "IE", "IS", "IT", "LT", "LU" ,"LV", "MT" ,"NL", "NO" ,"PL", "PT", "RO", "SE", "SI", "SK",
          "TR"),
  Full = c("Austria", "Belgium", "Bulgaria", "Switzerland", "Czech Republic", "Cyprus", "Germany","Denmark", "Estonia",
           "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Iceland", "Italy",
           "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia",
           "Turkey")
)
# merging with dictionary
merged_map <- merged_map %>%
  left_join(dict, by=c("geo" = "SKR")) %>% 
  select(coicop, Full, Inflation)
# dictionary of food names
food_dict <- data.frame(
  SKR = c("CP01154", "CP01174", "CP01147", "CP0121",  "CP01124", "CP0115",  "CP0114"),
  Full_food = c("Other edible oils", "Potatoes", "Eggs", "Coffe, tea, cocoa", "Poultry", "Oil, fats", "Milk, cheese")
)
# merging with dictionary
merged_map <- merged_map %>% 
  left_join(food_dict, by=c("coicop" = "SKR")) %>% 
  select(Full_food, Inflation, Full)
#--------------------------------------------------------------------
map_data_europe <- map_data("world") #map of wolrd and merging on region
dane_mapa <- map_data_europe %>%
  left_join(merged_map, by = c("region" = "Full"))
#map
ggplot(dane_mapa) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Full_food), color = "azure4") +
  scale_fill_viridis_d(  na.value = "grey") +  
  scale_fill_manual(values = c("#440154FF", "#46337EFF", "#365C8DFF", "#277F8EFF",  "#4AC16DFF", "#94DBAA", "#238B45","#1FA187FF"))+
  labs(title = "Food category whose price increased the most compared to the country", fill = "Type of food") +
  theme_minimal() +
  theme(legend.position = "right", axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(), panel.grid = element_blank() ) +
  coord_cartesian(xlim = c(-30, 45), ylim = c(35, 70))
