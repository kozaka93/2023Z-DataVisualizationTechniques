# TWD project 1
# Loading libraries and data
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
# data from eurostat
data <- read_csv("D:/Studia/Semestr 3/TWD/changes-in-food-prices---data-analysis/prc_fsc_idx__custom_8184163_linear.csv.gz")
data  <- data.frame(data)
# Tasks:
#--------------------------------------------------------------------
# 1. Price changes between 2018 and 2022 for every half of year.
# inflation is measured in reference to the previous half-year.
# converting data for Poland
# filter and divio into half-years
dataPL <- data %>% 
  filter(geo == "PL") %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Month = month(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Polrocze = ifelse(Month > 6, "07-12","01-06")) %>% 
  filter(Year > 2017) %>% 
  filter(Year < 2023)
# average price in every half- year
x <-dataPL %>% 
  group_by(Year, Polrocze) %>% 
  summarise(average = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
# calculating inflation
x <- x  %>% 
  mutate(Inflacja = ((average - lag(average)) / lag(average)) * 100)
x[1, "Inflacja"] <- 0
# creating periods
x$period = paste(x$Year, x$Polrocze, sep = ".")
x <- x %>% 
  select(period, Inflacja)

# converting data for Germany
dataGER <- data %>% 
  filter(geo == "DE") %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Month = month(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Polrocze = ifelse(Month > 6, "07-12","01-06")) %>% 
  filter(Year > 2017) %>% 
  filter(Year < 2023)
GER <-dataGER %>% 
  group_by(Year, Polrocze) %>% 
  summarise(average = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
GER <- GER  %>% 
  mutate(Inflacja = ((average - lag(average)) / lag(average)) * 100)
GER[1, "Inflacja"] <- 0

GER$period = paste(GER$Year, GER$Polrocze, sep = ".")
GER <- GER %>% 
  select(period, Inflacja)
# full names
x$country <- 'Poland'
GER$country <- 'Germany'
# rbind of datas
POLGER <- rbind(x, GER)

# converting data for all EU countries
EU <- data %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Month = month(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Polrocze = ifelse(Month > 6,"07-12","01-06")) %>% 
  filter(Year > 2017) %>% 
  filter(Year < 2023)
EU <-EU %>% 
  group_by(Year, Polrocze) %>% 
  summarise(average = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
EU <- EU  %>% 
  mutate(Inflacja = ((average - lag(average)) / lag(average)) * 100)
EU[1, "Inflacja"] <- 0

EU$period = paste(EU$Year, EU$Polrocze, sep = ".")
EU <- EU %>% 
  select(period, Inflacja)
EU$country <- "Europe"
POLGEREU <- rbind(POLGER, EU)

# converting data for France
dataFR <- data %>% 
  filter(geo == "FR") %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Month = month(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Polrocze = ifelse(Month > 6,"07-12","01-06")) %>% 
  filter(Year > 2017) %>% 
  filter(Year < 2023)
FR <-dataFR %>% 
  group_by(Year, Polrocze) %>% 
  summarise(average = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
FR <- FR  %>% 
  mutate(Inflacja = ((average - lag(average)) / lag(average)) * 100)
FR[1, "Inflacja"] <- 0

FR$period = paste(FR$Year, FR$Polrocze, sep = ".")
FR <- FR %>% 
  select(period, Inflacja)
FR$country <- 'France'
ctr4 <- rbind(POLGEREU, FR)

# converting data for Hungary
dataHU <- data %>% 
  filter(geo == "HU") %>% 
  mutate(Year = year(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Month = month(as.Date(paste0(TIME_PERIOD, "-01")))) %>% 
  mutate(Polrocze = ifelse(Month > 6,"07-12","01-06")) %>% 
  filter(Year > 2017) %>% 
  filter(Year < 2023)
HU <-dataHU %>% 
  group_by(Year, Polrocze) %>% 
  summarise(average = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  ungroup()
HU <- HU  %>% 
  mutate(Inflacja = ((average - lag(average)) / lag(average)) * 100)
HU[1, "Inflacja"] <- 0

HU$period = paste(HU$Year, HU$Polrocze, sep = ".")
HU <- HU %>% 
  select(period, Inflacja)
HU$country <- 'Hungary'
ctr5 <- rbind(ctr4, HU)

#--------------------------------------------------------------------
# 2. map of products, which price increase the most
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


# Plots:
#--------------------------------------------------------------------
# lineplot from task 1
ggplot(ctr5, aes(x = period, y = Inflacja, color = country,group = country)) +
  geom_line() +
  geom_point() + 
  labs(title = "Inflation by every half a year", x = "Time", y = "% of inflation",
       fill = "Country or region") +
  scale_color_manual(values = c("Europe" = "blue", "Germany" = "black", "Poland" = "red", "France" = "purple", "Hungary" = "green3"))
#--------------------------------------------------------------------
# map from task 2
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


