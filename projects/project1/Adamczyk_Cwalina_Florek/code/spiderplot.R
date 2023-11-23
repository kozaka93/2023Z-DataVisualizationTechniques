# Spider Plot - by Jan Cwalina
library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(forcats)

data2 <- read.csv("./TWD/Projekt_1_food/prc_fsc_idx__custom_8184163_linear.csv.gz")
d2 <- data2 %>% select(5:9) %>%
  mutate(Year = substring(TIME_PERIOD, 1, 4),
         month = substring(TIME_PERIOD, 6, 8)) %>%
  filter(Year == c(2018:2022), geo != "TR") %>%
  filter(!(geo %in% c("EA19", "EA20", "EU28", "EU27_2020"))) %>%
  group_by(Year, geo) %>%
  summarise(mean = mean(OBS_VALUE, na.rm = TRUE)) %>% 
  mutate(geo = forcats::fct_reorder(geo, 1:length(geo)),
         Year = factor(Year, levels = c(2022, 2021, 2020, 2019, 2018)))

skroty_państw <- c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", 
                   "FI", "FR", "HR", "HU", "IE", "IS", "IT", "LT", "LU", "LV", 
                   "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", "SK", "UK")

# Wektor pełnych nazw państw odpowiadający skrótom
nazwy_państw <- c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic", 
                  "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", 
                  "Croatia", "Hungary", "Ireland", "Iceland", "Italy", "Lithuania", 
                  "Luxembourg", "Latvia", "Malta", "Netherlands", "Norway", "Poland", 
                  "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "United Kingdom")

# Tworzenie ramki danych
ramka_danych <- data.frame(
  geo = skroty_państw,
  nazwa = nazwy_państw
)
table(d2$Year)
d2 <- cbind(d2,n = c(1:153))
d22 <- inner_join(d2,ramka_danych, by = "geo")
d22 %>% ggplot(aes(
  x = geo,
  y = mean,
  group = Year,
  color = Year)) +
  expand_limits(y = c(0,80)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_polygon(alpha = 0) +
  geom_point() +
  scale_color_manual(values = c("#d62728",
                                         "#ff7f0e",
                                         "#2ca02c",
                                         "#1f77b4",
                                         "#9467cf")) +
                                           coord_polar() +
  labs(title = "Mean value of food-related products prices in years 2018-2022",x = "Country",y = "Value") + 
  theme(legend.position = "right") + theme(panel.background = element_rect(fill = "#E8FFEF"),
                                           panel.grid = element_line(color = "grey"))
######################################################### MUR JANKA #########################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

