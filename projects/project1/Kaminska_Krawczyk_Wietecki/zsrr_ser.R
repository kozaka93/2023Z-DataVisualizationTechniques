library(tidyverse)
library(ggplot2)
library(dplyr)


setwd('~/Desktop/pw/3 sem/techniki_wizualizacji_danych/cheese/')


cheese <- read_csv("UNdata_Export1.csv")
cheese <- cheese %>% 
  select("field/0/__text", "field/1/__text", "field/2/__text",
         "field/3/__text", "field/4/__text", "field/5/__text")
colnames(cheese) <- c("Country_Area", "Element", "Year",
                      "Unit", "Value", "Aggregate")
cheese <- as_data_frame(cheese)

# cheese %>% 
#   filter(Year > 2009) %>% 
#   group_by(Country_Area) %>%
#   # summarise(sum_value = sum(Value), years = length(Year)) %>% 
#   summarise(mean_value = mean(Value)) %>% 
#   arrange(-mean_value) %>% 
#   print(n = 147)

options(scipen = 999)


# swiat

# cheese %>% 
#   filter(Country_Area == "World" & Year %in% c(1990, 1991, 1992, 1993, 1994, 1996)) %>% 
#   select(Year, Value) %>% 
#   transmute(Year, Value = Value/1000000) %>%
#   arrange(Year) -> prod_zsrr


prod_sera <- cheese %>% 
  filter(Country_Area == "World") %>% 
  select(Year, Value) %>% 
  transmute(Year, Value = Value/1000000) %>%
  arrange(Year) %>% 
  ggplot(aes(x = Year, y = Value)) +
  geom_line(color = "#ffc425", linewidth = 1.3) +
  # geom_ribbon(aes(ymin = 0, ymax = Value), fill = "#ffc425", alpha = 0.9) +
  labs(x = "Lata", y = "Miliony ton") +
  scale_y_continuous(limits = c(0,25),
                     expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA), 
    legend.background = element_rect(fill='transparent'), 
    legend.box.background = element_rect(fill='transparent'),
    axis.text = element_text(color="#540606",size = 9),
    axis.title = element_text(size=14,color="#540606"),
    title = element_text(size=20),
    panel.grid.major = element_line(color="darkgoldenrod",linewidth = 0.1),
    panel.grid.minor = element_line(color="darkgoldenrod",linewidth=0.1)) +
  scale_x_continuous(breaks=c(1960,1970,1980, 1990, 2000, 2010,2020),
                     limits = c(1960,2020))
prod_sera
  

ggsave('myplot.png',prod_sera, bg='transparent',units = "px", height=900,width=1300)


# kontynenty
cheese %>% 
  filter(Country_Area %in% c("Europe", "Northern America", "South America", "Asia", "Oceania", "Africa")) %>%
  # mutate(Kontynent = case_when(Country_Area == "Europe" ~ "Europa",
  #                         Country_Area == "Northern America" ~ "Ameryka Płn.",
  #                         Country_Area == "South America" ~ "Ameryka Płd.",
  #                         Country_Area == "Asia" ~ "Azja",
  #                         Country_Area == "Oceania" ~ "Australia i Oc.",
  #                         Country_Area == "Africa" ~ "Afryka")) %>% 
  mutate(Kontynent = case_when(Country_Area == "Europe" ~ "Europa",
                               Country_Area == "Northern America" ~ "Ameryka Płn.",
                               TRUE ~ "Reszta")) %>% 
  group_by(Kontynent, Year) %>% 
  summarise(Value = sum(Value)) %>% 
  select(Kontynent, Year, Value) %>% 
  transmute(Kontynent, Year, Value = Value/1000000) %>%
  arrange(Year) %>% 
  # transmute(Kontynent = factor(Kontynent, c("Europa","Azja","Australia i Oc.","Ameryka Płd."
  #                                           ,"Ameryka Płn.","Afryka")),
  #           Year, Value) %>% 
  ggplot(aes(x = Year, y = Value, color = Kontynent)) +
    geom_line() +
  labs(x = "Lata", y = "Miliony ton", color = NULL) +
  scale_y_continuous(limits = c(0,13),
                     expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA), 
    legend.background = element_rect(fill='transparent', color = "#FFFAEB"), 
    legend.box.background = element_rect(fill='transparent'),
    legend.key = element_blank(),
    axis.text = element_text(color="#540606",size = 9),
    axis.title = element_text(size=14,color="#540606"),
    title = element_text(size=20),
    panel.grid.major = element_line(color="darkgoldenrod",linewidth = 0.1),
    panel.grid.minor = element_line(color="darkgoldenrod",linewidth=0.1),
    legend.text = element_text(color = "#540606")) +
  scale_x_continuous(breaks=c(1960,1970,1980, 1990, 2000, 2010,2020),
                     limits = c(1960,2020))+
  # scale_color_manual(values=c("#cc6600","#734d26","#660707","#472605", "#ffab0f", "#ebcd7a"))
  # scale_color_manual(values = c("#ffffd4","#fee391","#fec44f","#fe9929","#d95f0e","#993404"))
  scale_color_manual(values = c("#8c2d04","#cc4c02","#fe9929")) -> kont
kont
ggsave('kont.png',kont, bg='transparent',units = "px", height=900,width=1497)


# europa

cheese %>% 
  filter(Country_Area %in% c("Eastern Europe", "Northern Europe",
                             "Western Europe", "Southern Europe")) %>% 
  select(Country_Area, Year, Value) %>% 
  transmute(Rejon = case_when(Country_Area == "Eastern Europe" ~ "Europa Wsch.",
                              Country_Area == "Northern Europe" ~ "Europa Płn.",
                              Country_Area == "Western Europe" ~ "Europa Zach.",
                              Country_Area == "Southern Europe" ~ "Europa Płd."),
            Year, Value = Value/1000000) %>%
  arrange(Year) %>% 
  ggplot(aes(x = Year, y = Value, color = Rejon)) +
  geom_line() +
  labs(x = "Lata", y = "Miliony ton", color = NULL) +
  scale_y_continuous(limits = c(0,6),
                     expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA), 
    legend.background = element_rect(fill='transparent', color = "#FFFAEB"), 
    legend.box.background = element_rect(fill='transparent'),
    legend.key = element_blank(),
    axis.text = element_text(color="#540606",size = 9),
    axis.title = element_text(size=14,color="#540606"),
    title = element_text(size=20),
    panel.grid.major = element_line(color="darkgoldenrod",linewidth = 0.1),
    panel.grid.minor = element_line(color="darkgoldenrod",linewidth=0.1),
    legend.text = element_text(color = "#540606")) +
  scale_x_continuous(breaks=c(1960,1970,1980, 1990, 2000, 2010,2020),
                     limits = c(1960,2020))+
  scale_color_manual(values = c("#f03b20", "#fe9929", "#540606", "#993404")) -> eur

eur

ggsave('eur.png',eur, bg='transparent',units = "px", height=900,width=1497)



# # ILE PROCENT SPADLO Z ROKU NA ROK
# options(scipen = 999)
# cheese %>% 
#   filter(Country_Area == "World", Year %in% c("1990", "1991")) %>% 
#   select(Year, Value) -> A
# (A[A$Year == 1990,"Value"]-A[A$Year == 1991,"Value"])/A[A$Year == 1990,"Value"]
# 
# # europa i ameryka pln vs reszta
# cheese %>% 
#   filter(Country_Area %in% c("Europe", "Northern America", "South America", "Asia", "Oceania", "Africa")) %>% 
#   select(Country_Area, Year, Value) %>% 
#   transmute(Country_Area = case_when(Country_Area %in% c("Europe", "Northern America") ~
#                                                            "Europe and North America",
#                                      TRUE ~ "Rest"),
#             Year, Value = Value/1000000) %>% 
#   group_by(Country_Area, Year) %>% 
#   summarise(sum_value = sum(Value)) %>% 
#   arrange(Year) %>%
#   ggplot(aes(x = Year, y = sum_value, color = Country_Area)) +
#   geom_line() +
#   labs(x = "Lata", y = "Miliony ton", title = "Produkcja sera Europa i Ameryka Pln a reszta") +
#   scale_y_continuous(limits = c(0,20),
#                      expand = c(0,0))

# # europa vs reszta 
# cheese %>% 
  # filter(Country_Area %in% c("Europe", "Northern America", "South America", "Asia", "Oceania", "Africa")) %>%
#   select(Country_Area, Year, Value) %>% 
#   transmute(Country_Area = case_when(Country_Area %in% c("Europe") ~
#                                        "Europe",
#                                      TRUE ~ "Rest"),
#             Year, Value = Value/1000000) %>% 
#   group_by(Country_Area, Year) %>% 
#   summarise(sum_value = sum(Value)) %>% 
#   arrange(Year) %>%
#   ggplot(aes(x = Year, y = sum_value, color = Country_Area)) +
#   geom_line() +
#   labs(x = "Lata", y = "Miliony ton", title = "Produkcja sera Europa a reszta") +
#   scale_y_continuous(limits = c(0,15),
#                      expand = c(0,0))