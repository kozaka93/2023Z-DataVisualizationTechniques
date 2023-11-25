library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(extrafont)

## link do danych: 
## https://www.kaggle.com/datasets/carlosaguayo/usa-hospitals

setwd("C:\\Users\\HP\\R\\TWD_semestr3")

hospital <- read.csv("Hospitals.csv")

states <- map_data("state")
nazwy <- c("HOSPITALS, PSYCHIATRIC (EXCEPT CONVALESCENT)", "HOSPITALS, SUBSTANCE ABUSE", "CHILDREN'S HOSPITALS, SPECIALTY (EXCEPT PSYCHIATRIC, SUBSTANCE ABUSE)", 
           "REHABILITATION HOSPITALS (EXCEPT ALCOHOLISM, DRUG ADDICTION)", "CHILDREN'S HOSPITALS, GENERAL", "EXTENDED CARE HOSPITALS (EXCEPT MENTAL, SUBSTANCE ABUSE)", 
           "HOSPITALS, ADDICTION", "HOSPITALS, SPECIALTY (EXCEPT PSYCHIATRIC, SUBSTANCE ABUSE)" )

points <- hospital %>% 
  filter(STATUS == "OPEN") %>% 
  filter(COUNTRY == "USA") %>% 
  filter(NAICS_DESC %in% nazwy) %>% 
  filter(X >= -140) %>% 
  select(X, Y, NAICS_DESC) %>% 
  mutate(hospital_name = 
           case_when(NAICS_DESC == "HOSPITALS, PSYCHIATRIC (EXCEPT CONVALESCENT)" ~ "Psychiatric Hospital",
                     NAICS_DESC == "HOSPITALS, SUBSTANCE ABUSE" ~ "Substance Abuse Hospital",
                     NAICS_DESC == "CHILDREN'S HOSPITALS, SPECIALTY (EXCEPT PSYCHIATRIC, SUBSTANCE ABUSE)" ~ "Children's Hospital, Specialty (except psychiatric)",
                     NAICS_DESC == "REHABILITATION HOSPITALS (EXCEPT ALCOHOLISM, DRUG ADDICTION)" ~ "Rehabilitation Hospital (except alcoholizm and drug addiction)",
                     NAICS_DESC == "CHILDREN'S HOSPITALS, GENERAL" ~ "Children's Hospital (general)",
                     NAICS_DESC == "EXTENDED CARE HOSPITALS (EXCEPT MENTAL, SUBSTANCE ABUSE)" ~ "Extended Care Hospital(except mental and substance abuse)",
                     NAICS_DESC == "HOSPITALS, ADDICTION" ~ "Addiction Hospital",
                     NAICS_DESC == "HOSPITALS, SPECIALTY (EXCEPT PSYCHIATRIC, SUBSTANCE ABUSE)" ~ "Hospital, Specialty (except psychiatric and substance abuse)",))

palette <- c("#6ddf3c", "#15dde2", "#407e51", "#c722d5", "#124e98", "#f78c09", "#fcca04", "#960408")

ggplot() + 
  geom_polygon(data = states, aes(x=long, y = lat, group = group), fill = "#f1edd2", color = "black") +
  coord_fixed() +
  coord_map("mercator") +
  geom_point(data = points, aes(x = X, y = Y, color = hospital_name), size = 2) +
  theme_classic() +
  scale_color_manual(values = c("Psychiatric Hospital" = palette[1], "Substance Abuse Hospital" = palette[2], "Children's Hospital, Specialty (except psychiatric)" = palette[3], 
                                "Rehabilitation Hospital (except alcoholizm and drug addiction)" = palette[4], "Children's Hospital (general)" = palette[5], "Extended Care Hospital(except mental and substance abuse)" = palette[6], 
                                "Addiction Hospital" = palette[7], "Hospital, Specialty (except psychiatric and substance abuse)" = palette[8])) +
  theme(
    panel.background = element_rect(fill = "#dbd4b2"),
    plot.title = element_text(family = "Consolas", size = 20, hjust = 0.5),
    axis.title = element_text(family = "Consolas", size = 16),
    legend.title = element_text(family = "Consolas", size = 16),
    legend.text = element_text(family = "Consolas", size = 12)
  ) +
  labs(color = "Types of Hospitals", title = "Specialty Hospitals in the USA", x = "X", y = "Y")
  
