library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(mapdata)
library(rvest)

df <- read.csv("C:/PW/TWD/Projekty/Projekt 1/prc_fsc_idx_page_linear.csv")
View(df)
dim(df)
str(df)
summary(df)
colnames(df)

df <- df %>% 
  select(geo, TIME_PERIOD, OBS_VALUE)
View(df)

df1 <- df %>% 
  pivot_wider(names_from = TIME_PERIOD, values_from = OBS_VALUE) %>% 
  select(geo, "2018-01", "2023-01")

colnames(df1) <- c("Country", "Y2018", "Y2023")
View(df1)

df1$region <- c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus",
                       "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", 
                       "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland",
                       "Iceland", "Italy", "Lithuania", "Luxembourg", "Latvia",
                       "Malta", "Netherlands", "Norway", "Poland", "Portugal",
                       "Romania", "Sweden", "Slovenia", "Slovakia", "Turkey", "UK")

df2 <- df1 %>% 
  mutate(change = ((Y2023 - Y2018)/Y2018)*100)
View(df2)

world <- map_data("world")
Europe <- subset(world, region %in% c("Albania", "Andorra", "Austria",
                                                "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                                "Croatia", "Cyprus", "Czech Republic","Denmark","Estonia","Finland", 
                                                "France", "Germany", "Greece","Hungary","Iceland", 
                                                "Ireland", "Italy", "Kosovo", "Latvia","Liechtenstein", 
                                                "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                                "North Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                                                "Russia","San Marino","Serbia","Slovakia","Slovenia","Spain",
                                                "Sweden","Switzerland","Ukraine","UK","Vatican"))
Europe <- Europe %>% 
  left_join(df2)
View(Europe)

ggplot(Europe, aes(x= long, y= lat, group= group, fill=change)) +
  geom_polygon(color= "azure4") + xlim(-25,42) + ylim(35, 75) +
  scale_fill_fermenter(palette = 10,
                       direction = 1,
                       limits = c(0, 100),
                       breaks = c(0, 20, 40, 60, 80, 100),
                       labels = scales::percent_format(scale = 1),
                       na.value = "grey") + 
  theme_minimal() +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Change of food prices in Europe",
       subtitle = "Comparing 2018 and 2023",
       fill = "Change") +
  xlab("") + ylab("")
  

        