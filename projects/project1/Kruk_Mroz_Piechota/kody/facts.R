library(tidyr)
library(dplyr)
library(cowplot)
library(ggplot2)
library(ggthemes)


food_insecurity <- read.csv("food_insecurity.csv")

#1
calories <- read.csv("kalorieAfrykaAmerykaSwiat.csv") %>% 
  select(Area, Element, Year, Value) %>% 
  filter(Element == "Food supply (kcal/capita/day)") %>% 
  filter(Year == "2021") %>% 
  arrange(desc(-Value)) %>% 
  head(1)

#2
food_insecurity <- read.csv("food_insecurity.csv") %>% 
  filter(Year %in% c(2016,2021)) %>% 
  filter(Item == "Number of severely food insecure people (million) (annual value)")

africaPopulation2016 <- population %>% 
  filter(Year == "2016") %>% 
  select(Value)
africaPopulation2021 <- population %>% 
  filter(Year == "2021") %>% 
  select(Value)

percentOfPeople2016 <- (100 * 1000000 * food_insecurity[food_insecurity$Year == 2016, "Value"]) / (africaPopulation2016 * 1000)
percentOfPeople2021 <- (100 * 1000000 * food_insecurity[food_insecurity$Year == 2021, "Value"]) / (africaPopulation2021 * 1000)

#3
waterWorld <- read.csv("waterWorld.csv") %>% 
  select(Area, Item, Unit, Value, Year) %>% 
  filter(Year == "2020") %>% 
  arrange(desc(-Value)) %>% 
  head(1)



