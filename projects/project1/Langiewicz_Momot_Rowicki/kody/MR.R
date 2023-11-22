library(dplyr)
library(tidyr)
library(ggplot2)
library(devtools)
library(ggthemr)
library(tidyverse)
library(ggthemes)
library(openxlsx)
library(RColorBrewer)



# Wczytanie ramek danych 

burger_king_menu <- read.csv("data/burger-king-menu.csv")

deaths_obesity <- read.csv("data/deaths-due-to-obesity.csv")

quick_service_restaurants_us <- read.csv("data/number-of-quick-service-restaurants-in-the-us-2011_2022.csv")

Nutrition_Value_Dataset <- read.csv("data/Nutrition_Value_Dataset.csv")

obesity <- read.csv("data/obesity-percents.csv")

dietary_habits <- read.csv("data/Nutrition__Physical_Activity__and_Obesity.csv")

df <- dietary_habits %>% 
  group_by(Question) %>% 
  distinct(Question)

population <- read.csv("data/world_population.csv")

types_of_mortality_vs_fried_food_consumption_frequency <- read.csv("data/Types of Mortality vs. Fried Food consumption Frequency-mean.csv")

fried_food_consumption_and_mortality <- read.csv("data/Fried food consumption and mortality_ prospective cohort study.csv")

frequency_of_visiting_fast_food <- read.csv("data/average-fast-food-consumption-per-week-in-2016-2018.csv", sep = ";")



# How many times a week do you eat fast food? -----------------------------

frequency_of_visiting_fast_food_modified <- frequency_of_visiting_fast_food %>% 
  mutate(X2016 = X2016/100, X2017 = X2017/100, X2018 = X2018/100) %>% 
  pivot_longer(cols = c(X2016, X2017, X2018), 
               names_to = "Year", 
               values_to = "PercentageShare") %>%
  mutate(Year = as.factor(gsub("X", "", Year)))

ggthemr('light')

frequency_of_visiting_fast_food_modified %>% 
  ggplot(aes(y = Answer, x = PercentageShare, fill = Year)) +
  geom_bar(stat = "identity",
           position = position_dodge2(width = 0.5, preserve = "single"),
           width = 0.5) +
  labs(title = "How often do you eat fast food?", x = "Share of respondents (%)", y = "Answer") 



# Tworzenie mapy ----------------------------------------------------------

world_map = map_data("world") %>% 
  filter(! long > 180)



# Obesity among adults in the population ----------------------------------

obesity_in_2015_per_country <- obesity %>% 
  filter(Sex == 'Both sexes', Year == 2015) %>% 
  select(Country, Obesity_percent) %>% 
  remove_rownames()

# write.xlsx(obesity_in_2015_per_country, file = "data/obesity_in_2015_per_country.xlsx")

# wczytuję ramkę z poprawionymi ręcznie nazwami krajów

obesity_in_2015_per_country <- read.xlsx("data/obesity_in_2015_per_country.xlsx")

obesity_in_2015_per_country <- obesity_in_2015_per_country %>% 
  mutate(Discrete_obesity_percent = 
         factor(case_when(
           Obesity_percent <= 10 ~ '(0, 10]',
           Obesity_percent <= 20 & Obesity_percent > 10 ~ '(10, 20]',
           Obesity_percent <= 30 & Obesity_percent > 20 ~ '(20, 30]',
           Obesity_percent <= 40 & Obesity_percent > 30 ~ '(30, 40]',
           Obesity_percent <= 50 & Obesity_percent > 40 ~ '(40, 50]',
           Obesity_percent > 50 ~ '(50, 60]')))


countries <- world_map %>% 
  distinct(region) %>% 
  rowid_to_column() %>% 
  rename(Country = region) %>% 
  left_join(obesity_in_2015_per_country, by = 'Country') 
           

map_obesity_percent <- countries %>% 
  ggplot(aes(fill = Discrete_obesity_percent, map_id = Country)) +
  geom_map(map = world_map) +
  expand_limits(x = c(-190,190), y = world_map$lat) +
  coord_map("moll") +
  scale_fill_manual(values = c( "#fff323", "#FFBB13", "#FF9109", "#C83807", "#921B07", "#660000"),
                    na.value = "grey") +
  theme_minimal() +
  labs(fill = "Obesity among adults (‰)") +
  guides(fill = guide_legend(reverse = FALSE)) +
  theme(legend.background = element_rect(fill = "#18191C", colour = "#18191C"), 
        legend.text = element_text(color = "white"),
        plot.background = element_rect(fill = "#18191C", colour="#18191C"),
        legend.position = c(1, 1), legend.justification = c(1, 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        line = element_line(linewidth = 0.5, colour = "white")
        )

ggsave("plots/discrete_map_obesity_percent.pdf", plot = map_obesity_percent, width = 15, height = 10)



# Deaths due to obesity in proportion to population -----------------------

deaths_obesity_in_2015_per_country <-deaths_obesity %>% 
  filter(Year == 2015) %>% 
  rename(CCA3 = Code) %>% 
  inner_join(population, by = 'CCA3' ) %>% 
  select(c('Entity', 'Deaths', 'X2015.Population')) %>% 
  rename(c(Country = Entity, Population = X2015.Population)) %>% 
  mutate(Deaths_due_to_obesity_per_mille = (Deaths/Population) * 1000) %>% 
  select(Country, Deaths_due_to_obesity_per_mille) %>% 
  remove_rownames()

# write.xlsx(deaths_obesity_in_2015_per_country, file = "data/deaths_obesity_in_2015_per_country.xlsx")

# wczytuję ramkę z poprawionymi ręcznie nazwami krajów

deaths_obesity_in_2015_per_country <- read.xlsx("data/deaths_obesity_in_2015_per_country.xlsx")

deaths_obesity_in_2015_per_country <- deaths_obesity_in_2015_per_country %>% 
  mutate(Discrete_deaths_due_to_obesity_permille = 
           factor(case_when(
             Deaths_due_to_obesity_per_mille <= 3 & Deaths_due_to_obesity_per_mille > 2.5 ~ '(2.5, 3]',
             Deaths_due_to_obesity_per_mille <= 2.5  & Deaths_due_to_obesity_per_mille > 2 ~ '(2, 2.5]',
             Deaths_due_to_obesity_per_mille <= 2 & Deaths_due_to_obesity_per_mille > 1.5 ~ '(1.5, 2]',
             Deaths_due_to_obesity_per_mille <= 1.5 & Deaths_due_to_obesity_per_mille > 1 ~ '(1, 1.5]',
             Deaths_due_to_obesity_per_mille <= 1 & Deaths_due_to_obesity_per_mille > 0.5 ~ '(0.5, 1]',
             Deaths_due_to_obesity_per_mille <= 0.5 ~ '(0, 0.5]',
           )))

countries2 <- world_map %>% 
  distinct(region) %>% 
  rowid_to_column() %>% 
  rename(Country = region) %>% 
  left_join(deaths_obesity_in_2015_per_country, by = 'Country')


map_obesity_deaths_per_mille <- countries2 %>% 
  ggplot(aes(fill = Discrete_deaths_due_to_obesity_permille, map_id = Country)) +
  geom_map(map = world_map) +
  expand_limits(x = c(-190,190), y = world_map$lat) +
  coord_map("moll") +
  scale_fill_manual(values = c( "#660000","#921B07","#C83807","#FF9109","#FFBB13", "#fff323"), 
                    na.value = "grey") +
  theme_minimal() +
  guides(fill = guide_legend(reverse = FALSE)) +
  labs(fill = "Deaths due to obesity (‰)") +
  theme(legend.background = element_rect(fill = "#18191C", color = "#18191C"), 
        legend.text = element_text(color = "white"),
        plot.background = element_rect(fill = "#18191C", colour = "#18191C"),
        axis.title.x = element_blank(),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        line = element_line(linewidth = 0.5, colour = "white")
          )

ggsave("plots/map_obesity_deaths_per_mille.pdf", plot = map_obesity_deaths_per_mille, width = 15,height = 10)

