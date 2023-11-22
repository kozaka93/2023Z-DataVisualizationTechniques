# Celem ponizszego skrytpu jest stworzenie wizualizacji w formie kartogramu 
# w odwzorowaniu Mollweidego ukazującej współczynnik śmiertelności związanej
# z otyłością w poszczególnych państwach świata.


# Wczytanie bibliotek -----------------------------------------------------

library(dplyr)
library(tidyverse)
library(openxlsx)


# Wczytanie i agregacja danych --------------------------------------------

setwd(dir = "C:\\TWD\\Prace domowe\\Praca domowa 3\\RowickiMikolaj")

# Wczytuję dane dotyczące śmierci związanych z otyłością z poniższej strony:
# https://ourworldindata.org/grapher/deaths-due-to-obesity?tab=table&fbclid=IwAR34dFXIR5fuWf_ro7xnpgdHhTjg1-pRaVNgcN3mOM5Eo16wd1_2ATPaMkk.

deaths_obesity <- read.csv("data/deaths-due-to-obesity.csv")

# Wczytuję dane dotyczące populacji poszczególnych państw z poniższej strony:
# https://www.kaggle.com/datasets/iamsouravbanerjee/world-population-dataset/data

population <- read.csv("data/world_population.csv")

deaths_obesity_in_2015_per_country <-deaths_obesity %>% 
  
# Wizualizację przedstawię dla danych z 2015 roku.

filter(Year == 2015) %>% 
rename(CCA3 = Code) %>% 
inner_join(population, by = 'CCA3' ) %>% 
select(c('Entity', 'Deaths', 'X2015.Population')) %>% 
rename(c(Country = Entity, Population = X2015.Population)) %>% 

# Współczynnik śmierci będę podawał jako liczbę zanotowanych przypadków śmierci
# w przeliczeniu na 10000 mieszkańców danego państwa.

mutate(Deaths_due_to_obesity_per_ten_thousand_people = (Deaths/Population) * 10000) %>% 
select(Country, Deaths_due_to_obesity_per_ten_thousand_people)

rownames(deaths_obesity_in_2015_per_country) <- NULL

# Stworzę zmienną typu factor przypisującą wartościom współczynnika śmiertelności
# przynależność do odpowiedniego przedziału. Dzieki temu będę mógł na kartogramie
# zamiast ciągłej zmiennej zaprezentować dyskretne grupy. 

deaths_obesity_in_2015_per_country <- deaths_obesity_in_2015_per_country %>% 
  mutate(Discrete_ratio_of_deaths_due_to_obesity = 
           factor(case_when(
             Deaths_due_to_obesity_per_ten_thousand_people <= 30 & Deaths_due_to_obesity_per_ten_thousand_people > 25 ~ '1',
             Deaths_due_to_obesity_per_ten_thousand_people <= 25  & Deaths_due_to_obesity_per_ten_thousand_people > 20 ~ '2',
             Deaths_due_to_obesity_per_ten_thousand_people <= 20 & Deaths_due_to_obesity_per_ten_thousand_people > 15 ~ '3',
             Deaths_due_to_obesity_per_ten_thousand_people <= 15 & Deaths_due_to_obesity_per_ten_thousand_people > 10 ~ '4',
             Deaths_due_to_obesity_per_ten_thousand_people <= 10 & Deaths_due_to_obesity_per_ten_thousand_people > 5 ~ '5',
             Deaths_due_to_obesity_per_ten_thousand_people <= 5 ~ '6'),
           labels = c('(25, 30]', '(20, 25]', '(15, 20]', '(10, 15]','(5, 10]', '(0, 5]') )) 
levels(deaths_obesity_in_2015_per_country$Discrete_ratio_of_deaths_due_to_obesity)
# zapisuję ramkę do pliku i ręcznie poprawiam niektóre z nazw krajów, aby móc skutecznie wykonac inner_join

# write.xlsx(deaths_obesity_in_2015_per_country, file = "data/deaths_obesity_in_2015_per_country.xlsx")

# wczytuję ramkę z poprawionymi ręcznie nazwami krajów

deaths_obesity_in_2015_per_country <- read.xlsx("data/deaths_obesity_in_2015_per_country.xlsx")


# Tworzenie mapy ----------------------------------------------------------

# Tworzę mapę, wykorzystując funkcję map_data z pakietu tidyverse

world_map = map_data("world") %>% 
  filter(! long > 180)

countries <- world_map %>% 
  distinct(region) %>% 
  rename(Country = region) %>% 
  left_join(deaths_obesity_in_2015_per_country, by = 'Country')

p <- countries %>% 
  ggplot(aes(fill = Discrete_ratio_of_deaths_due_to_obesity, map_id = Country)) +
  geom_map(map = world_map, color = "black", linewidth = 0.01 ) +
  expand_limits(x = c(-185,185), y = world_map$lat) +
  coord_map("moll") +
  scale_fill_manual(values = c("#450B0C","#921B07","#E64008","#FF9109","#FFC107","#fff323"), 
                    na.value = "grey") +
  theme_minimal() +
  labs(title = "Death rate of obesity-related diseases per country in 2015",
       subtitle = 'Bulgaria and Ukraine have the highest death rate related to obesity in the world',
       fill = "Deaths per 10000 people") +
  theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 10)),
        plot.subtitle = element_text(hjust = 0.5,  size = 10, margin = margin(b = 10)),
        axis.title.x = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = c(0.14, 0.5),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        line = element_line(linewidth = 0.5, colour = "grey"),
        legend.background = element_rect(fill = "white", color = "grey")
  )

ggsave("map_death_reath_of_obesity_related_diseases_per_country.pdf", plot = p, width = 12,height = 8)
