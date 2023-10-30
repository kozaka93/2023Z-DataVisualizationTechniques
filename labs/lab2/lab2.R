###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 2            ###
###########################################

library(dplyr) # https://dplyr.tidyverse.org/

# Dane
starwars
starwars_new <- starwars[, c(1, 2, 3, 4, 5, 7, 8)]

# Informacje o zbiorze danych

str(starwars_new)

# Podgląd tabeli


# Określamy typy zmiennych:
# name - jakościowa, nominalna
# height - ilosciowa, ilorazowa
# mass - ilościowa. ilorazowa
# hair_color - jakościowa, nominalna
# skin_color - jakościowa, nominalna
# birth_year - ilościowa, przedziałowa
# sex - jakościowa, binarna ponoć

# 1) Wybór wierszy i kolumn w dplyr


# a) wybór kolumn ---> select()
select(starwars, name)
select(starwars, name, gender, mass)

select(starwars, -name)

select_cols <- c("name", "gender")
select(starwars, all_of(select_cols))

# b) wybór wierszy ---> filter()

filter(starwars, eye_color == "blue", hair_color == "blond")
# przecinek działa jak i


# 2) pipes %>% (skrót Ctrl + Shift + m)

starwars %>%
  filter(eye_color == "blue") %>%
  select(name) %>%
  head(10)

# Zadanie 1
# Używając funkcji z pakietu dplyr() wybierz te postacie, których gatunek to Droid, 
# a ich wysokość jest większa niż 100.

starwars %>%
  filter(species == "Droid", height > 100) %>%
  select(name, height, species)

# Zadanie 2 
# Używając funkcji z pakietu dplyr() wybierz te postacie, które nie mają określonego koloru włosów.

starwars %>%
  filter(is.na(hair_color) | hair_color %in% c("unknown", "none")) %>%
  select(name, hair_color)

# c) sortowanie wierszy ---> arrange()

arrange(starwars, height)

# Zadanie 3
# Używając funkcji z pakietu dplyr() wybierz postać o największej masie.

starwars %>%
  arrange(desc(mass)) %>%
  select(name, mass) %>%
  head(1)

# d) transformacja zmiennych ---> mutate()

starwars %>%
  mutate(height_m = height / 100) %>%
  select(name, height, height_m)

# e) transformacja zmiennych ---> transmute()


# Zadanie 4
# Używając funkcji z pakietu dplyr() wylicz wskaźnik BMI (kg/m^2) i wskaż postać, która ma największy wskaźnik.

starwars %>%
  mutate(bmi = mass / (height / 100)^2) %>%
  select(name, bmi) %>%
  arrange(desc(bmi)) %>%
  head(1)

# f) kolejność kolumn ---> relocate()

starwars %>%
  relocate(sex:homeworld, .before = height)

starwars %>%
  relocate(where(is.numeric), .after = where(is.character))

# g) dyskretyzacja ---> ifelse(), case_when()

starwars %>%
  mutate(species_new = ifelse(species == "Human", "Human", "Other")) %>%
  select(name, species, species_new)

starwars %>%
  mutate(species_new = case_when(
    species == "Human" ~ "Human",
    species == "Droid" ~ "Droid",
    TRUE ~ "Other"
  )) %>%
  select(name, species, species_new)


# h) funkcje agregujące ---> summarise(), n(), mean, median, min, max, sum, sd, quantile

starwars %>%
  summarise(mean_mass = mean(mass, na.rm = TRUE))

starwars %>%
  filter(hair_color == "blond") %>%
  summarise(n = n())

# i) grupowanie ---> group_by() + summarise()

starwars %>%
  group_by(hair_color) %>%
  summarise(n = n())


# 3) Przekształcenie ramki danych w tidyr
library(tidyr) # https://tidyr.tidyverse.org

# j) pivot_longer()

relig_income %>%
  pivot_longer(-religion, names_to = "income", values_to = "freq")

# k) pivot_wider()

fish_encounters %>%
  pivot_wider(names_from = "station", values_from = "seen", values_fill = 0)


# 4) Praca z faktorami (szczególnie w wizualizacji)
library(forcats) # https://forcats.tidyverse.org
library(ggplot2) # https://ggplot2.tidyverse.org


# l) kolejność poziomów ---> fct_infreq()

starwars %>%
  mutate(eye_color = fct_infreq(eye_color)) %>%
  ggplot(aes(x = fct_infreq(eye_color))) +
  geom_bar() +
  coord_flip()

# m) scalanie poziomów ---> fct_lump()

starwars %>%
  mutate(eye_color = fct_infreq(fct_lump(eye_color, n = 5))) %>%
  ggplot(aes(x = eye_color)) +
  geom_bar() +
  coord_flip()


# n) kolejność poziomów na podstawie innej zmiennej ---> fct_reorder()

iris %>%
  mutate(Species = fct_reorder(Species, Sepal.Length, median)) %>%
  ggplot(aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()

# 4) Praca z stringami
# Zaawansowane: https://stringi.gagolewski.com
library(stringr) # https://stringr.tidyverse.org

x <- paste0(letters[1:5], "=", 1:5, "__", letters[6:10], "=", 6:10)

# o) podział stringów ---> str_split()

str_split(x, "__", simplify = TRUE)

# p) usunięcie/zastąpienie znaków ---> str_remove(), str_replace()

str_remove(x, "__")
str_replace(x, "__", " ")

# 5) https://www.tidyverse.org/packages
# Bezpieczne wczytywanie danych w R https://readr.tidyverse.org
