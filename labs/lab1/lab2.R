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

view(starwars_new)

# Określamy typy zmiennych:
# name -  jakościowa nominalna
# height - ilościowa ilorazowa
# mass - ilościowa ilorazowa
# hair_color - jakościowa nominalna
# skin_color - jakościowa nominalna
# birth_year - ilościowa przedziałowa
# sex - jakościowa binarna

# 1) Wybór wierszy i kolumn w dplyr


# a) wybór kolumn ---> select()
select(starwars,name,birth_year)

select(starwars,-birth_year)

wybierz <- c("name","birth_year")
select(starwars,wybierz)

wybierz2 <- c("name","birth_year","lol")
select(starwars,any_of(wybierz2))


# b) wybór wierszy ---> filter()
filter(starwars_new,skin_color=="blue")

filter(starwars_new,skin_color=="blue",sex=="male")

filter(starwars_new,skin_color=="blue" | skin_color=="red")

filter(starwars_new,skin_color %in% c("blue","red"))

# 2) pipes %>% (skrót Ctrl + Shift + m)


# Zadanie 1
# Używając funkcji z pakietu dplyr() wybierz te postacie, których gatunek to Droid, 
# a ich wysokość jest większa niż 100.
filter(starwars,species=="Droid", height>100)

# Zadanie 2 
# Używając funkcji z pakietu dplyr() wybierz te postacie, które nie mają określonego koloru włosów.
filter(starwars,is.na(hair_color) | hair_color=="none")

# c) sortowanie wierszy ---> arrange()
starwars %>% arrange(height)
starwars %>% arrange(-height)
starwars %>% arrange(desc(height))

starwars %>% filter(hair_color=='blond') %>% arrange(height)
# Zadanie 3
# Używając funkcji z pakietu dplyr() wybierz postać o największej masie.
starwars %>% arrange(-mass) %>% head(1)
starwars %>% top_n(1,mass)


# d) transformacja zmiennych ---> mutate()
starwars %>% mutate(height=height/100)

starwars %>% mutate(height_m=height/100)


# e) transformacja zmiennych ---> transmute()
starwars %>% transmute(height_m=height/100)

# Zadanie 4
# Używając funkcji z pakietu dplyr() wylicz wskaźnik BMI (kg/m^2) i wskaż postać, która ma największy wskaźnik.
starwars %>% mutate(BMI=mass/((height/100)^2)) %>% top_n(1,BMI) %>% select(name,BMI)

# f) kolejność kolumn ---> relocate()
starwars %>% relocate(height:skin_color,.after = birth_year)

starwars %>% relocate(birth_year:sex,.before = mass)

starwars %>% relocate(where(is.numeric), .after=where(is.character))
# g) dyskretyzacja ---> ifelse(), case_when()

starwars %>% mutate(species_new = ifelse(species=="Human","Human","Other"))

starwars %>% mutate(species_new = case_when(species=="Human"~ "Human",species=="Droid" ~ "Droid",TRUE ~ "Other")) %>% 
  select(name,species,species_new) %>% tail()
# h) funkcje agregujące ---> summarise(), n(), mean, median, min, max, sum, sd, quantile
starwars %>% summarise(mean_height=mean(height,na.rm = TRUE),min_mass=min(mass,na.rm = TRUE),cnt=n())

# i) grupowanie ---> group_by() + summarise()
starwars %>% group_by(species) %>% summarise(mean_height=mean(height,na.rm=TRUE))

# 3) Przekształcenie ramki danych w tidyr
library(tidyr) # https://tidyr.tidyverse.org

# j) pivot_longer()

?relig_income
relig_income %>% pivot_longer(!religion,names_to = 'income',values_to = 'count') 

# k) pivot_wider()

?fish_encounters

fish_encounters %>% pivot_wider(names_from = station,values_from = seen)

# 4) Praca z faktorami (szczególnie w wizualizacji)
library(forcats) # https://forcats.tidyverse.org
library(ggplot2) # https://ggplot2.tidyverse.org



# l) kolejność poziomów ---> fct_infreq()
starwars %>% ggplot(aes(x=fct_infreq(eye_color))) + geom_bar() + coord_flip()
# m) scalanie poziomów ---> fct_lump()

starwars %>% ggplot(aes(x=fct_lump(eye_color,n=5))) + geom_bar() + coord_flip()

# n) kolejność poziomów na podstawie innej zmiennej ---> fct_reorder()


# 4) Praca z stringami
# Zaawansowane: https://stringi.gagolewski.com
library(stringr) # https://stringr.tidyverse.org

x <- paste0(letters[1:5], "=", 1:5, "__", letters[6:10], "=", 6:10)

# o) podział stringów ---> str_split()


# p) usunięcie/zastąpienie znaków ---> str_remove(), str_replace()


# 5) https://www.tidyverse.org/packages
# Bezpieczne wczytywanie danych w R https://readr.tidyverse.org
