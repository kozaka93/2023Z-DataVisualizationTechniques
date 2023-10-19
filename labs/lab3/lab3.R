###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 3            ###
###########################################

library(SmarterPoland)
?countries
View(countries)

# Zadanie 0.
# Określ typy zmiennych:
# country -
# birth.rate - 
# death.rate - 
# population - 
# continent - 

# ggplot2
library(ggplot2)

## 1. Główna funkcja ggplot2 ----> ggplot()

## 2. Mapowania ----> aes()

## 3. Operator '+'

# Stosujemy podobne formatowanie jak w dplyr

# Łączenie przez pipes %>% (skrót Ctrl + Shift + m)

## 4. Wykresy do badania rozkładu jednej zmiennej (ilościowej)

# a) histogram, geom_histogram() lub stat_bin()

# b) jądrowy estymator gęstości, geom_density() lub stat_density()

# c) boxplot, geom_boxplot() lub stat_boxplot()

# Zadanie 1
# Narysuj histogram wskaźnika urodzeń dla państw położonych w Europie,
# zadbaj o czytelność wykresu.


# Zadanie 2
# Narysuj wykres gęstości wskaźnika śmierci, zadbaj o czytelność wykresu.


## 5. Wykresy do badania rozkładu jednej zmiennej (jakościowej)

# wykres słupkowy, geom_bar(), geom_col()


## 6. Wykresy do badania rozkładu dwóch zmiennych (dwie zmienne ilościowe - numeryczne)

# a) dwie zmienne numeryczne bez porządku (np. bez zależności od czasu)

# wykres punktowy

# b) jedna zmienna ilościowa, jedna jakościowa


spotify_2023 <- read.csv("https://raw.githubusercontent.com/kozaka93/2023Z-DataVisualizationTechniques/main/homeworks/hw1/spotify-2023.csv")


# Zadanie 3
# Zbadaj rozkład taneczności piosenek w poszczególnych miesiącach.



# Zadanie 4
# Ile wydano piosenek w poszczególnych latach?



# Zadanie 5
# Jak wygląda zależnośc pomiędzy energią a tanecznością utworów? Dodaj podział 
# ze względu na skalę (mode).


