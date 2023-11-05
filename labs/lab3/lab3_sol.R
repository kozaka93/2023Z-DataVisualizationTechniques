###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 3            ###
###########################################

install.packages("SmarterPoland")
library(SmarterPoland)
?countries
View(countries)

# Zadanie 0.
# Określ typy zmiennych:
# country - jakościowa, nominalna
# birth.rate - ilościowa, ilorazowa
# death.rate - ilościowa, ilorazowa
# population - ilościowa, ilorazowa
# continent - jakościowa, nominalna

# ggplot2
install.packages("ggplot2")
library(ggplot2)

## 1. Główna funkcja ggplot2 ----> ggplot()

ggplot()

ggplot(data = countries)

## 2. Mapowania ----> aes()

ggplot(data = countries, mapping = aes(x = birth.rate, y = death.rate))


## 3. Operator '+'

ggplot(data = countries, mapping = aes(x = birth.rate, y = death.rate)) +
  geom_point() 

# Stosujemy podobne formatowanie jak w dplyr

# Łączenie przez pipes %>% (skrót Ctrl + Shift + m)

library(dplyr)
countries %>% 
  ggplot(mapping = aes(x = birth.rate, y = death.rate)) +
  geom_point()

## 4. Wykresy do badania rozkładu jednej zmiennej (ilościowej)

# a) histogram, geom_histogram() lub stat_bin()
ggplot(countries, aes(x = population))

# zamiana formatu naukowego na liczby
options(scipen = 12)

ggplot(countries, aes(x = population)) +
  geom_histogram()

ggplot(countries, aes(x = population)) +
  geom_histogram(binwidth = 500000)

ggplot(countries, aes(x = population)) +
  geom_histogram(bins = 20)

ggplot(countries, aes(y = population)) +
  geom_histogram(bins = 20)

ggplot(countries, aes(x = population)) +
  geom_histogram() +
  ggtitle("Histogram dla zmiennej populacja", subtitle = "Rok 2013")

ggplot(countries, aes(x = population)) +
  geom_histogram() +
  labs(title = "Histogram dla zmiennej populacja",
       subtitle = "Rok 2013",
       x = "Populacja",
       y = "Częstość")

p <- ggplot(countries, aes(x = population))

p + geom_histogram()

# b) jądrowy estymator gęstości, geom_density() lub stat_density()

ggplot(countries, aes(x = population)) +
  geom_density()

ggplot(countries, aes(x = population)) +
  geom_density(adjust = 1/2)

ggplot(countries, aes(x = population)) +
  geom_density(adjust = 5)

ggplot(countries, aes(x = population, color = continent)) +
  geom_density() +
  xlim(0, 50000)

ggplot(countries, aes(x = population, fill = continent)) +
  geom_histogram()


ggplot(countries, aes(x = population, fill = continent)) +
  geom_density(alpha = 0.7) +
  xlim(0, 25000)

# c) boxplot, geom_boxplot() lub stat_boxplot()

ggplot(countries, aes(population)) +
  geom_boxplot()

ggplot(countries, aes(population)) +
  geom_boxplot(outlier.color = "red")

ggplot(countries, aes(population, color = continent)) +
  geom_boxplot()

ggplot(countries, aes(x = population, y = continent)) +
  geom_boxplot()

ggplot(countries, aes(x = population, y = continent)) +
  geom_boxplot() +
  coord_flip()

ggplot(countries, aes(y = population, x = continent)) +
  geom_boxplot() 

# Zadanie 1
# Narysuj histogram wskaźnika urodzeń dla państw położonych w Europie,
# zadbaj o czytelność wykresu.

countries %>% 
  filter(continent == "Europe") %>% 
  ggplot(aes(x = birth.rate)) +
  geom_histogram() +
  labs(title = "Histogram wskaźnika urodzeń dla państw położonych w Europie",
       x = "Wskaźnik urodzeń", 
       y = "Częstość")

# Zadanie 2
# Narysuj wykres gęstości wskaźnika śmierci, zadbaj o czytelność wykresu.

ggplot(countries, aes(x = death.rate)) +
  geom_density() +
  labs(title = "Wykres gęstości wskaźnika śmierci",
       x = "Wskaźnik śmierci", 
       y = "Gestość")


## 5. Wykresy do badania rozkładu jednej zmiennej (jakościowej)

# wykres słupkowy, geom_bar(), geom_col()

ggplot(countries, aes(continent)) + 
  geom_bar()

tmp <- data.frame(table(countries$continent))
tmp
ggplot(tmp, aes(x = Var1, y = Freq)) +
  geom_col()

## 6. Wykresy do badania rozkładu dwóch zmiennych (dwie zmienne ilościowe - numeryczne)

# a) dwie zmienne numeryczne bez porządku (np. bez zależności od czasu)

# wykres punktowy

ggplot(countries, aes(x = birth.rate, y = death.rate)) + 
  geom_point()


ggplot(countries, aes(x = birth.rate, y = death.rate, color = continent)) + 
  geom_point()

ggplot(countries, aes(x = birth.rate, y = death.rate, size = continent)) + 
  geom_point()

ggplot(countries, aes(x = birth.rate, y = death.rate, shape = continent)) + 
  geom_point()

ggplot(countries, aes(x = birth.rate, y = death.rate)) + 
  geom_point(color = "blue", size = 1)

# b) jedna zmienna ilościowa, jedna jakościowa

ggplot(countries, aes(x = birth.rate, y = continent)) +
  geom_violin()
ggplot(countries, aes(x = birth.rate, y = continent)) +
  geom_boxplot()


spotify_2023 <- read.csv("spotify-2023.csv")


# Zadanie 3
# Zbadaj rozkład taneczności piosenek w poszczególnych miesiącach.

spotify_2023 %>% 
  ggplot(aes(x = as.factor(released_month), y = `danceability_%`)) + 
  geom_boxplot() +
  scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
  labs(x = "Miesiąc",
       y = "Taneczność",
       title = "Tanczeność piosenek na przestrzeni miesięcy") +
  theme_bw()


# Zadanie 4
# Ile wydano piosenke w poszczególnych latach?

spotify_2023 %>% 
  ggplot(aes(x = released_year)) + 
  geom_bar() +
  labs(x = "Rok",
       y = "Liczba piosenek",
       title = "Liczba piosenek na przestrzeni lat") + 
  theme_bw() +
  scale_y_continuous(expand = c(0,0))


# Zadanie 5
# Jak wygląda zależnośc pomiędzy energią a tanecznością utworów? Dodaj podział 
# ze względu na skalę (mode).

spotify_2023 %>% 
  ggplot(aes(x = `danceability_%`, y = `energy_%`, color = mode)) + 
  geom_point() +
  labs(x = "Taneczność", 
       y = "Energia", 
       color = "Skala",
       title = "Zależność taneczności od energii w podziale na skale.") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_colour_manual(values = c("navyblue", "orange")) + 
  scale_x_continuous(limits = c(0,100)) + 
  scale_y_continuous(limits = c(0,100)) 
