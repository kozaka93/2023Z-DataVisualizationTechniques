###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) Prowadzący
# Anna Kozak/Mateusz Krzyziński/Mikołaj Spytek/Katarzyna Woźnica
# Kontakt: MS Teams lub mail
# anna.kozak@pw.edu.pl/mateusz.krzyzinski.stud@pw.edu.pl/mikolaj.spytek.stud@pw.edu.pl/katarzyna.woznica.dokt@pw.edu.pl

## 1) Sposób pracy na zajęciach laboratoryjnych
# a) pracujemy w R (większość semestru) i Python
# b) pracujemy na przygotowanych plikach, które będą na repozytorium przedmiotu
# c) podczas zajęć prowadzący będzie wprowadzał zagdanienia, a następnie będzie rozwiązywanie zadań w celu utrwalenia wiadomości
# d) kolejna porcja materiału będzie omawiana jeżeli większość grupy wykona zadane zadanie 
# e) wszelkie pytania czy to związane z kodem, pracą domową czy kwestie teoretyczne proszę śmiało zgłaszać prowadzącemu 

## 2) Materiały
# Repozytorium na GitHub
# https://github.com/kozaka93/2023Z-DataVisualizationTechniques

## 3) Jak działa GitHub?
# Jak zgłosić pracę domową/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 4) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[1,] # wiersz 1
mtcars[1] # kolumna 1
mtcars[,1] # kolumna jako wektor 

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1,1] 

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10,2:3]
mtcars[1:10,c(2,7)]


# Jak wybieramy kolumny po nazwach? 

mtcars[, "mpg"]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?


# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)
ncol(mtcars)
nrow(mtcars)

# 2. Jakie są typy zmiennych?

typeof(mtcars)
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

unique(mtcars[,'cyl'])
mtcars$cyl # do wyciągania tylko jednej kolumny

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars$drat[mtcars$cyl == 4])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

table(mtcars$am)

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$cyl)


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

#1
employees$login[employees$name == "John"]
proton(action = "login", login="johnins")

#2
for (i in top1000passwords) {
  proton(action = "login", login="johnins", password=i)
}

#3

install.packages("dplyr")
library(dplyr)

employees$login[employees$surname == "Pietraszko"]

logs %>% filter(login == "slap") %>% group_by(host) %>% summarise( number = n() ) %>% arrange(desc(number)) 
# 194.29.178.16 
proton(action = "server", host="194.29.178.16")

#4



## 5) Umieszczamy rozwiązanie na repozytorium.