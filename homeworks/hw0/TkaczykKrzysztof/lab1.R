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
tail(mtcars)

dim(mtcars)
str(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[c(2,3),2:3]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]
# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]
head(mtcars,10)[,2:3]#xd

# Jak wybieramy kolumny po nazwach? 
mtcars[,c("cyl", "disp")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[,c("am","wt","mpg")]

# Jak wybierać jedną kolumnę?
mtcars[,c("mpg")]
mtcars$mpg
mtcars["mpg"]

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

# 2. Jakie są typy zmiennych?

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
length(unique(mtcars$cyl))
unique(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4,]$drat)

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)?
table(mtcars$am)
?table

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg,mtcars$hp)
?plot


# Zmienna "cyl" - barplot

barplot(table(mtcars$cyl))


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

#Problem 1
str(employees)
employees[employees$name == "John" & employees$surname == "Insecure", c("login")]

#johnins

#Problem 2
str(top1000passwords)
for (password in top1000passwords) {
  proton(action = "login", login="johnins", password=password)
}

#Problem 3
employees[employees$surname == "Pietraszko",]$login

mostlogged = max(table(logs[logs$login=="slap",]$host))
table(logs[logs$login=="slap",]$host)[table(logs[logs$login=="slap",]$host)==mostlogged]

proton(action ="server", host="194.29.178.16")

#Problem 4
str(bash_history)

grep()

## 5) Umieszczamy rozwiązanie na repozytorium.