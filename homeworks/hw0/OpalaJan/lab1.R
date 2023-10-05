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

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[c(2,3), c(1,3)]
mtcars[1,] # 1 wiersz
mtcars[ ,] # wszystko
mtcars[, 1] # wektor
mtcars[,1, drop=FALSE] # niewektor
mtcars[1:10, c(2,3)]

# Jak wybieramy kolumny po nazwach? 

mtcars[1:10, c("mpg", "cyl")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?

mtcars$wt # jako wektor
mtcars["wt"] # jako ramka danych
var <- "wt"
mtcars[var]

# Uwaga na przecinek i wybór kolumn poprzez indeksy

mtcars[1,2] # zwraca komórkę
mtcars[c(1,2)] # zwraca dwie pierwsze kolumny

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)
nrow(mtcars)
ncol(mtcars)

# 2. Jakie są typy zmiennych?

# numeric, string, factor (niski/wysoki)

str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

unique(mtcars)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars$drat[mtcars$cyl==4])
mean(mtcars[mtcars$cyl == 4, "drat"]) # 2 równoważne metody

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

unique(mtcars$am)
table(mtcars$am)

# Prosty wykres

hist(mtcars$am)

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$cyl)
barplot(table(mtcars$cyl))
hist(mtcars$cyl) # zły dla wartości dyskretnych


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
employees[employees$name == "John", ]
proton(action = "login", login = "johnins", password = "123456")
tab <- table(top1000passwords)
barplot(tab)
for (x in top1000passwords){
  proton(action = "login", login = "johnins", password = x)
}
logs[logs$login == "pietraszko",]

employees$login[employees$surname=="Pietraszko"]
logs[logs$login == "slap", ]
sort(table(logs[logs$login == "slap", "host"]))
proton(action = "server", host = "194.29.178.16 ")

## 5) Umieszczamy rozwiązanie na repozytorium.
