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

mtcars[1,] # wiersz
mtcars[,1] # kolumna
mtcars[,1, drop=FALSE]

# Pierwszy wiersz, pierwsza kolumna?

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10, 2:3]

# Jak wybieramy kolumny po nazwach? 

mtcars[,"disp"]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[,c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?

mtcars$mpg # jako wektor
mtcars["mpg"] # jako jedno-kolumnowa ramke danych

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)
nrow(mtcars)
ncol(mtcars)

# 2. Jakie są typy zmiennych?

str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

unique(mtcars$cyl)
length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl==4,]$drat)

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

unique(mtcars$am)
table(mtcars$am)

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$cyl)
barplot(table(mtcars$cyl))

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
data("employees")
employees[employees$surname=="Insecure"]
proton(action = "login", login="johnins")
top1000passwords
proton(action = "login", login="johnins", password="ABC")
for(i in top1000passwords){
  proton(action = "login", login="johnins", password=i)
  
}

proton(action = "server", host="194.29.178.16")
data(logs)

sort(table(logs[logs$login=="slap",]$host))
a <-logs[logs$login=="slap",]
bash_history
head(bash_history)

?strsplit
b <- strsplit(bash_history, " ")
## 5) Umieszczamy rozwiązanie na repozytorium.