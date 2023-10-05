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
#wiersze
mtcars[1,]
mtcars[c(1,4),]
mtcars[1:4,]

#kolumny
mtcars[,c(2,3)]

#oba
mtcars[1:5, c(2,5,7)]


# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,]
mtcars[,1] #wektor

mtcars[,1, drop=FALSE] #jednokolumnowa df

mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,c(2,3)]
mtcars[1:10,2:3]

# Jak wybieramy kolumny po nazwach? 
mtcars[1:10, c("mpg", "cyl")]
mtcars[1:10, c("cyl", "mpg")]

mtcars[1]

mtcars["cyl"]
mtcars[c("cyl", "mpg")]


# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$wt
mtcars["wt"]

var <- "wt"
mtcars[var]

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)
nrow(mtcars) #wiersze
ncol(mtcars) #kolumny

# 2. Jakie są typy zmiennych?

#numeric (num), string (char), factor (fct)

#jak sprawdzic? - str() - structure
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

unique(mtcars$cyl)
length(unique(mtcars$cyl))
#3 rozne:6 4 8

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mtcars[mtcars$cyl==4, "drat"] #wszystkie takie wartosci
mean(mtcars[mtcars$cyl==4, "drat"])
#4.070909

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

unique(mtcars$am) 
# 1 i 0
table(mtcars$am)
#19 zer, 13 jedynek

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot

barplot(table(mtcars$cyl))

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
#1
data(employees)
employees[employees$name=="John"& employees$surname=="Insecure",]
proton(action = "login", login="johnins")
#2
for(password in top1000passwords){
  if(proton(action = "login", login="johnins", password=password)=="Success! User is logged in!"){
    print(password)
    break
  }
}
#3
employees[employees$surname=="Pietraszko",]
logs[logs$login=="slap", "host"]
sort(table(logs[logs$login=="slap", "host"]))
proton(action = "server", host="194.29.178.16")

#4
bash_history

## 5) Umieszczamy rozwiązanie na repozytorium.