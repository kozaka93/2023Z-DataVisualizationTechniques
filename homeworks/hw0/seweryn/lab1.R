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
mtcars[1,]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]
# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]
# Jak wybieramy kolumny po nazwach? 
mtcars["gear"]
# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c("am","wt","mpg")]
# Jak wybierać jedną kolumnę?
mtcars[,1]
# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)
nrow(mtcars)
ncol(mtcars)
# 2. Jakie są typy zmiennych?
# integrer, numeric, character, logical, factor
str(mtcars)
# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars[,"cyl"])
as.factor(mtcars$cyl) #FACTOR - taki enum, wartosci ktore mozemy podporzadkowac
factor(mtcars$cyl, levels = c("8", "6", "4"), ordered = TRUE)
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4,"drat"])
# 5. Jakie są unikal=ne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
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
head(employees)
dt1 = employees[employees$surname == "Insecure",]
login = dt1$login
login
proton(action = "login", login = login)
top1000passwords
proton(action = "login", login=login, password=top1000passwords)

a = lapply(top1000passwords, function(x){
  proton(action = "login", login=login, password=x)
})

for(i in 1:1000){
  if(proton(action = "login", login=login, password=top1000passwords[i]) == "Success! User is logged in!" ){
    top1000passwords[i]
    break
  
  }
}

head(logs)
proton(action = "server", host="XYZ")
employees[employees$surname == "Pietraszko",]
b = logs[logs$login == "slap",]
b
c = table(b$host)

## 5) Umieszczamy rozwiązanie na repozytorium.
library(dplyr)
host = c %>% sort(decreasing = T) %>% head(1)

server = "194.29.178.16"
proton(action = "server", host=server)

head(bash_history)
bash_history[is.character(bash_history)]

library(stringr)

table(word(bash_history, 1))

proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")

