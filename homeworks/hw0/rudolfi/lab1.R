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
mtcars[1]
mtcars[,1]

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10, 2:3]
mtcars[1:10, c(2,5)]

# Jak wybieramy kolumny po nazwach? 

mtcars[, "mpg"]
mtcars$mpg

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[, c('am', 'wt','mpg')]

# Jak wybierać jedną kolumnę?


# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)
nrow(mtcars)
ncol(mtcars)

# 2. Jakie są typy zmiennych?

str(mtcars)


# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

unique(mtcars[,'cyl'])
as.factor(mtcars$cyl)
factor(mtcars$cyl, levels= c("8", "6", "4"), ordered=TRUE)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars$drat[mtcars$cyl==4])

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

str(employees)

condition= employees$name=="John" & employees$surname=="Insecure"

employees[condition, ]

proton(action = "login", login="johnins")

top1000passwords

for (x in 1:length(top1000passwords)) {
  val= top1000passwords[x]
  proton(action = "login", login="johnins", password=val)
}

for (x in 1:length(top1000passwords)) {
  val= top1000passwords[x]
  proton(action = "login", login="slap", password=val)
}


login= employees[employees$surname=="Pietraszko",]

login$

host<- logs[, "host"]

host<- table(host)

host_right<-logs[order(host,decreasing = TRUE)][1]

host_right

proton(action = "server", host=host_right)


## 5) Umieszczamy rozwiązanie na repozytorium.
