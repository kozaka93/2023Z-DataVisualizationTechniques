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

mtcars[c(2,3),c(1,3)]
mtcars[2:10, 1:5]
# od:do indeksacja od 1

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,]
mtcars[,1]
mtcars[,1,drop = FALSE]
mtcars[1,1]


# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]
mtcars[1:10, c(2,3)]

# Jak wybieramy kolumny po nazwach? 
mtcars[, c("mpg", "cyl")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$wt
mtcars["wt"]
var <- "wt"
mtcars[var]
dim(mtcars)

# Uwaga na przecinek i wybór kolumn poprzez indeksy
mtcars[1,2]
mtcars[c(1,2)]

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mtcars[mtcars$cyl == 4, "drat"]

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot



# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))
hist(table(mtcars$cyl))


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
proton(action="login", login = "johnins")
for (x in top1000passwords){
  if (proton(action="login", login="johnins", password = x) =="Success! User is logged in!"){
    print(x)
  }
}
proton(action="login", login = "johnins", password = "q1w2e3r4t5")
logs[logs$login == "johnins",]
head(logs)

## 5) Umieszczamy rozwiązanie na repozytorium.