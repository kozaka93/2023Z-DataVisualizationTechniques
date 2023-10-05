
## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

head(employees)
employees$login[employees$name == "John"]

proton(action = "login", login="johnins")

top1000passwords
for (i in top1000passwords) {
  proton(action = "login", login="johnins", password=i)
}

head(logs)
table(logs$host)

employees[employees$surname == "Pietraszko", ]
sort(table(logs[logs$login == "slop", "host"]))
proton(action = "server", host="194.29.178.16")


## 5) Umieszczamy rozwiązanie na repozytorium.