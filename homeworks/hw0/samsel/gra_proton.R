
## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

head(employees)
loginJohn <- employees[employees$surname == "Insecure" & employees$name == "John", "login"]
proton(action = "login", login=loginJohn)

for (i in 1:1000){
  proton(action = "login", login=loginJohn, password=top1000passwords[i])
}

head(logs)
loginPietr <- employees[employees$surname == "Pietraszko", "login"]
logsP <- logs[logs$login == loginPietr,]
logsJohn <- sort(table(logsP$host))
names(logsJohn[length(logsJohn)])
proton(action = "server", host=names(logsJohn[length(logsJohn)]))

head(bash_history)
library(stringr)
trimed <- str_extract(bash_history, pattern = "[^ ]+")

for (i in 1:length(trimed)){
    proton(action = "login", login=loginPietr, password=trimed[i])
}
