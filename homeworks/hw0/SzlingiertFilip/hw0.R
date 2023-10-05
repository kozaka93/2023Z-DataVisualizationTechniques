install.packages("proton")
library(proton)
proton()

str(employees)
employees[employees$name == "John" & employees$surname == "Insecure",]
proton(action = "login", login = "johnins")

for(i in top1000passwords){
  proton(action = "login", login="johnins", password = i)
}

str(logs)
employees[employees$surname == "Pietraszko",]
mp <- logs[logs$login == "slap",] 
?max
max(table(mp$host))
mp2 <- table(mp$host)

proton(action = "server", host = "194.29.178.16")
