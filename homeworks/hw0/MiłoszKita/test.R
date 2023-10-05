install.packages("proton")
library(proton)
proton()

employees[(employees$name == "John") & (employees$surname == "Insecure"),]$login
proton(action = "login", login = "johnins")

top1000passwords

for (i in 1:1000) {
  proton(action = "login", login = "johnins", password = top1000passwords[i])
}

employees[employees$surname == "Pietraszko",]
sort(table(logs[logs$login == "slap","host"]))
proton(action = "server", host = "194.29.178.16")
