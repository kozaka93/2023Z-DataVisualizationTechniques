library(proton)
proton()

colnames(employees)
employees[employees$name == "John",]
proton(action = "login", login = "johnins")

# etap 2
for(i in 1:1000) {
  res <-  proton(action = "login", login = "johnins", password = top1000passwords[i])
  res
}

# etap 3
str(logs)
employees[employees$surname == "Pietraszko",]
logiPietraszko <- logs[logs$login == "slap",]
aggregate(logiPietraszko)
