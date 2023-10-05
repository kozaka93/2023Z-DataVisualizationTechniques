install.packages("proton")
library(proton)
proton()
employees[employees$surname == "Insecure",]
proton(action = "login", login="johnins")
for (variable in top1000passwords) {
  proton(action = "login", login="johnins", password=variable)
}
employees[employees$surname == "Pietraszko",]
slp <- logs[logs$login == "slap",]
table(slp$host)
#194.29.178.16
proton(action = "server", host ="194.29.178.16")