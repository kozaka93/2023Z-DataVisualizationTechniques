install.packages("proton")
library(proton)
proton()

for (x in top1000passwords) {
  proton(action = "login", login="johnins", password=x)
}

employees[employees$surname=="Pietraszko",]

logs[logs$login=="slap","host"]
names(which.max(table(logs[logs$login=="slap","host"])))
