install.packages("proton")
library(proton)
proton()
data("employees")
employees[employees$name == "John",]
proton(action = "login", login="johnins")
data("top1000passwords")
for(i in top1000passwords){
  proton(action = "login", login="johnins", password=i)
}

data("logs")
logs
employees[employees$surname == "Pietraszko",]
data.frame(table(logs[logs$login == "slap",]$host))


