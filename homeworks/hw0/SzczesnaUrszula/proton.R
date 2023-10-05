install.packages("proton")
library(proton)
proton()
employees
employees[employees$name == "John",]

proton(action = "login", login="johnins")

head(top1000passwords)

proton(action = "login", login="XYZ", password="ABC")

for(i in top1000passwords){
  proton(action = "login", login="johnins", password=i)

}

head(logs)
employees[employees$surname == "Pietraszko",]
logs[logs$login == "slap",]
data.frame(table(logs[logs$login =="slap",]$host))
#194.29.178.16
proton(action = "server", host="194.29.178.16")


head(bash_history)
