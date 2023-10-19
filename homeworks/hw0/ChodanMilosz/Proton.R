install.packages("proton")
library(proton)
proton()

data("employees")
employees[employees$name == "John", "login"]
employees[employees$surname == "Insecure", "login"]
proton(action = "login", login="johnins")
data("top1000passwords")
for(i in 1:1000) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}
data("logs")
proton(action = "server", host="XYZ")
employees[employees$surname == "Pietraszko", "login"]
logs[logs$login == "slap",]
x <-data.frame(table(logs[logs$login == "slap","host"]))
x[x$Freq != 0,]
proton(action = "server", host="194.29.178.16")
data("bash_history")
