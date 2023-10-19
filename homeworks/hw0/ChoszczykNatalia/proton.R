install.packages("proton")
library(proton)
proton()

employees
employees[employees$name == "John", ]

proton(action = "login", login="johnins")

head(top1000passwords)

for(i in 1:1000) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

head(logs)
proton(action = "server", host="johnins")

employees[employees$surname == "Pietraszko", ]

proton(action = "server", host="slap")



table(logs[logs$login == "slap", ]$host)
