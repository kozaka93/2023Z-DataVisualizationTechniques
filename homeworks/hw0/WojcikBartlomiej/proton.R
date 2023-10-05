install.packages("proton")
library(proton)
proton()

str(employees)
employees[employees$name == "John" & employees$surname == "Insecure",]
proton(action = "login", login="johnins")

str(top1000passwords)
top1000passwords[217]
for(password in top1000passwords) {
  proton(action = "login", login="johnins", password=password)
}

view(logs)
str(logs)
employees[employees$name == "Slawomir" & employees$surname == "Pietraszko",]

max(table(logs[logs$login == "slap" , "host"]))
proton(action = "server", host="194.29.178.16")
