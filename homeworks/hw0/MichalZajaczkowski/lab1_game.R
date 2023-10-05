install.packages("proton")
library(proton)
proton()
employees[employees$name == "John", ]
proton(action = "login", login="johnins")
top1000passwords
for(i in top1000passwords) {
  proton(action = "login", login="johnins", password=i)
}
str(logs)
head(logs)
employees[employees$surname == "Pietraszko", ]
slap
head(logs)
logs$login
table(logs[logs$login == "slap", ]$host)
194.29.178.108
proton(action = "server", host="194.29.178.16")
