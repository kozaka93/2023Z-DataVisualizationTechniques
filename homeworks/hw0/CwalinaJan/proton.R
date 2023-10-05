#install.packages("proton")
#library(proton)
#proton()

#1
login = employees[employees$name=="John" & employees$surname=="Insecure",]$login
proton(action = "login", login=login)

#2
for (i in top1000passwords) {
  proton(action = "login", login="johnins", password=i)
}

#
employees[employees$name=="Pietraszko" | employees$surname=="Pietraszko",]
head(sort(table(logs[logs$login=="slap",c(2)]),decreasing = TRUE))
proton(action = "server", host="194.29.178.16")