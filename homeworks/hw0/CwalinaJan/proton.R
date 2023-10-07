#install.packages("proton")
library(proton)
library(stringi)
#proton()

#1
login = employees[employees$name=="John" & employees$surname=="Insecure",]$login
proton(action = "login", login=login)

#2
for (i in top1000passwords) {
  proton(action = "login", login="johnins", password=i)
}

#3
employees[employees$name=="Pietraszko" | employees$surname=="Pietraszko",]
head(sort(table(logs[logs$login=="slap",c(2)]),decreasing = TRUE))
proton(action = "server", host="194.29.178.16")

#4
for (i in unique(stri_extract(bash_history,regex = "\\w+"))) {
  proton(action = "login", login="slap", password=i)
}
