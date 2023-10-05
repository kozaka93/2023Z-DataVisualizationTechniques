install.packages("proton")
library(proton)
proton()
head(employees)
employees[employees$name=="John", ]
proton(action="login",login="johnins")
for (i in top1000passwords){
  proton(action = "login", login="johnins", password=i)
}
employees[employees$surname=="Pietraszko", ]
tmp <- logs[logs$login=="slap",]

aggregate(tmp$login,tmp['host'],length)
proton(action = "server", host="194.29.178.16")


