#instalacja
install.packages("proton")
library(proton)
proton()

data("employees")
employees[employees$name=="John" & employees$surname=="Insecure", ]
proton(action = "login", login = "johnins")


for (i in top1000passwords){
  proton(action = "login", login = "johnins", passwords=i)
}

data(logs)
rightPerson <- logs[logs$login=="johnins", ]
table(rightPerson)
