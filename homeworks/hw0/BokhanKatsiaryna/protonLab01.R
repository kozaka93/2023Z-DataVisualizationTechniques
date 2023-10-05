install.packages("proton")
library(proton)
proton()


employees[employees$name=="John",]
johnins
proton(action = "login", login="johnins")


top1000passwords
for (i in top1000passwords){
  proton(action = "login", login="johnins", password=i)
}
  

for (i in logs[logs$login=="johnins",]$host){
  proton(action = "server", host=i)
}


proton(action = "server", host="194.29.178.81")
