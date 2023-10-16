library(proton)
proton()
#Etap 1
proton(action = "login", login = employees[employees$name=="John" & employees$surname=="Insecure", "login"])

for (pass in top1000passwords){
  proton(action = "login", login = employees[employees$name=="John" & employees$surname=="Insecure", "login"], password = pass)
}

employees[employees$surname == "Pietraszko", ]
