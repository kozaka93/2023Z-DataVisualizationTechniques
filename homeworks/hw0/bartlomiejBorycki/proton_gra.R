
install.packages("proton")
library(proton)
proton()

#zad1
data("employees")
log <- employees[employees$name=="John" & employees$surname=="Insecure" ,"login"]



#zad2
data("top1000passwords")

for(i in 1:length(top1000passwords)){
  proton(action = "login", login=log, password=top1000passwords[i],host=ip)
}

#zad3
pietraszkolog <- employees[employees$surname == "Pietraszko","login"]

data(logs)
str(logs)


  
  
  
  
