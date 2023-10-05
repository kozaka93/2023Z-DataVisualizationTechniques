install.packages("proton")
library(proton)
pproton()

#Zad 1

head(employees)
employees[employees["surname"]=="Insecure",]
employees[employees["surname"]=="Pietraszko",]

##Slawomir Pietraszko  slap
##John Insecure johnins

#Zad 2
top1000passwords


for (val in top1000passwords){
  p=val
  proton(action = "login", login="johnins", password=p)
  
}
lgs <- logs

#Zad 3
head(logs)
l <- logs[logs["login"]=="slap",]
f <-  as.data.frame(table(l["host"]))


