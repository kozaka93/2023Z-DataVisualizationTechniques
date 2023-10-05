install.packages("proton")
library(proton)
proton()

data("employees")
employees1 <- employees[employees$name == "John", ]
employees1[employees1$surname == "Insecure", c("login")]

proton(action = "login", login="johnins")


for(i in 1:1000){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

data(logs)
loginP <- employees[employees$surname == "Pietraszko", c("login")]
table(logs[logs$login == loginP, ]$host)

proton(action = "server", host="194.29.178.16")


data("bash_history")
