install.packages("proton")
library(proton)
proton()

data(employees)
employees[employees$surname == "Insecure", ]
proton(action = "login", login="johnins")

for(i in top1000passwords) {
  message <- proton(action = "login", login="johnins", password=i)
  if(message == "Success! User is logged in!") {
    return
  }
}

data(logs)
johnins_logs <- logs[logs$login == "johnins",]
