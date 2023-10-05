#Gra proton - rozwiązanie

install.packages("proton")
library(proton)
proton()

#Etap 1

employees

colnames(employees)

employees[employees$surname == "Insecure", ]

proton(action = "login", login = "johnins")

#Etap 2

top1000passwords

for (ABC in top1000passwords) {
  result <- proton(action = "login", login = "johnins", password = ABC)
  if (result == 'Success! User is logged in!') {
    cat(ABC)
  }
}

# Hasło: q1w2e3r4t5

#Etap 3

employees[employees$surname == "Pietraszko",]

sort(table(logs[logs$login == "slap", c("host")]))

proton(action = "server", host = "194.29.178.16")

#Etap 4

bash_history 

bh <- strsplit(bash_history, " ")

commands <-  c()

for (x in bh){ 
  commands <- c(commands, x[[1]])
}

for (pass in unique(commands)){
  proton(action = "login", login = "slap", password = pass)
}