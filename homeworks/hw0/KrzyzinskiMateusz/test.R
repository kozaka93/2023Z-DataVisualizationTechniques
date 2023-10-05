# Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

#etap 1 
colnames(employees)
employees[employees$surname == "Insecure", ]
proton(action = "login", login = "johnins")


# etap 2
proton(action = "login", login="johnins", 
       password="ABC")
top1000passwords

for (pass in top1000passwords){
  response <- proton(action = "login", login = "johnins",
                     password = pass)
  if (response == "Success! User is logged in!"){
    cat(pass)
  }
}

# etap 3
employees[employees$surname == "Pietraszko", ]
sort(table(logs[logs$login == "slap", "host"]))
proton(action = "server", host = "194.29.178.16"