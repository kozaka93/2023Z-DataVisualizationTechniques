library(proton)
proton()

login <- employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login = login)

for(password in top1000passwords) {
  proton(action = "login", login = login, password = password)
}

employees[employees$surname == "Pietraszko",]
sort(table(logs[logs$login == "slap", "host"]))
proton(action = "server", host = "194.29.178.16")