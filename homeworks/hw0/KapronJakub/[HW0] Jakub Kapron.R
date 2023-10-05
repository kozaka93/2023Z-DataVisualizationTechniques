library(proton)
proton()

# Problem 1
data("employees")
login <- employees[employees$name == "John" & employees$surname == 'Insecure', 'login']

proton(action = "login", login=login)

# Problem 2
data("top1000passwords")

for (password in top1000passwords) {
  proton(action = 'login', login = login, password = password)
}

# Problem 3
data(logs)
employees[employees$surname == 'Pietraszko', ]
sort(table(logs[logs$login == 'slap', 'host']))
proton(action = "server", host="194.29.178.16")
