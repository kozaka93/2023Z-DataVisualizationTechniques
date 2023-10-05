install.packages("proton")
library(proton)
proton()

# ETAP 1

employees[employees$name == "John", ]
proton(action = "login", login="johnins")

# ETAP 2

for (word in top1000passwords) {
  proton(action = "login", login="johnins", password=word)
}

# ETAP 3

employees[employees$surname == "Pietraszko", ]
head(logs)
slaplogs = logs[logs$login == "slap", ]
table(slaplogs$host)
