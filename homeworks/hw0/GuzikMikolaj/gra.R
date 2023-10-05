install.packages("proton")
library(proton)
proton()
employees[employees$surname == "Insecure", ]
proton(action = "login", login="johnins")
top1000passwords
for(i in top1000passwords)
{
  proton(action = "login", login="johnins", password=i)
}
employees[employees$surname == "Pietraszko", ]
logs[logs$login == "slap",]
max(table(logs[logs$login == "slap",]$host))

proton(action = "server", login="194.29.178.16")
