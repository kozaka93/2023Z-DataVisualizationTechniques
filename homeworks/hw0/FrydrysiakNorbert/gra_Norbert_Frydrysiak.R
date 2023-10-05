library(proton)
proton()
em=employees
login=em[em$name == 'John' & em$surname=='Insecure',]$login
proton(action = "login", login="johnins")
topka=top1000passwords
for (v in topka) {
  cat(v)
  cat("\n")
  proton(action = "login", login="johnins", password=v)
}

#q1w2e3r4t5 is correct password

proton(action = "login", login="johnins", password="q1w2e3r4t5")

login2=em[em$surname=='Pietraszko',]$login
logi=logs[logs$login == login2,]

tail(sort(table(logi$host)),1)
# 194.29.178.16
proton(action = "server", host="194.29.178.16")


pass=grep( "\\s\\w+" ,bash_history, value=TRUE)
