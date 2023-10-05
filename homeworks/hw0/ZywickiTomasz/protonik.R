install.packages('proton')
library(proton)
proton()

View(employees)
employees[employees$name == 'John'& employees$surname == 'Insecure', ]

proton(action = 'login', login = 'johnins')


View(top1000passwords)
for(pass in top1000passwords) {
  proton(action = "login", login = "johnins", password = pass)
}

#Well done! This is the right password!
#Bit used John Insecure's account in order to log into the Proton server.
#It turns out that John has access to server logs.
#Now, Bit wants to check from which workstation Pietraszko is frequently logging into the Proton server. Bit hopes that there will be some useful data.  

#Logs are in the `logs` dataset. 
#Consecutive columns contain information such as: who, when and from which computer logged into Proton.
#Problem 3: Check from which server Pietraszko logs into the Proton server most often.

#Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
#The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.

View(logs)
employees[employees$surname == 'Pietraszko', ]
df = logs[logs$login == 'slap', ]
df[which.max(df$host), ]
cts = table(df$host)
names(cts[which.max(cts)])
proton(action = 'server', host='194.29.178.16')


#Problem 4: Find the Pietraszko's password.

#In the `bash_history` dataset you will find all commands and parameters which have ever been entered.
#Try to extract from this dataset only commands (only strings before space) and check whether one of them looks like a password.

View(bash_history)



