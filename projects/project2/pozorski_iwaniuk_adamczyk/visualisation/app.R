source("dataMain.R")
source("serverMain.R")
source("uiMain.R")


#####runapp#####
shinyApp(ui = uiMain, server = serverMain) 

