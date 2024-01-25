library(shiny)
source("./servers/gym.R")
source("./servers/moodServer.R")
source("./servers/sleepServer.R")
source("./servers/foodServer.R")

server <- function(input, output, session) {
  gymLogic(input, output, session)
  moodLogic(input, output, session)
  sleepLogic(input, output, session)
  foodLogic(input, output, session)
}
