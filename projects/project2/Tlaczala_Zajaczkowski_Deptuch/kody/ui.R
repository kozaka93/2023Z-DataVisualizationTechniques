library(shiny)
library(shinydashboard)
library(networkD3)

grafUI <- fluidPage(
  fluidRow(
    tags$div(
      tags$div(
        tags$div(
          "",
          class = "activity-status"
        ),
        "",
        class = "profile-picture"
      ),
      tags$div(
        tags$div(
          "What hides in our head?",
          class = "title-text"
        ),
        tags$div(
          "Active now",
          class = "status-text"
        ),
        class = "title-box"
      ),
      tags$div(
        icon("phone"),
        icon("video"),
        icon("circle-info"),
        class = "icon-box fa-3x"
      ),
      class = "title-bar white-text"
    ),
    tags$div(
      tags$div(
        sliderInput(
          "charNr",
          "How many characters?",
          step = 1,
          min = 1,
          max = 19,
          value = c(4, 12)
        ),
        sliderInput(
          "wordsNr",
          "How many words?",
          step = 1,
          min = 1,
          max = 10,
          value = 7
        ),
        class = "input-box"
      ),
      forceNetworkOutput("plot"),
      class = "main-dish main-dish-graph"
    )
  )
)

plotKiedyUI <- fluidPage(
  fluidRow(
    tags$div(
      tags$div(
        tags$div(
          "",
          class = "activity-status"
        ),
        "",
        class = "profile-picture"
      ),
      tags$div(
        tags$div(
          "Do we even sleep?",
          class = "title-text"
        ),
        tags$div(
          "Active now",
          class = "status-text"
        ),
        class = "title-box"
      ),
      tags$div(
        icon("phone"),
        icon("video"),
        icon("circle-info"),
        class = "icon-box fa-3x"
      ),
      class = "title-bar white-text"
    ),
    tags$div(
      tags$div(
        radioButtons("time",
          "Choose period:",
          choices = c("Month", "Day Of Month", "Hour"),
          selected = "Hour"),
        class = "input-box"
      ),
      plotlyOutput("plotKiedy"),
      class = "main-dish main-dish-when"
    )
  )
)

plotZKimUI <- fluidPage(
  fluidRow(
    tags$div(
      tags$div(
        tags$div(
          "",
          class = "activity-status"
        ),
        "",
        class = "profile-picture"
      ),
      tags$div(
        tags$div(
          "These are the people that we like the most",
          class = "title-text"
        ),
        tags$div(
          "Active now",
          class = "status-text"
        ),
        class = "title-box"
      ),
      tags$div(
        icon("phone"),
        icon("video"),
        icon("circle-info"),
        class = "icon-box fa-3x"
      ),
      class = "title-bar white-text"
    ),
    tags$div(
      tags$div(
        radioButtons("zKim",
          "Z kim",
          choices = c("Groups", "People", "All"),
          selected = "All"),
        class = "input-box"
      ),
      plotlyOutput("plotZKim"),
      class = "main-dish main-dish-towho"
    )
  )
)
plotOdKogoUI <- fluidPage(
  fluidRow(
    tags$div(
      tags$div(
        tags$div(
          "",
          class = "activity-status"
        ),
        "",
        class = "profile-picture"
      ),
      tags$div(
        tags$div(
          "Those people were constantly thinking about us this year",
          class = "title-text"
        ),
        tags$div(
          "Active now",
          class = "status-text"
        ),
        class = "title-box"
      ),
      tags$div(
        icon("phone"),
        icon("video"),
        icon("circle-info"),
        class = "icon-box fa-3x"
      ),
      class = "title-bar white-text"
    ),
    tags$div(
      tags$div(
        radioButtons("odKogo",
          "Ok kogo",
          choices = c("Groups", "People", "All"),
          selected = "All"),
        class = "input-box"
      ),
      plotlyOutput("plotOdKogo"),
      class = "main-dish main-dish-fromwho"
    )
  )
)

shinyUI(
  dashboardPage(
    dashboardHeader(title = tags$a(tags$img(src="/media/messengerLogo.png", height=40, width=40),
                                   "MESSENGE(R)", class = "white-text")),
    dashboardSidebar(
      sidebarMenu(
        id = 'tabs',
        menuItem("Who do we message?", icon = icon("users"),
                 menuSubItem("Messages out", tabName = "wykres1"),
                 menuSubItem("Messages in", tabName = "wykres2")),
        menuItem("When do we write?", icon = icon("clock"), tabName = "wykres5"),
        menuItem("What do we write?", icon = icon("comment-dots"), tabName = "wykres3"),
        
        menuItem(selectInput("user",
                             "Choose your hero: ",
                             c("Mateusz",
                               "Kornel",
                               "Michał"),
                             selected = 'Kornel'),
                 icon = icon("user")
        ),
        class = "fa-2x"
      )
    ),
    
    dashboardBody(tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                  ),
      tabItems(
        tabItem("wykres3", grafUI),
        tabItem("wykres5", plotKiedyUI),
        tabItem("wykres2", plotOdKogoUI),
        tabItem("wykres1", plotZKimUI)
      )
    ),
  )
)
