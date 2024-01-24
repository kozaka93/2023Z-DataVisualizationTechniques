library(shiny)
library(shinydashboard)
library(lubridate)
source("./intro/intro.R")
source("./ui/sleep.R")
source("./ui/mood.R")
source("./ui/food.R")

ui <- dashboardPage(
  dashboardHeader(title = "HealthieR"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Main Page", tabName = "main", icon = icon("dashboard")),
      menuItem("Gym", tabName = "silka", icon = icon("weight")),
      menuItem("Sleep", tabName = "sleep", icon = icon("bed")),
      menuItem("Emotions", tabName = "mood", icon = icon("smile")),
      menuItem("Nutrition", tabName = "food", icon = icon("utensils")),

      radioButtons("selectPerson",
                   "Select person to show: ",
                   choices = c("Adam", "Hubert", "Mateusz"),
                   selected = "Adam"),
      
      # Conditional for silka page
      conditionalPanel(
        condition = "input.sidebar == 'silka'",
        HTML(paste("<p style='padding-left: 10px;'>Tracking app used: <strong>Hevy</strong></p>")),
        radioButtons("variables", "Variables to Display:", 
                     choices = c("Weight" = "weight", "Reps" = "reps"),
                     selected = "weight"),
        selectInput(
          "spider_date_start",
          "Select Date",
          choices = c(7, 31, 365),
          selected = 31
        ),
        dateRangeInput(
          "dateRangeGym",
          "Wybierz zakres dat:",
          start = "2023-12-12",
          end = "2024-01-07",
        )
      ),
      
      # Conditional for mood page
      conditionalPanel(
        condition = "input.sidebar == 'mood'",
        HTML(paste("<p style='padding-left: 10px;'>Tracking app used: <strong>Daylio</strong></p>")),
        dateRangeInput(
          "dateRange",
          "Select date range:",
          start = "2023-12-12",
          end = "2024-01-07",
        ),
        radioButtons(
          "chartType",
          "Select plot type:",
          choices = c("Lollipop", "Line"),
          selected = "Lollipop"
        )
      ),
      
      # Conditional for sleep page
      conditionalPanel(
        condition = "input.sidebar == 'sleep'",
        HTML(paste("<p style='padding-left: 10px;'>Tracking app used: <strong>Sleep Android</strong></p>")),
        dateRangeInput(
          "dateRangeSleep",
          "Select date range:",
          start = "2023-12-12",
          end = "2024-01-07",
        )
      ),
      
      # Conditional for nutrition page
      conditionalPanel(
        condition = "input.sidebar == 'food'",
        HTML(paste("<p style='padding-left: 10px;'>Tracking app used: <strong>MyFitnessPal</strong></p>")),
        radioButtons("showModeFood",
                    "Select plot type:",
                    choices = c("Lines and markers", "Bars"),
                    selected = "Lines and markers")
      )
      
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
              tags$style(
                HTML(".img-circle {
              border-radius: 50%;
              width: 150px; 
              height: 150px; 
              object-fit: cover;
                }
              .box.box-solid.box-primary>.box-header {
                color:#fff;
                background:#605ca8;
                
              }
              .box.box-solid.box-primary{
                border-bottom-color:#605ca8;
                border-left-color:#605ca8;
                border-right-color:#605ca8;
                border-top-color:#605ca8;
                             
    }
              ")
            )),
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                column(12, align = 'center',
                       HTML(paste("<span style='font-size: 60px; margin-bottom: 10px;'>
                                  <strong>Welcome!</strong></span><br>
                                  <span style='font-size: 20px;'>
                                  Modern technology advancements have enabled a lot of 
                                  different ways to control your well-being better - applications 
                                  on your smartphone can help you see how effectively you
                                  eat, sleep or exercise. They allow you to easily track your 
                                  mood, meals or trainings. In this project, we have decided 
                                  to use that opportunity to take a look at our lifestyles.
                                  Combining R, JS, HTML and CSS in its code, this application 
                                  summarizes and analyzes important information we have 
                                  gathered about ourselves and our health.<br>")),
                       
                       libraries_used(),
                       
                       HTML('<h2>About us:</h2>')
                            ),
              ),
              
              fluidRow(
                column(4, intro_columns("./photos/Adam.jpg", 
                                        "Adam Kaniasty", 
                                        "Student of Data Science at Warsaw University of Technology. Co-Founder of A{P}PI Marketplace",
                                        "https://github.com/AdamKaniasty",
                                        "https://www.linkedin.com/in/adam-kaniasty-35a5a21a6/"), 
                       align = 'center'),
                column(4, intro_columns("./photos/Hubert.jpg", 
                                        "Hubert Kowalski", 
                                        "I'm a student at Warsaw University of Technology, originally from the charming city of Bydgoszcz. 
                                        While I've dabbled in various sports like calisthenics or squash, these days, climbing is 
                                        my go-to workout. Outside the university hustle, you'll find me enjoying skiing, playing the piano and unwinding with a good movie. 
                                        ",
                                        "https://github.com/kowalskihubert",
                                        "https://www.linkedin.com/in/hubert-kowalski-1b19bb1a3"), 
                       align = 'center'),
                column(4, intro_columns("./photos/Mateusz.jpg", 
                                        "Mateusz Król", 
                                        "Originally from Koszalin near the Baltic Sea, I currently study data analysis at Warsaw University of Technology. 
                                        Fan of all things space-related, from astronomy to science-fiction games. When the weather is good, I enjoy long cycling trips.",
                                        "https://github.com/mkrol11",
                                        "https://www.linkedin.com/in/mateusz-król-1a6a38265"), 
                       align = 'center')
              )
              
      ),
      tabItem(tabName = "silka",
              fluidRow(
                column(12,
                       source("./ui/gym.R", local = TRUE)
                )
              )
      ),
      tabItem(tabName = "sleep",
              fluidRow(
                column(12, generate_sleep_ui()
                       )
              )
      ),
      tabItem(tabName = "mood",
              fluidRow(
                column(12, generate_mood_ui()
                       )
              )
      ),
      tabItem(tabName = "food",
              fluidRow(
                column(12, generate_nutrition_ui())
              )
      )
    )
  ),
  skin = "purple"
)

