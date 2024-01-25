library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(fresh)
library(lubridate)
library(tidyverse)
library(viridis)
library(htmltools)
library(scales)
library(leaflet)
library(geosphere)
library(htmlwidgets)
library(dashboardthemes)
library(shinycssloaders)
library(magick)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

#setwd("C:\\Users\\macie\\Desktop\\STUDIA\\SEMESTR3\\Techniki Wizualizacji Danych\\PROJEKTY\\Project2\\MM")
activities_Ola <- read.csv("data\\activities_Ola.csv")
ActivitiesIndividual_Ola <- read.csv("data\\ActivitiesIndividual_Ola.csv")

ActivitiesTogether <- read.csv("data\\ActivitiesTogether_Maciek.csv")
ActivitiesTogether$startTime <- as.POSIXct(ActivitiesTogether$startTime, format = "%Y-%m-%d %H:%M:%S")
ActivitiesTogether$Rok <- format(ActivitiesTogether$startTime, "%Y")
ActivitiesTogether$RokAndMonth <- format(ActivitiesTogether$startTime, "%Y-%m")
ActivitiesTogether$Rok <- as.numeric(ActivitiesTogether$Rok)
ActivitiesTogether$Day <- wday(ActivitiesTogether$startTime, week_start = 1)
ActivitiesTogether$Month <- month(ActivitiesTogether$startTime)

zmienne = c("Distance (km)", "Time (minutes)","Average Speed (km/h)")
zmienne_heat = c("Number of records","Distance (km)","Time (minutes)")

Kroki_Calorie <- read.csv("data\\Kroki_Kalorie_Maciek.csv")
HeartRate_Maciek <- read.csv("data\\HeartRate_Maciek.csv")
HeartRate_Maciek$recordDay <- as.POSIXct(HeartRate_Maciek$recordDay)




# HEADER

# footer = HTML("
#                 <footer class='text-center text-sm-start' style='width:100%;'>
#                 <hr>
#                 <p class='text-center' style='font-size:12px;'>
#                   © 2024 Copyright:
#                   <a class='text-dark' href='https://www.mi2.ai/'>MI2</a>
#                 </p>
#                 </footer>
#                 ")

#        FOOTERA NIE WIEM W KTORYM MIEJSCU DODAC, JESLI WY WIECIE TO WSTAWCIE. 

header <- dashboardHeader(
  titleWidth = 300,
  title = tags$div(style = "color: white; font-size: 26px; font-family: 'Arial, sans-serif';", 
                   HTML('<img src="https://creazilla-store.fra1.digitaloceanspaces.com/icons/3432125/bike-icon-md.png" style="height:30px; margin-right:10px;">'),
                   "Our Bike Activity"))


customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#000000"
    ,primaryFontColor = "#ffffff"
    ,bodyBackColor = "#f2f2f2"
    
  ### header
  ,logoBackColor = "#f9a13c"
    
  
  ,headerButtonBackColor = "#394572"
    ,headerButtonIconColor = "#ffffff"
    ,headerButtonBackColorHover = "#f9a13c"
    ,headerButtonIconColorHover = "#ffffff"
    
  ,headerBackColor = "#394572"
    ,headerBoxShadowColor = "#394572"
    ,headerBoxShadowSize = "12px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "#f9a13c"
    ,colorMiddle = "#394572"
    ,colorEnd = "#1f2c5d"
    ,colorStartPos = 0
    ,colorMiddlePos = 70
    ,colorEndPos = 100
  )
  
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
    
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "#ffffff"
  ,sidebarTabTextSize = 15
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "#394572"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = "#394572"
  ,sidebarTabTextColorSelected = "#ffffff"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = "#394572"
  ,sidebarTabTextColorHover = "#ffffff"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "#394572"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "#E1E3EC"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 2px 2px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 20
  ,boxDefaultColor = "#394572"
  ,boxPrimaryColor = "#394572"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


# SIDEBAR

sidebar <- dashboardSidebar(
  
  width = 300,
  
  sidebarMenu(
  menuItem("Home", tabName = "home",icon = icon("home")),
  menuItem("TOP 5", tabName = "top5", icon = icon("map")),
  menuItem("Short/Medium/Long", tabName = "sml", icon = icon("chart-simple")),
  menuItem("Competition", tabName = "competition", icon = icon("trophy")),
  menuItem("Individual Statistics", tabName = "individualstat", icon = icon('chart-line'), startExpanded = F,
            menuSubItem('Maciek', tabName = "individualMaciek", icon = icon('m')),
            menuSubItem('Kuba', tabName = "individualKuba", icon = icon("k")),
            menuSubItem('Ola', tabName = "individualOla", icon = icon("o"))
           )
  )
  
)

# BODY

body <- dashboardBody(
  
  customTheme,

  
  tabItems(
    ### Kuba ###
       tabItem(tabName="home",
               #fluidRow(
                # uiOutput("homePageDescription")
               #),
               fluidRow(column(
                 width=12,
                 offset=0,
                 style="text-align: center;",
                 imageOutput("magickGif")
              )),
              fluidRow(
                
                column(4, h1("Kuba", align = "center")),
                
                column(4, h1("Maciek", align = "center")),
                
                column(4, h1("Ola", align = "center"))
              ),
              fluidRow(
                
                column(4, img(src = "ikona3-2.png", width = 300, height = 300),style="text-align: center;"),
                
                column(4, img(src = "ikona2-2.png", width = 300, height = 300),style="text-align: center;"),
                
                column(4, img(src = "ikona1-2.png", width = 300, height = 300),style="text-align: center;")
              )
              ),
    
       tabItem(tabName = "top5",
           
            fluidRow(
              box(title = "Top 5 tracks", status = "primary", solidHeader = T,
                  uiOutput("top5description")),
              box(status = "primary", 
                  sidebarMenu(id = "sidebarmenuTop5",
                              selectInput(
                                inputId = "track",
                                label = "Choose person:",
                                choices = c("Kuba", "Maciek", "Ola"))
                              
              ),
              sidebarMenu(id="sidebarmenuTrack",
                          selectInput(
                            inputId = "number_of_track",
                            label = "Choose interesting track:",
                            choices = c("1","2","3","4","5")))
              )),
            fluidRow(
              box(status = "primary",
                  shinycssloaders::withSpinner(leafletOutput(outputId = "mapOfTrack",width="100%",height=720),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),width = 12),
              box(title="Track description", status = "primary", solidHeader = T,
                  uiOutput("descriptionOfTrack"),
                  width = 12)
            ),
            
    ),
    tabItem(tabName = "individualKuba",
            fluidRow(
              box(title = "Individual Statistics", status = "primary", solidHeader = T,
                  uiOutput("individualTextKuba1"), width = 6),
              box(status = "primary", shinycssloaders::withSpinner(plotlyOutput("individualPlotKuba1"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ), width = 6),
              box(status = "primary", uiOutput("individualTextKuba2"), 
                  width=12)),
            fluidRow(
              box(title = "Elevation Data", status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(leafletOutput(outputId="individualPlotKuba2",width="100%",height = 720),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),width = 12,
                  uiOutput("individualTextKuba3"))),
            fluidRow(
              box(status = "primary",           
                selectInput("zmienna_heat_Kuba",
                            "Choose the variable to analyse",
                            zmienne_heat),
                width = 2),
              box(title = uiOutput("individualKuba4PlotTitle"), status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("individualKuba4"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  width = 5),
              box(title = uiOutput("individualKuba5PlotTitle"), status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("individualKuba5"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  width = 5)
            )
            ),
    
    
    ### Ola ###
    
    tabItem(tabName = "sml",
            
            fluidRow(
              box(title = "Small/Medium/Long Tracks", status = "primary", solidHeader = T,
                  uiOutput("smldescription")),
              sidebarMenu(id = "sidebarmenu",
                          selectInput(
                            inputId = "person",
                            label = "Choose person:",
                            choices = c("Kuba", "Maciek", "Ola"))
            )),
            
            fluidRow(
              box(title = "What type of routes do we most frequently choose?", 
                  status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("hisPlot"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ), width = 6),
              
              box(status = "primary", 
                  sliderInput("zakres1",
                                 "Choose the range of years",
                                 value = c(2020, 2023),
                                 min = 2020,
                                 max = 2023,
                                 step = 1),
                  uiOutput("smlchart1"),
                  width = 6
                     )
            ),
            
            fluidRow(
              box(title = "When do we go cycling?", status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("densityPlot"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  uiOutput("smlchart2"), width = 6),
              
              box(title = "Does the length of the route affect our average speed?",
                  status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("violinPlot"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  uiOutput("smlchart3"), width = 6)
              )
    ),
    
    tabItem(tabName = "individualOla",
            
            fluidRow(
              box(title = "Individual Statistics",
                  status = "primary", solidHeader = T,
                  uiOutput("individual1"), width = 6),
              box(status = "primary",
                  shinycssloaders::withSpinner(plotlyOutput("individualOla1"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
              ), width = 6)
            ),
            
            fluidRow(
              box(title =  "Analyzing the Diversity of Bike Adventures",
                  status = "primary", solidHeader = T,
                  selectInput("yearChoice",
                              "Activity from which year do your want to see?",
                              unique(ActivitiesIndividual_Ola$Year)),
                  shinycssloaders::withSpinner(plotlyOutput("individualOla2"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  shinycssloaders::withSpinner(plotlyOutput("individualOla3"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                width = 12)
            ),
            fluidRow(
              box(status = "primary",           
                selectInput("zmienna_heat_Ola",
                            "Choose the variable to analyse",
                            zmienne_heat),
                width = 2),
              box(title = uiOutput("individualOla4PlotTitle"), status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("individualOla4"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  width = 5),
              box(title = uiOutput("individualOla5PlotTitle"), status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("individualOla5"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  width = 5)
            )
            
            
            ),
    
    ### Maciek ###
    
    tabItem(tabName = "competition",
            fluidRow(
              box(
                title = "Competiton", status = "primary", solidHeader = T,
                uiOutput("competitiondescribtion"),
                uiOutput("competitionInteractivityDesc"), width = 6
              ),
              box(
                status = "primary", solidHeader = T,
                selectInput("zmienna",
                            "For which variable do you want to summarize?",
                            zmienne,
                           ),
                sliderInput("zakres",
                            "Choose the range of years",
                            value = c(min(2020), max(ActivitiesTogether$Rok)),
                            min = 2020,
                            max = max(ActivitiesTogether$Rok),
                            step = 1),
                width = 6
                 )
            ),
            
            fluidRow(
              box(
                title = uiOutput("competition1PlotTitle"), status = "primary", solidHeader = T,
                shinycssloaders::withSpinner(plotlyOutput("competition1"),
                                             type = getOption("spinner.type", default = 5),
                                             color = getOption("spinner.color", default = "#394572"),
                                             size = getOption("spinner.size", default = 1)
                ),
                width = 6
              ),
              box(
                title = "Progression over the years", status = "primary", solidHeader = T,
                shinycssloaders::withSpinner(plotlyOutput("competition2"),
                                             type = getOption("spinner.type", default = 5),
                                             color = getOption("spinner.color", default = "#394572"),
                                             size = getOption("spinner.size", default = 1)
                )
            ),
            )
    ),
    
    tabItem(tabName = "individualMaciek",
            # fluidRow(
            #   box(
            #     uiOutput("individualMaciekdescribtion"),
            #     status = "primary", solidHeader = T
            #   )
            # ),
            fluidRow(
              box(
                status = "primary",
                uiOutput("individualMaciek1Desc"),
                width = 3
              ),
              box(
                title = "Corelation between steps and calories",
                status = "primary", solidHeader = T,
                numericInput("limitKcal", "Type in calories limit", value=max(Kroki_Calorie$Kalorie)),
                shinycssloaders::withSpinner(plotlyOutput("individualMaciek1"),
                                             type = getOption("spinner.type", default = 5),
                                             color = getOption("spinner.color", default = "#394572"),
                                             size = getOption("spinner.size", default = 1)
                ),
                width = 9
              )
            ),
            fluidRow(
              box(
                title = "In which month do I walk the most?",
                status = "primary", solidHeader = T,
                shinycssloaders::withSpinner(plotlyOutput("individualMaciek2"),
                                             type = getOption("spinner.type", default = 5),
                                             color = getOption("spinner.color", default = "#394572"),
                                             size = getOption("spinner.size", default = 1)
                ),
                width = 6
              ),
              box(
                status = "primary",
                uiOutput("individualMaciek2Desc"),
                width = 6
              )
            ),
            fluidRow(
              box(uiOutput("individualMaciek3Desc"),
                  status = "primary",
                  width = 12
                  )
            ),
            fluidRow(
              box(title = "Average heart rate during exercising per month",
                  status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("individualMaciek3"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  width = 12
                  )
            ),
            fluidRow(
              box(status = "primary", solidHeader = T,           
                selectInput("zmienna_heat_Maciek",
                            "Choose the variable to analyse",
                            zmienne_heat),
                width = 2),
              box(title = textOutput("individualMaciek4PlotTitle"),
                  status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("individualMaciek4"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  width = 5),
              box(title = textOutput("individualMaciek5PlotTitle"),
                  status = "primary", solidHeader = T,
                  shinycssloaders::withSpinner(plotlyOutput("individualMaciek5"),
                                               type = getOption("spinner.type", default = 5),
                                               color = getOption("spinner.color", default = "#394572"),
                                               size = getOption("spinner.size", default = 1)
                  ),
                  width = 5)
            )
    )
    
  
  
  ## STYLES

  # tags$head(
  #     # # HTML('.main-sidebar { width: 250px; }'),
  #     # # HTML('.content-wrapper, .right-side { margin-left: 250px; }'),
  #     # HTML('.main-header .logo { width: 300px }'),
  #     # HTML('main-header { width: 400px }')
  # )
  
  )
)
dashboardPage(header, sidebar, body)

ui <- dashboardPage(header, sidebar, body)




server <- function(input, output) {

    output$hisPlot <- renderPlotly({
      
      ### Ola ###
      
      activities_Ola$Type <- factor(activities_Ola$Type, levels = c("Short", "Medium", "Long")) 
      
      activities_Ola %>% 
        filter(Osoba == input$person) %>% 
        group_by(Type) %>% 
        summarise(n = n()) -> summarizeData
      
      as.numeric(max(summarizeData$n)) -> max_coord 
    
      
      activities_Ola %>%
        filter(activities_Ola$Year >= input$zakres1[1],
               activities_Ola$Year <= input$zakres1[2]) %>% 
        filter(Osoba == input$person)-> activities_plot
      
      p <- ggplot(activities_plot, aes(x = Type, fill = Type, text = paste("<br>Number of tracks: ", after_stat(count)))) +
      geom_bar(stat = "count") +
      scale_fill_manual(values = c("#53296E", "#6B82DB", "#f9a13c")) +
      labs(title = "Number of tracks",
           x = "Type") +
      theme_minimal()+
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
              axis.title.y = element_blank()) +
      coord_cartesian(ylim = c(0, max_coord))
      
      ggplotly(p, tooltip = c("text"))

    })
    
    
    
    output$violinPlot <- renderPlotly({
      
      activities_Ola$Type <- factor(activities_Ola$Type, levels = c("Short", "Medium", "Long"))
      
      activities_Ola %>%
        filter(Osoba == input$person)-> activities_plot
      
      p <-ggplot(activities_plot, aes(x = Type, y = as.numeric(AvgSpeed), fill = Type)) +
        geom_violin() +
        scale_fill_manual(values = c("#53296E", "#6B82DB", "#f9a13c")) +
        labs(title = "Distribution of average speed",
             x = "Type",
             y = "Average Speed") +
        theme_minimal()+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = c(0, 35))
      
      ggplotly(p) %>% 
        style(hoverinfo = "none")
      
    })
    
    output$densityPlot <- renderPlotly({
      
      activities_Ola$Type <- factor(activities_Ola$Type, levels = c("Short", "Medium", "Long"))
      
      activities_Ola %>%
        filter(Osoba == input$person)-> activities_plot
      
      p <- ggplot(activities_plot, aes(x = Hour, fill = Type)) +
        geom_density(alpha = 0.75) +
        scale_fill_manual(values = c("#53296E", "#6B82DB", "#f9a13c")) +
        labs(title = "Density Plot of Activity Distribution Across Hours",
             x = "Hour") +
        theme_minimal()+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
              axis.title.y = element_blank())
      
      ggplotly(p) %>% 
        style(hoverinfo = "none")
      
      
    })
    
    output$smldescription <- renderUI({
      HTML("<div style='text-align: justify;'>
      Short/Medium/Long is a tab where our cycling adventure where categorized 
      according to the distance. A ride spanning 0-40 kilometers is considered 
      a short track, 40-70 kilometers falls into the medium category, while 
      anything beyond 70 kilometers is viewed as a long and adventurous expedition.</div>")
    })
    
    output$smlchart1 <- renderUI({
      HTML("<div style='text-align: justify;'>
      Explore our track completion trends with this interactive bar chart. 
      Categories (short, long, medium) are color-coded, and above slider let 
      you focus on specific years.Thanks to this chart, we can see our 
      preferences regarding track length and how they have changed over the years.</div>")
    })
    
    output$smlchart2 <- renderUI({
      HTML("<div style='text-align: justify;'>
      <br>
      On the above chart, you can observe the times of day when we embark on 
      different types of bike rides. It is noticeable that longer cycling 
      adventures are usually chosen during the morning hours, while shorter 
      rides tend to take place in the afternoon.</div>")
    })
    
    output$smlchart3 <- renderUI({
      HTML("<div style='text-align: justify;'>
      <br>
      On this chart, you can observe the speed distribution categorized by the 
      type of ride. Surprisingly, the distributions look quite similar. It is 
      slightly more challenging to maintain a high average speed on longer routes.</div>")
    })
    
    # individual
    
    output$individual1 <- renderUI({
      HTML("<div style='text-align: justify;'>
      Initially, cycling for me was associated with leisurely afternoon rides 
      with family or friends. I began to view cycling as a sport when I started 
      riding with my dad, a passionate cycling enthusiast. In the beginning, it was 
      just to enjoy our time together, but later on, I began tracking my time 
      and distance, challenging my own records, aiming to ride faster and farther. 
      The following page is dedicated to more detailed statistics such as speed, 
      heart rate, pedaling cadence, and riding frequency.<br>
      On the right side of the chart, you can observe the correlation between average 
      speed and pulse. It's evident that for longer distances, the relationship is slightly 
      higher than for shorter ones, even at comparable average speeds.</div>")
      
    })
    
    
    output$individualOla1 <- renderPlotly({
      
      p <- ActivitiesIndividual_Ola %>%
        filter(Średnie.tętno != 0) %>% 
        ggplot(aes(x = Średnie.tętno, y = Średnia.prędkość, color = Dystans, size = Dystans, text = paste("Average Pulse: ", Średnie.tętno,
                                                                                                          "<br>Average Speed: ", Średnia.prędkość,
                                                                                                          "<br>Distance: ",  Dystans, "km"))) +
        geom_point() +
        scale_color_gradient(low = "#ffb663", high = "#7C065C", name = "Distance [km]") +
        scale_size_continuous(range = c(1, 4), name = "Distance [km]") +
        labs(
          title = "Relation between Average Speed and Average Pulse",
          x = "Average Pulse",
          y = "Average Speed [km/h]"
        ) +
        guides(color = guide_legend(title = "Distance [km]")) +
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly(p, tooltip = c("text"))
      
    })
    
    
    output$individualOla2 <- renderPlotly({
      
      ActivitiesIndividual_Ola %>% 
        filter(ActivitiesIndividual_Ola$Year == 2023)  %>% 
        filter(Średni.rytm.pedałowania != 0 & Średnie.tętno != 0) -> activities_Ola_plot
      if(input$yearChoice == 2019){
        activities_Ola_plot %>% 
          filter(Dystans != 8.70) -> activities_Ola_plot
      }
      
      activities_Ola_plot$Data <- as.POSIXct(activities_Ola_plot$Data, format = "%Y-%m-%d %H:%M:%S")
      activities_Ola_plot %>% 
        arrange(Data)-> activities_Ola_plot
      
      activities_Ola_plot$Data <- format(activities_Ola_plot$Data, "%d %b")
      custom_order <- activities_Ola_plot$Data
      activities_Ola_plot$Data <- factor(activities_Ola_plot$Data, levels = custom_order)
      
      plot_ly(
        data = activities_Ola_plot, 
        x = ~Data,
        hoverinfo = 'none') %>%
        add_trace(y = 140, type = 'scatter', mode = 'lines', line = list(color = '#6D657C', width = 1, dash = 'dash'), showlegend = FALSE, hoverinfo = 'none') %>% 
        add_trace(y = 160, type = 'scatter', mode = 'lines', line = list(color = '#6D657C', width = 1, dash = 'dash'), showlegend = FALSE, hoverinfo = 'none') %>%
        add_trace(y = 150, type = 'scatter', mode = 'lines', line = list(color = "rgba(225, 227, 236, 0.5)", width = 74), name = "Aerobic training area") %>%
        add_lines(y = ~Średnie.tętno, name = "Average Pulse", line = list(color = "#ffb663", width = 3), 
                  text = paste("Date: ", activities_Ola_plot$Data, "<br>Distance: ", activities_Ola_plot$Dystans,
                               "<br>Time: ", activities_Ola_plot$Czas, "<br>Average pulse: ", activities_Ola_plot$Średnie.tętno), 
                  hoverinfo = 'text') %>%
        add_lines(y = ~Maksymalne.tętno, name = "Max Pulse", line = list(color = "#7C065C", width = 3), 
                  text = paste("Date: ", activities_Ola_plot$Data, "<br>Distance: ", activities_Ola_plot$Dystans,
                               "<br>Time: ", activities_Ola_plot$Czas, "<br>Max pulse: ", activities_Ola_plot$Maksymalne.tętno), 
                  hoverinfo = 'text') %>%
        layout(title = "Average and Max Pulse",
               xaxis = list(title = "Date", tickangle = -60, categoryorder = "array", categoryarray = custom_order),
               yaxis = list(title = "Pulse", range = c(125, 185)),
               margin = list(t = 70, b =100))
      
      
    })
    
    output$individualOla3 <- renderPlotly({
      
      ActivitiesIndividual_Ola %>% 
        filter(ActivitiesIndividual_Ola$Year == input$yearChoice)  %>% 
        filter(Średni.rytm.pedałowania != 0 & Średnie.tętno != 0) -> activities_Ola_plot
      if(input$yearChoice == 2019){
        activities_Ola_plot %>% 
          filter(Dystans != 8.70) -> activities_Ola_plot
      }
      
      activities_Ola_plot$Data <- as.POSIXct(activities_Ola_plot$Data, format = "%Y-%m-%d %H:%M:%S")
      activities_Ola_plot %>% 
        arrange(Data)-> activities_Ola_plot
      
      activities_Ola_plot$Data <- format(activities_Ola_plot$Data, "%d %b")
      custom_order <- activities_Ola_plot$Data
      activities_Ola_plot$Data <- factor(activities_Ola_plot$Data, levels = custom_order)
      
      my_color_scale = list(list(0,"#ffb663"),
                            list(1, "#7C065C"))
      
      plot_ly(
        data = activities_Ola_plot, 
        x = ~as.factor(Data),
        hoverinfo = 'none')%>%
        add_trace(y = ~Średni.rytm.pedałowania, name = "Average Pedaling Rhythm", type = "bar",
                 marker = list(color = ~Łącznie.obrotów, colorscale = my_color_scale,
                               colorbar = list(title = "Total Number of Turns",
                                               len = .6, outlinewidth = 0,
                                               tickfont = list(size = 10)))) %>%
        add_lines(y = ~Maksymalny.rytm.pedałowania, name = "Max Pedaling Rhythm", line = list(color = "#7C065C", width = 3),
                  text = paste("Date: ", activities_Ola_plot$Data, "<br>Distance: ", activities_Ola_plot$Dystans,
                               "<br>Time: ", activities_Ola_plot$Czas, "<br>Max pedaling rhythm: ", activities_Ola_plot$Maksymalny.rytm.pedałowania,
                               "<br>Average pedaling rhythm: ", activities_Ola_plot$Średni.rytm.pedałowania),
                  hoverinfo = "text") %>%
        layout(title = "Average and Max Pedaling Rhythm",
               xaxis = list(title = "Date", tickangle = -50, categoryorder = "array", categoryarray = custom_order),
               yaxis = list(title = "Number of turns per minute"),
               margin = list(t = 70, b = 90),
               showlegend = TRUE) 
      
      
    })
    output$individualOla4 <- renderPlotly({
      if(input$zmienna_heat_Ola == "Number of records"){
        output$individualOla4PlotTitle <- renderText({"Number of records in each month"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Ola") %>% group_by(Osoba,Month) %>% summarise(score=n())
      }
      if(input$zmienna_heat_Ola == "Distance (km)"){
        output$individualOla4PlotTitle <- renderText({"Distance (km) in each month"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Ola") %>% group_by(Osoba,Month) %>% summarise(score = sum(Dystans))
      }
      if(input$zmienna_heat_Ola == "Time (minutes)"){
        output$individualOla4PlotTitle <- renderText({"Time (minutes) in each month"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Ola") %>% group_by(Osoba,Month) %>% summarise(score = sum(Czas))
      }
      plot_ly(df, 
              x=~Month, 
              y=~score, 
              type="bar",
              hoverinfo = "text",
              text = paste("Month: ", month.name[df$Month], paste("<br>",input$zmienna_heat_Ola, ":"), df$score),
              textposition = "none",
              marker = list(color = "#7C065C")
      ) %>% 
        layout( 
          xaxis = list(title = "Month", tickformat=".d",tickvals = unique(df$Month), 
                       ticktext = month.abb[unique(df$Month)]),
          yaxis = list(title=input$zmienna_heat_Ola)
        )
    })

    output$individualOla5 <- renderPlotly({
      weekday_names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      
      if(input$zmienna_heat_Ola == "Number of records"){
        output$individualOla5PlotTitle <- renderText({"Number of records in each day"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Ola") %>% group_by(Osoba,Day) %>% summarise(score=n())
      }
      if(input$zmienna_heat_Ola == "Distance (km)"){
        output$individualOla5PlotTitle <- renderText({"Distance (km) in each day"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Ola") %>% group_by(Osoba,Day) %>% summarise(score = sum(Dystans))
      }
      if(input$zmienna_heat_Ola == "Time (minutes)"){
        output$individualOla5PlotTitle <- renderText({"Time (minutes) in each day"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Ola") %>% group_by(Osoba,Day) %>% summarise(score = sum(Czas))
      }
      plot_ly(df, 
              x=~Day, 
              y=~score, 
              type="bar",
              hoverinfo = "text",
              text = paste("Day: ", weekday_names[df$Day], paste("<br>",input$zmienna_heat_Ola, ":"), df$score),
              textposition = "none",
              marker = list(color = "#7C065C")
      ) %>% 
        layout( 
          xaxis = list(title = "Day", tickformat=".d",tickvals = unique(df$Day), 
                       ticktext = weekday_names[unique(df$Day)]),
          yaxis = list(title=input$zmienna_heat_Ola)
        )
    })
      
      ### Kuba ###
    
    output$magickGif<-renderImage({
      list(src="www/cyclist-3.gif",
           contentType = 'image/gif')
    },deleteFile = FALSE)
    
    track_info_data <-
      activities_Ola[c(74, 23, 54, 49, 2, 253, 169, 276, 109, 275, 307, 314, 319, 316, 318), ]
    track_info_data %>% mutate(
      choice_type = c(
        "Kuba_1",
        "Kuba_2",
        "Kuba_3",
        "Kuba_4",
        "Kuba_5",
        "Maciek_1",
        "Maciek_2",
        "Maciek_3",
        "Maciek_4",
        "Maciek_5",
        "Ola_1",
        "Ola_2",
        "Ola_3",
        "Ola_4",
        "Ola_5"
      ),
      time_h = DurationMinutes %/% 60,
      time_m = floor(DurationMinutes - time_h * 60),
      time_s = DurationMinutes * 60 - time_h * 3600 - time_m * 60
    ) -> track_info_data
    
    
    output$mapOfTrack <- renderLeaflet(
      {
        track_selected<-paste(input$track,"_",input$number_of_track,sep="")
        df_info_track<-track_info_data %>% filter(choice_type==track_selected)
        mydata<-read.csv(paste("data\\Top5_Activities\\activity_",input$track,"_",input$number_of_track,".csv",sep=""))
        mydata$ele<-mydata$speed
        hotlinePlugin <- htmltools::htmlDependency(
          name = 'Leaflet.hotline',
          version = "0.4.0",
          src = c(file = normalizePath("hotline_plugin")),
          script = "leaflet.hotline.js"
        )
        
        registerPlugin <- function( map, plugin ) {
          map$dependencies <- c( map$dependencies, list( plugin ) )
          map
        }
        palette<- colorNumeric(palette = colorRampPalette(c("#008800","#ffff00","#ff0000"))(5),
                               domain = 0:40)
        
        mydata %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km
        mydata %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km
        
        coords_of_kilometers<-last_lon_lat_each_km %>% left_join(avg_speed_per_km,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)
        
        paste("Kilometer: ","<strong>",coords_of_kilometers$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers$mean_speed,2),"</strong>",sep="") %>%
          lapply(htmltools::HTML) -> labels
        
        map_with_colors<-leaflet() %>% addTiles() %>%
          fitBounds( min(mydata$lon), min(mydata$lat), max(mydata$lon), max(mydata$lat) ) %>%
          registerPlugin(hotlinePlugin) %>%
          onRender("function(el, x, data) {
            data = HTMLWidgets.dataframeToD3(data);
            data = data.map(function(val) { return [val.lat, val.lon, val.ele]; });
            L.hotline(data, {min: 0, max: 40}).addTo(this);
          }", data = mydata ) %>% addLegend("bottomright",pal=palette,values=0:40,opacity=1,title="Speed [km/h]") %>% 
          addCircleMarkers(data=coords_of_kilometers,lat=~lat,lng=~lon,label=~labels,fillColor="#405CB9",fillOpacity=1,stroke=F,radius=6)
  #       map_with_colors %>% onRender(
  #         "
  #   function(el, x, df_info_track) {
  #     var info = L.control();
  # 
  #     info.onAdd = function(map) {
  #       this._div = L.DomUtil.create('div', 'info-box'); // tworzymy div z klasą 'info-box'
  #       this.update();
  #       return this._div;
  #     };
  # 
  #     info.update = function(df) {
  #       var currentData = HTMLWidgets.dataframeToD3(df); // Zakładam, że dane są jednolite na mapie
  # 
  #       this._div.innerHTML = '<h4>Informacje</h4>' +
  #                             '<p>Data: ' + currentData.Date + '</p>' +
  #                             '<p>Dystans: ' + currentData.Distance + ' km</p>' +
  #                             '<p>Czas: ' + currentData.DurationMinutes + ' minut</p>' +
  #                             '<p>Średnia prędkość: ' + currentData.AvgSpeed + ' km/h</p>';
  #     };
  # 
  #     info.addTo(this);
  #   }
  # ",df=df_info_track)
      }
    )
    output$homePageDescription<-renderUI({HTML("<div style='text-align: justify; font-size: 18px; margin: 20px;'>I have been planning this trip with my bike comrade Michał for a year. 
       We couldn’t find the time for it, but finally on the last day of summer holidays, on 31.08.2021 we managed to realize our “dream”. Until we reached Warka, we didn’t have much difficulties. 
       The troubles began after leaving the city. We did not know the route perfectly and even with GPS we had difficulties. We landed in forest two times, the road surface was terrible, we were moving with really low speed. 
       But in the end, we reached our destination. We didn’t have time to stay there, as the departure time of train was near.</div>")
    })
    output$top5description <- renderUI({
      HTML("<div style='text-align: justify;'>Explore our most interesting tracks. The map consists track route, which is coloured in various colors.
      Each color represents respective speed. Shades of green are used for presenting lower speeds, such as 0 km/h or 10 km/h. 
      Yellow and green color mixtures represent values of speed in range from 10 km/h to 20km/h. Yellow color corresponds to the value of 20km/h,
      red stands for every speed that is equal to 40 km/h or greater than it. Shades of mixed yellow and red are for values between 20 km/h and 40 km/h.</div>")
    })
    
    output$descriptionOfTrack <- renderUI({
      text_Kuba_1<-HTML("<div style='text-align: justify;'>Monday, 5th of September 2022 at first glance did not seem to become a day, when I set my longest bike trip.  
        The starting time of the ride did not indicate that it would be one of the most amazing trips. 
        I messaged my friend Michał, and we met each other on the Most Południowy at 1:00 PM. We planned to reach our beloved Góra Kalwaria and then decide what to do next. 
        Everything went as planned, we also had a decent average speed, the weather was fantastic, so we decided to extend our ride and go to Warka. 
        We have been there quite a few times, but the road to Warka is always really demanding. There are a lot of trucks, so you have to keep the pace and be really cautious. 
        It wasn’t easy at all, but we have made to the city. We stayed there for a several minutes, chilled a little bit, and then started coming back. The route to Warsaw was completed without any major obstacles. 
        We had a really good average speed, I was feeling that my muscles aren’t that tired, so I have decided to extend the ride in Warsaw. 
        I managed to do extra 50 kilometres, I was extremely exhausted, but also very happy of this achievement.</div>")
      
      text_Kuba_2<-HTML("<div style='text-align: justify;'>I have been planning this trip with my bike comrade Michał for a year. 
       We couldn’t find the time for it, but finally on the last day of summer holidays, on 31.08.2021 we managed to realize our “dream”. Until we reached Warka, we didn’t have much difficulties. 
       The troubles began after leaving the city. We did not know the route perfectly and even with GPS we had difficulties. We landed in forest two times, the road surface was terrible, we were moving with really low speed. 
       But in the end, we reached our destination. We didn’t have time to stay there, as the departure time of train was near.</div>")
      
      text_Kuba_3<-HTML("<div style='text-align: justify;'>The route to Warka on June 22nd 2022 was another trip to our liked place. 
        We started cycling around 10:00 AM. The weather was quite bearable, there was around 22°C. The trip was mostly peaceful. We did not have any troubles and difficulties. 
        We stayed and chilled in Warka for some time and managed to comeback to Warsaw without any unexpected adventures. In the meantime we had a stop at our favourite grocery shop in Góra Kalwaria. 
        We recharged our batteries and rode with quite good pace to our final destination.</div>")
      
      text_Kuba_4<-HTML("<div style='text-align: justify;'>This trip was the only one from considered routes, that I rode alone. It was just an ordinary trip do Góra Kalwaria. However I did it on Sunday, which was actually quite surprising, because I usually don’t go cycling on that day. 
        I felt power on that day, as the average speed shows –  26,2 km/h. I did the first part in less than hour, I felt huge exhaust, so the second part was much more difficult. 
        However I managed to return to home with a lot of effort, but the prize waiting in home – lying on the bed and resting – was infusing hope and peace.</div>")
      
      text_Kuba_5<-HTML("<div style='text-align: justify;'>The last trip is the oldest one – from 29 June 2021. It was not filled with high speed, however it was quite interesting. 
       Me and my two friends decided to visit Zalew Zegrzyński. We rode through Jabłonna and Legionowo quite peacefully. Then we reached Nieporęt, our expected destination. 
       Even though we did not have bath costumes, we have decided to splash a little. It was really hot, so the danger of catching a cold was rather negligible. However it wasn’t the end of bathing. 
       Our friend’s girlfriend called him and asked whether him whether we would like to join her in Zielonka. We were really enthusiastic about it and quickly rode there. We had quite a lot of fun there too and then we finally split up and ended in our homes. I managed to do 100 km, so that’s a lovely result. 
       Great fun, which was there will certainly stay in my heart for a long time.</div>")
      
      text_Maciek_1<-HTML("<div style='text-align: justify;'>On 3rd of June 2022 just after mature exams with my friends we decided to take a trip around the suburbs of Zamość to celebrate
      the end of the school year and spend some time on fresh air after months of studying. It took us about 7,5 hours and I buried almost 1700 kcal. 
      It was great experience because we were aware that we had 4 months of vacation ahead of us</div>")
      
      text_Maciek_2<-HTML("<div style='text-align: justify;'>This is just one of my favourite routes in Zamość. Take a look :)</div>")
      
      text_Maciek_3<-HTML("<div style='text-align: justify;'>On 28th of August 2023 I decided to do a challenge to beat my previous record in riding to my girlfriend as fast as possible.
      And so I did it. 9 kilometers with average speed 22 km/h. It was really challenging and in the end I was sweating so much that I had to take a
      shower and change clothes which were soaked. But it was fantastic day and I won't forget it.</div>")
      
      text_Maciek_4<-HTML("<div style='text-align: justify;'>On 29th of May 2020 I rode with a group of friends to the lagoon nearby Zamość. A really bumpy road with lots of challenging hills.
      As it was just after lockdown caused by COVID we were so happy to finally leave our houses and meet up for this adventure. The road shown on the chart
      is only one way. While we were returning it was significantly easier, because we were going downhill and all in all we made about 70 km this day.</div>")
      
      text_Maciek_5<-HTML("<div style='text-align: justify;'>And finally Warsaw. My first longer trip took place on 29th of July 2023. I transported my bike from my home town and started
      sightseeing the surroundings. A large part of the ride was in Wilanów, because I really enjoyed this district and couldn't leave it. All the time there was
      something interesting there that I wanted to check.</div>")
      
      text_Ola_1<-HTML("<div style='text-align: justify;'>On September 2nd, I decided to go for a bike ride along one of my favourite routes, known as the \"around the chimneys\" trail. 
        No, it doesn't mean that I passed by chimneys along the way. In my family, we refer to routes that lead through villages near our town as \"around the chimneys\" trails. 
        The route mainly follows side roads, so there isn't too much traffic on it, which makes this route really nice.</div>")
      
      text_Ola_2<-HTML("<div style='text-align: justify;'>A quick trip to the Wisła, unfortunately, this time without a break for splashing in the water. 
        This route is a very pleasant option for a slightly longer excursion. On the morning of August 13th, I took this route with my dad – a huge cycling enthusiast. 
        It was a very successful outing. Unfortunately, this route has one drawback – crossing a very busy street, which can be challenging and sometimes dangerous.</div>")
      
      text_Ola_3<-HTML("<div style='text-align: justify;'>A quick ride to Kołbiel with a satisfying average speed of 27.6 km/h. On the way to Kołbiel, 
        I rode along the express road S17, which wasn't the most enjoyable due to the noise from the busy street. However, on the way back home, 
        I took the route through the surrounding villages, which was significantly more pleasant.</div>")
      
      text_Ola_4<-HTML("<div style='text-align: justify;'>On July 23rd, I embarked on another cycling adventure, heading to Parysów, a beautiful town with a charming market square. 
        Just before entering the town, there's a steep hill, which climbing up is always a huge challenge. Afterward, I headed towards Michałówka. 
        Initially, I planned to return through Osieck. Unfortunately, it was a very hot July morning, and I could feel strength in my legs fading. 
        In Huta, I decided to shorten my route by a few kilometres and head back home.</div>")
      
      text_Ola_5<-HTML("<div style='text-align: justify;'>The cycling trip from June 11th is one of my favourites. Along the way, I encountered a group of cyclists who suggested that I join them. I gladly agreed. 
        Riding in a peloton is a completely different experience. Following behind other cyclists is much easier, and I don't tire as quickly. 
        The new friends turned out to be a very cool group of people. I hope we'll catch up on the road again!</div>")
      
      eval(parse(text=paste("text",input$track,input$number_of_track,sep="_")))
    })
    output$individualTextKuba1 <- renderUI({
      HTML("<div style='text-align: justify;'>Cycling for me was always a good form of spending my free time. 
      Back when I was younger I used to ride with my dad. 
      The routes weren’t as long as they became later, but I always treated cycling as a great form of relaxation.
      I changed my perspective in high school, when I met my friend Michał. 
      Our relation wasn’t connected in no way with riding a bike. Everything changed on 2020 summer. 
      We came with an idea of meeting each other and decided to try to ride to Góra Kalwaria. 
      This was a beginning of a beautiful story, the fruits of which you can see here.</div>")
      
    })
    output$individualTextKuba2 <- renderUI({
      HTML("<div style='text-align: justify;'>The first plot shows the relation between distance and actual spent time on riding a bike.   
      The y-axis value corresponds to fraction of the ride, in which speed was greater than zero. 
      So for example when my ride had a value of 79, this means that the 79% of the time I recorded activity were actually ridden. 
      The 21% of the time I spent on waiting for light change or I was just chilling and regenerating. You can explore how actual ride time corresponded to length of the ride.</div>")
      
    })
    output$individualTextKuba3 <- renderUI({
      HTML(
        "<div style='text-align: justify;'>
      Check out my elevation data from all of my rides. You may find it confusing at first glance to understand, what is presented here, 
        but I will try to explain you. This map shows all coordinates of places with maximum elevation  and minimum elevation. 
        Each marker is responsible for either maximum or minimum elevation on a certain ride. Yellow ones are for minimum elevation and red ones are for maximum elevation. 
        The bigger radius of marker, the higher elevation it is. For example for my ride near Baltic sea, you will notice both maximum and minimum elevation quite low, so as the radius is. 
        You can explore all of markers, by clicking on them. There are some interesting info about track data on them. 
        You also may notice that there are a lot of red markers near Góra Kalwaria. That’s beacause I used to ride there a lot..</div>"
      )
      
    })
    output$individualPlotKuba1<-renderPlotly({
      read.csv("data\\activities_JP.csv")->dane_JP
      
      dane_JP %>% filter(Activity.Type=="Ride") %>% 
        mutate(percent_of_ridden_time=Moving.Time/Elapsed.Time,
               length_of_track=ifelse( Distance<40,"Short",ifelse(Distance<70,"Medium","Long"))) -> sml_elapsed_moving_time
      
      plot_ly(data=sml_elapsed_moving_time,
              x=~Distance,
              y=~(100*percent_of_ridden_time),
              color=~length_of_track,colors=c("#53296E", "#6B82DB", "#f9a13c"),
              hoverinfo = "text",
              text = paste("Distance: ",sml_elapsed_moving_time$Distance," km", "<br> Percent of ridden time: ",round(100*sml_elapsed_moving_time$percent_of_ridden_time,2),"%" ,"<br> Track type: ",sml_elapsed_moving_time$length_of_track,sep=""),
              textposition = "none")%>%
        layout(title="Relation between distance and actual time spend on the ride",
                 yaxis=list(title="Percent of time spent on the ride"))
    })
    output$individualPlotKuba2<-renderLeaflet({
      max_elevations<-read.csv("data\\max_elevations_JP.csv")
      leaflet() %>% addTiles() %>% addCircleMarkers(data = max_elevations,lng=~lon,lat=~lat,radius=~(ele/10),color= ifelse(max_elevations$min_max=="min","yellow","red"),fillColor = ifelse(max_elevations$min_max=="min","yellow","red"),fillOpacity = 0.8,popup = paste0(
        "<strong>Date: </strong>", max_elevations$activity_date, "<br>",
        "<strong>Distance (km): </strong>", max_elevations$distance, "<br>",
        "<strong>Time : </strong>", seconds_to_period(max_elevations$time), "<br>",
        "<strong>Mean speed (km/h): </strong>", round(max_elevations$mean_speed*3.6,2), "<br>", 
        "<strong>Minimum/Maximum elevation: </strong>", paste(max_elevations$min_max,": ",max_elevations$ele," m",sep=""), "<br>"))
      })
    output$individualKuba4 <- renderPlotly({
      if(input$zmienna_heat_Kuba == "Number of records"){
        output$individualKuba4PlotTitle <- renderText({"Number of records in each month"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Kuba") %>% group_by(Osoba,Month) %>% summarise(score=n())
      }
      if(input$zmienna_heat_Kuba == "Distance (km)"){
        output$individualKuba4PlotTitle <- renderText({"Distance (km) in each month"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Kuba") %>% group_by(Osoba,Month) %>% summarise(score = sum(Dystans))
      }
      if(input$zmienna_heat_Kuba == "Time (minutes)"){
        output$individualKuba4PlotTitle <- renderText({"Time (minutes) in each month"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Kuba") %>% group_by(Osoba,Month) %>% summarise(score = sum(Czas))
      }
      plot_ly(df, 
              x=~Month, 
              y=~score, 
              type="bar",
              hoverinfo = "text",
              text = paste("Month: ", month.name[df$Month], paste("<br>",input$zmienna_heat_Kuba, ":"), df$score),
              textposition = "none",
              marker = list(color = "#F9B330")
      ) %>% 
        layout( 
          xaxis = list(title = "Month", tickformat=".d",tickvals = unique(df$Month), 
                       ticktext = month.abb[unique(df$Month)]),
          yaxis = list(title=input$zmienna_heat_Kuba)
        )
    })
    
    output$individualKuba5 <- renderPlotly({
      weekday_names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      
      if(input$zmienna_heat_Kuba == "Number of records"){
        output$individualKuba5PlotTitle <- renderText({"Number of records in each day"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Kuba") %>% group_by(Osoba,Day) %>% summarise(score=n())
      }
      if(input$zmienna_heat_Kuba == "Distance (km)"){
        output$individualKuba5PlotTitle <- renderText({"Distance (km) in each day"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Kuba") %>% group_by(Osoba,Day) %>% summarise(score = sum(Dystans))
      }
      if(input$zmienna_heat_Kuba == "Time (minutes)"){
        output$individualKuba5PlotTitle <- renderText({"Time (minutes) in each day"})
        df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Kuba") %>% group_by(Osoba,Day) %>% summarise(score = sum(Czas))
      }
      plot_ly(df, 
              x=~Day, 
              y=~score, 
              type="bar",
              hoverinfo = "text",
              text = paste("Day: ", weekday_names[df$Day], paste("<br>",input$zmienna_heat_Kuba, ":"), df$score),
              textposition = "none",
              marker = list(color = "#F9B330")
      ) %>% 
        layout( 
          xaxis = list(title = "Day", tickformat=".d",tickvals = unique(df$Day), 
                       ticktext = weekday_names[unique(df$Day)]),
          yaxis = list(title=input$zmienna_heat_Kuba)
        )
    })
      
      
      ### Maciek ###
      output$competition1 <- renderPlotly({
        if(input$zmienna == "Distance (km)"){
          m = "Dystans"
          output$competition1PlotTitle <- renderText({
            "Total distance over the years"
          })
          df <- ActivitiesTogether %>% filter(Rok >= input$zakres[1],
                                              Rok <= input$zakres[2]) %>%
            group_by(Osoba) %>% 
            summarize(score = sum(!!sym(m)))
          yaxisRange = c(0,15000)
        }
        if(input$zmienna == "Time (minutes)"){
          m = "Czas"
          output$competition1PlotTitle <- renderText({
            "How much time have we spent cycling over the years (in minutes)"
          })
          df <- ActivitiesTogether %>% filter(Rok >= input$zakres[1],
                                              Rok <= input$zakres[2]) %>%
            group_by(Osoba) %>% 
            summarize(score = sum(!!sym(m)))
          yaxisRange = c(0,25000)
        }
        if(input$zmienna == "Average Speed (km/h)"){
          m = "ŚredniaPrędkość"
          output$competition1PlotTitle <- renderText({
            "Our average speed across all rides"
          })
          df <- ActivitiesTogether %>% filter(Rok >= input$zakres[1],
                                              Rok <= input$zakres[2]) %>%
            group_by(Osoba) %>% 
            summarize(score = mean(!!sym(m)))
          yaxisRange = c(0,50)
        }
      
      plot_ly(df,
              x=~Osoba,
              y=~score,
              color=~Osoba,
              hoverinfo = "text",
              text = paste("Person: ", df$Osoba, paste("<br>",input$zmienna, ":"), df$score),
              textposition = "none",
              colors = c("#F9B330","#263672", "#7C065C"),
              type = "bar") %>% 
        layout(xaxis = list(title = "Person"), yaxis = list(title = input$zmienna,tickformat = ",d",range=yaxisRange))
      })
      
      output$competitiondescribtion <- renderUI({
        HTML("<div style='text-align: justify;'>Competition tab is where our activity is summarised. Here we compare 
        ourselves depending on total distance, average speed or time
        spent on cycling in minutes.</div>")
      })
      
      output$competitionInteractivityDesc <- renderUI({
        HTML("<div style='text-align: justify;'>Explore our scores with those two charts. The bar chart consists summarized score in
        selected period of time while linear chart shows the progress throughout years. You can easily change 
        considering topic on the top of this text and select years with slider. (Note that slider doesn't affect 
        linear chart. Otherwise this chart would make no sense)</div>")
        
      })
      #competition2
      output$competition2 <- renderPlotly({
        if(input$zmienna == "Distance (km)"){
          m = "Dystans"
          df <- ActivitiesTogether %>% filter(Rok >=2020) %>%
            group_by(Osoba,Rok) %>% 
            summarize(score = sum(!!sym(m)))
        }
        if(input$zmienna == "Time (minutes)"){
          m = "Czas"
          df <- ActivitiesTogether %>% filter(Rok >=2020) %>%
            group_by(Osoba,Rok) %>% 
            summarize(score = sum(!!sym(m)))
        }
        if(input$zmienna == "Average Speed (km/h)"){
          m = "ŚredniaPrędkość"
          df <- ActivitiesTogether %>% filter(Rok >=2020) %>%
            group_by(Osoba,Rok) %>% 
            summarize(score = mean(!!sym(m)))
        }
        plot_ly(df,
                x=~Rok,
                y=~score,
                type="scatter",
                mode="lines",
                hoverinfo = "text",
                text = paste("Person: ", df$Osoba, paste("<br>",input$zmienna, ":"), df$score, "<br>Year: ", df$Rok),
                textposition = "none",
                line = list(width = 4),
                color = ~Osoba,
                colors = c("#F9B330","#263672", "#7C065C")
        ) %>% layout(
          yaxis = list(title = input$zmienna),
          xaxis = list(title = "Year",tickmode = "array", tickvals = unique(df$Rok), 
                       ticktext = unique(df$Rok))
        )
      })
      
      output$individualMaciek1 <- renderPlotly({
        df <- Kroki_Calorie
        colnames(df)[2] <- "Steps"
        colnames(df)[3] <- "Calories"
        ggplotly(df  %>% filter(Calories <= input$limitKcal) %>%  ggplot(aes(x=Steps,y=Calories)) + stat_density2d(geom="tile", aes(fill = after_stat(density)), contour = FALSE) +
          geom_point(colour = "white") + theme_minimal() +
          geom_smooth() +  
          theme(legend.position = "none") + labs(x = "Steps", y= "Calories (kcal)")) %>%
          layout(xaxis = list(rangeslider = list(type = "Steps")))
      })
      output$individualMaciek1Desc <- renderUI({
        HTML("<div style='text-align: justify;'>Let's look at the chart on the right. You can manipulate the x axis by slider and y axis by typing the limit.
        The chart shows how calories change depending on the amount of steps. Each white dot shows calories buried and steps made in each day. 
        If we properly check the smaller values as Steps <= 20000 and Calories <= 500 we would see the linear dependence
        highlighted. This may suggest that those records are days in which I did not ride a bike and I was only walking, whereas records with
        less steps and more calories buried are days in which I did excercise on bike and did not walk that much.</div>")
      })
      
      output$individualMaciek2 <- renderPlotly({
          df <- Kroki_Calorie %>%
          mutate(miesiac = factor(month(recordDay)), rok=factor(year(recordDay))) %>% 
          filter(rok != "2019") %>% 
          group_by(miesiac, rok) %>% summarise(n = sum(Kroki)) 
          plot_ly(df, x=~miesiac,y=~n,type="bar", color =~factor(rok), colors = c("#263672", "#5A78CD", "#9F3BAF", "#DC2AA9"),
                  hoverinfo = "text",
                  text = paste("Month: ",df$miesiac,", Year: ", df$rok, "<br>Steps: ", df$n),
                  textposition = "none",
                  marker = list(colorscale = 'category10')) %>% 
          layout(yaxis = list(title = "Steps",tickformat = ".d"),
                 xaxis = list(title="month"),
                 barmode = "stack")
      })
      
      output$individualMaciek2Desc <- renderUI({
        HTML("<div style='text-align: justify;'>This chart shows us how amount of steps depend on the month. As you can notice there is a significant difference between summer months
        and month in winter. I walk most often during summer break in June,July,August and September. As you can also see in 2020 I used to take walks
        almost in every month and I made more steps comparing to other years. That may be due to more freetime and less responibilities.</div>")
      })
      
      output$individualMaciek3 <- renderPlotly({
        df <- HeartRate_Maciek %>%
          mutate(mean_pulse = mean(avgPulse),rok = factor(year(recordDay)), miesiac = factor(month(recordDay))) %>% 
          group_by(rok, miesiac) %>% summarise(average_pulse = mean(avgPulse)) %>% mutate(recordTime = sprintf("%s-%02d-01", rok, miesiac))
        df$recordTime <- as.POSIXct(df$recordTime, format = "%Y-%m-%d")
          # HeartRate_Maciek %>%
          #          mutate(mean_pulse = mean(avgPulse),rok = factor(year(recordDay))) %>% 
          #          filter(rok != "2019") %>% 
          #          ggplot(aes(x = recordDay, y = avgPulse)) + 
          #          scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) + 
          #          geom_line(color = "darkred") +
          #          geom_hline(aes(yintercept = mean_pulse, color = "Average overall"), linetype = "dashed", size = 1) +
          #          labs(x = "",
          #               y = "beats per minute",
          #               color = "") +
          #          theme_minimal() +
          #          theme(axis.text.x = element_text(angle = 45, hjust = 1))
          plot<-df %>% filter(rok != "2019") %>% 
            ggplot(aes(x = recordTime, y = average_pulse)) + 
            scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) + 
            geom_line(color = "darkred") +
            labs(x = "Date",
                 y = "Beats per minute",
                 color = "") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
          ggplotly(plot)
          
      })
      
      output$individualMaciek3Desc <- renderUI({
        HTML("<div style='text-align: justify;'>The chart below shows the change of my heart rate in each month of the years during excercising.</div>")
      })
      
      output$individualMaciek4 <- renderPlotly({
        if(input$zmienna_heat_Maciek == "Number of records"){
          output$individualMaciek4PlotTitle <- renderText({"Number of records in each month"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Month) %>% summarise(score=n())
        }
        if(input$zmienna_heat_Maciek == "Distance (km)"){
          output$individualMaciek4PlotTitle <- renderText({"Distance (km) in each month"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Month) %>% summarise(score = sum(Dystans))
        }
        if(input$zmienna_heat_Maciek == "Time (minutes)"){
          output$individualMaciek4PlotTitle <- renderText({"Time (minutes) in each month"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Month) %>% summarise(score = sum(Czas))
        }
        plot_ly(df, 
                x=~Month, 
                y=~score, 
                type="bar",
                hoverinfo = "text",
                text = paste("Month: ", month.name[df$Month], paste("<br>",input$zmienna_heat_Maciek, ":"), df$score),
                textposition = "none",
                marker = list(color = "#263672")
        ) %>% 
          layout( 
            xaxis = list(title = "Month", tickformat=".d",tickvals = unique(df$Month), 
                         ticktext = month.abb[unique(df$Month)]),
            yaxis = list(title=input$zmienna_heat_Maciek)
          )
      })

      
      output$individualMaciek5 <- renderPlotly({
        weekday_names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
        
        if(input$zmienna_heat_Maciek == "Number of records"){
          output$individualMaciek5PlotTitle <- renderText({"Number of records in each day"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Day) %>% summarise(score=n())
        }
        if(input$zmienna_heat_Maciek == "Distance (km)"){
          output$individualMaciek5PlotTitle <- renderText({"Distance (km) in each day"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Day) %>% summarise(score = sum(Dystans))
        }
        if(input$zmienna_heat_Maciek == "Time (minutes)"){
          output$individualMaciek5PlotTitle <- renderText({"Time (minutes) in each day"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Day) %>% summarise(score = sum(Czas))
        }
        plot_ly(df, 
                x=~Day, 
                y=~score, 
                type="bar",
                hoverinfo = "text",
                text = paste("Day: ", weekday_names[df$Day], paste("<br>",input$zmienna_heat_Maciek, ":"), df$score),
                textposition = "none",
                marker = list(color = "#263672")
        ) %>% 
          layout( 
            xaxis = list(title = "Day", tickformat=".d",tickvals = unique(df$Day), 
                         ticktext = weekday_names[unique(df$Day)]),
            yaxis = list(title=input$zmienna_heat_Maciek)
          )
      })
      
    
}

shinyApp(ui = ui, server = server)
