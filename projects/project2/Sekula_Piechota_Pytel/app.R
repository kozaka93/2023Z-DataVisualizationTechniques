### libraries and packages
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(extrafont)
library(showtext)
library(lubridate)
library(scales)
### fonts 

# import fonts - takes ~5 minutes 
# font_import()

# default font
#loadfonts() 
#showtext_auto()
#font_add("Inter", regular = "C:/my_repos/PersonalVisualizationProject/Inter/Inter-VariableFont_slnt,wght.ttf")


### theme

theme_blue <- shinyDashboardThemeDIY(
  appFontFamily = "Helvetica" 
  ,appFontColor = "#edc9c7"
    ,primaryFontColor = "#edc9c7"
    ,infoFontColor = "#edc9c7"
    ,successFontColor = "#edc9c7"
    ,warningFontColor = "#edc9c7"
    ,dangerFontColor = "#edc9c7"
    
  ,bodyBackColor = "#068fa4" 
    ,logoBackColor = "#068fa4" 
    
  ,headerButtonBackColor = "#068fa4" # kolor  na ikonce chowania
    ,headerButtonIconColor = "#edc9c7" # kolor pasków na ikonce chowania sidebaru
    ,headerButtonBackColorHover = "#edc9c7" # hover ikonki chowania
    ,headerButtonIconColorHover = "#068fa4" # kolor paskow podczas hovera
    ,headerBackColor = "#068fa4" # header
    ,headerBoxShadowColor = "" #cien pod headerem - nie zmieniac
  ,headerBoxShadowSize = "0px 0px 0px"  # nie zmieniac
  
  
  ,sidebarBackColor = "#068fa4"
    ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 5
  ,sidebarMenuBorderRadius = 1
  
  ,sidebarShadowRadius = "" 
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#edc9c7"
    
  ,sidebarSearchBackColor = "#edc9c7"
    ,sidebarSearchIconColor = "#068fa4"
    ,sidebarSearchBorderColor = "#edc9c7"
    
  ,sidebarTabTextColor = "#edc9c7"
    ,sidebarTabTextSize = 20
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "#068fa4"
    ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "#edc9c7"
    ,sidebarTabTextColorSelected = "#068fa4" 
    ,sidebarTabRadiusSelected = "20px" 
  
  ,sidebarTabBackColorHover = "#edc9c7"
    ,sidebarTabTextColorHover = "#068fa4"
    ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "20px" 
  
  ,boxBackColor = "#068fa4" 
    ,boxBorderRadius = 1
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 36
  ,boxDefaultColor = "#068fa4"
    ,boxPrimaryColor = "#068fa4"
    ,boxInfoColor = "#068fa4"
    ,boxSuccessColor = "#068fa4"
    ,boxWarningColor = "#068fa4"
    ,boxDangerColor = "#068fa4"
    
  ,tabBoxTabColor = "#068fa4"
    ,tabBoxTabTextSize = 20
  ,tabBoxTabTextColor = "#068fa4"
    ,tabBoxTabTextColorSelected = "#068fa4"
    ,tabBoxBackColor = "#edc9c7"
    ,tabBoxHighlightColor = "#edc9c7"
    ,tabBoxBorderRadius = 1
  
  ,buttonBackColor = "#068fa4"
    ,buttonTextColor = "#edc9c7"
    ,buttonBorderColor = "#edc9c7"
    ,buttonBorderRadius = 1
  
  ,buttonBackColorHover = "#068fa4"
    ,buttonTextColorHover = "#edc9c7"
    ,buttonBorderColorHover = "#edc9c7"
    
  ,textboxBackColor = "#068fa4" 
    ,textboxBorderColor = "#edc9c7" 
    ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "#068fa4"
    ,textboxBorderColorSelect = "#edc9c7"
    
    
  ,tableBackColor = "#068fa4"
    ,tableBorderColor = "#edc9c7"
    ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

plot_theme <- theme(panel.grid = element_line(colour = "#edc9c7",linetype = "dotted"),
                    panel.background = element_rect(fill = "transparent", color = 'transparent'),
                    plot.background = element_rect(fill = "#068fa4", color = 'transparent'),
                    plot.title = element_text(face = "bold", size = 20, hjust = 0.5,
                                              color = "#edc9c7", family = "Helvetica"),
                    axis.text = element_text(color = "#edc9c7", size = 10, family = "Helvetica"),
                    axis.title = element_text(color = "#edc9c7", size = 12, hjust = 0.5, family = "Helvetica"),
                    legend.key = element_rect(fill = "#068fa4", color = "transparent"),
                    legend.title = element_text(color = "#edc9c7", size = 16, family = "Helvetica"),
                    legend.background = element_rect(fill = "#068fa4", color = 'transparent'),
                    legend.text = element_text(color = "#edc9c7", size = 12, family = "Helvetica")) 



### server

server <- function(input, output, session) {
  
  print("Server function is running!")  # Add more print statements as needed
  
  steps_df <- read.csv("data/final/stepsdf.csv")
  water_df <- read.csv("data/final/waterdf.csv")
  sleep_df <- read.csv("data/final/sleepdf.csv")
  ### HOME PAGE ###
  
  ### SLEEP ###
  
  ## wykres boxplot liczba godzin snu w zależności od dnia tygodnia 
  output$sleepGentleman <- renderUI({
    selectInput(
      inputId = "sleepGentleman",
      label = "Person:",
      choices = unique(sleep_df$name)
    )
  })
  
  
  output$plotSleepGentleman <- renderPlotly({
    selected_gentleman <- input$sleepGentleman
    
    if (is.null(selected_gentleman)) {
      return(NULL)  # Handle the case where no Gentleman is selected
    }
    
    p <- sleep_df %>%
      filter(name == selected_gentleman) %>%
      ggplot(aes(x = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
                 y = hm(duration), fill = selected_gentleman)) +
      geom_boxplot() +
      scale_y_continuous(labels = function(x) sprintf("%.1f h", x / 3600),breaks =seq(0, 24*3600, by = 3600)) + 
      labs(x = "Weekday",
           y = "Duration",
           title = paste0("Distribution of sleep duration for ", selected_gentleman)) +
      scale_fill_manual(values = c("Gentleman1" = "#B74F6F", "Gentleman2" = "#59608E", "Gentleman3" = "#F39C3F")) +
      plot_theme +
      guides(fill = FALSE)
    
    ggplotly(p)
  })
  
  output$textSleepGentleman <- renderText({
    "How long do we sleep. This boxplot shows the distribution of sleep by the weekday for a chosen Gentleman.
    This gives an insight on what are our sleep tendencies. This is one of the most important factors regarding our health, so this makes it important to keep this around 7 to 9 hours."
  })
  
  
  ## wykres kolumnowy godzina rozpoczęcia snu w zal. od dnia tygodnia ??
  
  # output$colPlotSleepStart <- renderPlot({
  #   ggplot()
  # })
  
  
  
  output$histSleepStart <- renderPlotly({
    p <- sleep_df %>%
      filter(name == input$sleepGentleman) %>%
      ggplot(aes(x = hour(ymd_hms(start_time)), fill = input$sleepGentleman)) +
      geom_histogram(bins = 48) +
      scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d:00", seq(0, 23, by = 1))) +
      labs(x = "Hour of the Day",
           y = "Count",
           title = paste0("Distribution of sleep start times for ", input$sleepGentleman)) +
      scale_fill_manual(values = c("Gentleman1" = "#B74F6F", "Gentleman2" = "#59608E", "Gentleman3" = "#F39C3F")) +
      plot_theme +
      guides(fill = FALSE)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  
  
  output$textSleepGentleman2 <- renderText({
    "Uncover the diverse going to sleep habits of the distinctive personalities within the 'ME project'—Gentleman1, Gentleman2, and Gentleman3—with this interactive histogram. This visual representation displays the distribution of wake-up times throughout a 24-hour period, allowing you to explore the nuances of their evening routines."
  })
  
  

  
  
  output$histSleepEnd <- renderPlotly({
    p <- sleep_df %>%
      filter(name == input$sleepGentleman) %>%
      ggplot(aes(x = hour(ymd_hms(end_time)), fill = input$sleepGentleman)) +
      geom_histogram(bins = 48) +
      scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d:00", seq(0, 23, by = 1)),
                         limits = c(0, 23)) +
      labs(x = "Hour of the Day",
           y = "Count",
           title = paste0("Distribution of sleep end times for ", input$sleepGentleman)) +
      scale_fill_manual(values = c("Gentleman1" = "#B74F6F", "Gentleman2" = "#59608E", "Gentleman3" = "#F39C3F")) +
      plot_theme +
      guides(fill = FALSE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees
    
    ggplotly(p)
  })
  
  
  
  
  
  output$textSleepGentleman3 <- renderText({
    "Uncover the diverse wake-up habits of the distinctive personalities within the 'ME project'—Gentleman1, Gentleman2, and Gentleman3—with this interactive histogram. This visual representation displays the distribution of wake-up times throughout a 24-hour period, allowing you to explore the nuances of their morning routines."
  })
  ## wykres gęstosci godzina zakończenia snu w zal. od dnia tygodnia
  
  # output$densityPlotSleepEnd <- renderPlot({
  #   ggplot()
  # })
  
  ### STEPS ###
  
  ## wykres kolumnowy kroków dla poszczególnych dni
  
  output$colPlotSteps <- renderPlotly({
    weekdays <- distinct(steps_df[, c("date", "weekday")])
    
    
    p <- steps_df  %>% 
      group_by(date, name) %>%
      summarise(total_steps = sum(count)) %>%
      inner_join(weekdays) %>% 
      group_by(weekday, name) %>% 
      summarise(avg_steps = mean(total_steps)) %>% 
      ggplot(aes(fill = name,
                 x = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
                 y = avg_steps,
                 text = paste("Person: ", name, "<br>Weekday: ", weekday, "<br>Average Steps: ", avg_steps))) +
      geom_bar(position="dodge", stat="identity") +
      scale_fill_manual(values = c("Gentleman1" = "#B74F6F", "Gentleman2" = "#59608E", 
                                   "Gentleman3" = "#F39C3F"),
                        name = "Person:")+
      labs(title = "Number of steps by day",
           x = "Weekday",
           y = "Steps")+
      plot_theme
    
    ggplotly(p) %>%
      add_trace(
        hoverinfo = 'text'
      )
  })
  
  output$textBarSteps <- renderText({
    "This grouped bar plot showcases the average number of steps taken by three individuals over the course of a week.
     Each weekday is represented by a distinct bar, and the height of each bar corresponds to the average number of steps
     taken by each Gentleman on that specific day. The plot provides a comprehensive overview of their weekly step 
     patterns, offering insights into their activity levels throughout the weekdays."
  })
  
  ## wykres krokow dla dat z interaktywnym sliderem
  
  output$colPlotStepsByDate <- renderPlotly({
    steps_modified <- steps_df %>% 
      group_by(date, name) %>% 
      summarise(total_steps = sum(count)) 
      #filter(name %in% selected_gentlemen)
    
    p <- ggplot(steps_modified[-1,], aes(x = date, y = total_steps, color = name)) + 
      geom_line(aes(group=1)) +
      geom_point() + 
      plot_theme +
      scale_color_manual(values = c("Gentleman1" = "#B74F6F", "Gentleman2" = "#59608E", 
                                    "Gentleman3" = "#F39C3F"),
                         name = "Person:") +
      labs(title = "Number of steps by day",
           x = "Weekday",
           y = "Steps") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% 
      layout(xaxis = list(rangeslider = list(type = "date")))
    
  })
  
  output$textLineSteps <- renderText({
    "This dynamic line plot presents the daily step counts for the time period spanning
     from December 17th to January 22nd. The plot is designed with an interactive X axis,
     allowing users to choose and adjust the time interval for a customized view of the data.
     Each date on the plot is associated with a specific step count, providing a detailed representation
     of the individuals' daily activity levels. The interactive feature enhances the user experience by
     enabling a more personalized exploration of the step data over the selected time frame."
  })
  
  ### WATER ###
  
  ## wykres korelacji woda/kroki
  
  output$pointWaterStepsGents <- renderUI({
    checkboxGroupInput("gentlemanCheckbox",
                       label = "Select Gentlemen",
                       choices = c("Gentleman1", "Gentleman2", "Gentleman3"),
                       selected = c("Gentleman1", "Gentleman2", "Gentleman3"))
  })
  
  output$pointWaterStepsWdDay <- renderUI({
    checkboxGroupInput("weekdayCheckbox",
                       label = "Select Weekdays",
                       choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                       selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  })
  
  
  output$pointPlotWaterSteps <- renderPlotly({
    
    selected_gentlemen <- input$gentlemanCheckbox
    selected_weekdays <- input$weekdayCheckbox
    
    steps_modified <- steps_df %>% 
      group_by(date, weekday) %>% 
      summarise(total_steps = sum(count))
    
    filtered_data <- steps_modified %>% 
      left_join(water_df, by = "date") %>%
      filter(name %in% selected_gentlemen & weekday.x %in% selected_weekdays)
    
    
    p <- ggplot(filtered_data, aes(x = total_steps, y = amount, color = name)) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Gentleman1" = "#B74F6F", "Gentleman2" = "#59608E", 
                                    "Gentleman3" = "#F39C3F"),
                         name = "Person:") +
      labs(title = "Correlation between daily steps and water intake",
           x = "Total daily steps",
           y = "Amount of water consumed, in ml") +
      plot_theme
    
    ggplotly(p)
    
    
  })
  
  output$textWaterSteps <- renderText({
    "The scatter plot examines whether there is a relationship between the daily step count and water 
    intake among three individuals. Each point on the plot represents a day, with the x-axis showing 
    the total daily steps and the y-axis displaying the amount of water consumed in milliliters. 
    By selecting specific gentlemen and weekdays through checkboxes, you can explore potential correlations 
    and patterns between walking activity and water consumption."
  })
  
  
  ## wykres boxplot/violinplot woda w zal. od dnia tygodnia
  
  output$waterDayOfWeek <- renderUI({
    selectInput(
      inputId = "waterDayOfWeek",
      label = "Choose day of week:",
      choices = unique(water_df$weekday)
    )
  })
  
  
  output$plotWaterWeekday <- renderPlotly({
    p1 <- ggplot() +
      geom_violin(data = water_df, aes(x = name, y = amount, fill = name), alpha = 0.5)+
      geom_point(data = water_df %>% filter(weekday == input$waterDayOfWeek), aes(x=name, y=amount), size=3, color = "#edc9c7")+
      ylim(c(500, 2500))+
      scale_fill_manual(values = c("Gentleman1" = "#B74F6F", "Gentleman2" = "#59608E", "Gentleman3" = "#F39C3F"),
                        name = "Person:")+
      labs(title = paste0("Water consumption on ", input$waterDayOfWeek, "s"),
           x = "",
           y = "Amount of water consumed, in ml")
    
    p2 <- p1 + plot_theme
    
    ggplotly(p2)
    
  })
  
  output$textWaterWeekday <- renderText({
    "Is there a weekday on which we have tendency to drink the most water? \n
    The plot visualizes the distribution of water consumption across our beloved individuals 
    (Gentleman1, Gentleman2, and Gentleman3) on a specific day of the week selected below. 
    The violin plot displays the probability density of water intake on all days, 
    while the individual data points highlight the actual values for each person for selected weekday.

 "
  }) 
  
  output$textHomeAboutUs <- renderUI({
    HTML("Embark on a journey of self-discovery and transformation with the remarkable individuals behind the 'ME project.'
  Meet Gentleman1, Gentleman2, and Gentleman3 – three distinct personalities united by a common quest for personal growth and well-being.<br>
  For G1, a healthy lifestyle is very important, including regular exercise, regular and healthy meals or a long sleep of more than eight hours. 
  Gentleman2 is also trying to sleep at least 8 hours and it is important for him to go for a walk everyday.
  Meanwhile, Gentleman3 is a runner at heart, so his daily steps may outstand. <br>
  Together, they invite you to delve into their individual narratives, sharing insights, challenges, and triumphs on the path to a better version of themselves.
  The 'ME project' is not just a journey; it's an exploration of the intricate facets that make them who they are.
  Join us as we witness the unfolding chapters of self-awareness and well-being in the lives of Gentleman1, Gentleman2, and Gentleman3 – the driving forces behind the 'ME project.'")
  })
  
  
  
  output$textHomeMethods <- renderUI({
    HTML("In the pursuit of understanding and enhancing our well-being, we embarked on a health data collection journey through Samsung Health. Commencing from the early days of December 23, our endeavor continued until the dawn of January 2024. Throughout this period, we meticulously gathered and analyzed data pertaining to three vital aspects of our health: steps taken, water intake, and sleep duration.

This initiative aimed not only to capture the quantitative metrics of our daily activities but also to uncover potential correlations and patterns that could provide valuable insights into our overall health and lifestyle. The collected data spans across various dimensions, allowing us to delve into the nuances of our physical activity, hydration levels, and the quality of our sleep.

Through the integration of cutting-edge technology and the commitment to personal well-being, we present a comprehensive exploration into the intricacies of our health data. Join us as we navigate through the rich tapestry of information gathered from Samsung Health, unraveling the story of our daily steps, water consumption habits, and the rejuvenating hours we devote to restful sleep.
")
  })
  
  
}


### UI

## header

header <- dashboardHeader(
  title = "Health data analysis"
  
)

## sidebar

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home")),
    menuItem("Sleep", tabName = "Sleep", icon = icon("bed")),
    menuItem("Steps", tabName = "Steps", icon = icon("heartbeat")),
    menuItem("Water", tabName = "Water", icon = icon("tint"))
  ),
  width = 250
  
)

## body

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "Home",
      
      fluidRow(
        box(title = "About Collecting Data", uiOutput("textHomeMethods"))
        
      ),
      br(),
      br(),
      br(),
      fluidRow(
        box(title = "About Us", uiOutput("textHomeAboutUs")),
      )
      
    ),
    tabItem(
      tabName = "Water",
      
      fluidRow(
        box(title = "Who of us drinks the most (water) on Fridays?"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("plotWaterWeekday"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        ),
        column(width = 3,
               textOutput("textWaterWeekday"),
               br(),
               uiOutput("waterDayOfWeek"))
        
      ),
      br(),
      br(),
      fluidRow(
        box(title = "Do we drink more water if we walk more?"),
        column(width = 8, 
               shinycssloaders::withSpinner(plotlyOutput("pointPlotWaterSteps"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))),
        column(width = 4,
               textOutput("textWaterSteps"),
               br(),
               uiOutput("pointWaterStepsGents"),
               br(),
               uiOutput("pointWaterStepsWdDay")
               
               
        )
      )),
    tabItem(
      tabName = "Steps",
      fluidRow(
        box(title = "What days do we walk the most?"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("colPlotSteps"),
                                            type = getOption("spinner.type", default = 5))),
        column(width = 3,
               textOutput("textBarSteps"))
        ),
      br(),
      br(),
      fluidRow(
        box(title = "What days do we walk the most?"),
        column(width = 12,
               shinycssloaders::withSpinner(plotlyOutput("colPlotStepsByDate"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))),
        column(width = 10,
               textOutput("textLineSteps"))
        ),
      
    ),
    tabItem(
      tabName = "Sleep",
      fluidRow(
        box(title = "How long do we sleep?"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("plotSleepGentleman"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        ),
        column(width = 4,
               textOutput("textSleepGentleman"),
               br(),
               uiOutput("sleepGentleman"))
      ),
      fluidRow(
        box(title = "At what hours do we go to sleep?"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("histSleepStart"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        ),
        column(width = 4,
               textOutput("textSleepGentleman2")
               
      )),
      fluidRow(
        box(title = "When do we wake up?"),
        column(width = 8,
               shinycssloaders::withSpinner(plotlyOutput("histSleepEnd"),
                                            type = getOption("spinner.type", default = 5),
                                            color = getOption("spinner.color", default = "#edc9c7"))
        ),
        column(width = 4,
               textOutput("textSleepGentleman3")
               
        ))
    )),
  theme_blue
)

## ui 

ui <- dashboardPage(
  
  header,
  sidebar,
  body
  
)

shinyApp(ui, server)


