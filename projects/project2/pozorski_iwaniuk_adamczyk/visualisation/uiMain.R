#####theme#####
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial",
  appFontColor = "",
  primaryFontColor = "",
  infoFontColor = "",
  successFontColor = "",
  warningFontColor = "",
  dangerFontColor = "",
  bodyBackColor = "",
  
  
  ### header
  logoBackColor = "	rgb(0, 128, 191)",
  headerButtonBackColor = "",
  headerButtonIconColor = "",
  headerButtonBackColorHover = "",
  headerButtonIconColorHover = "",
  
  headerBackColor = "	rgb(0, 128, 191)",
  headerBoxShadowColor = "",
  headerBoxShadowSize = "",
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = ""
    ,colorMiddle = ""
    ,colorEnd = ""
    ,colorStartPos = 0
    ,colorMiddlePos = 50
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
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(0,0,0)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(0, 128, 191)"
    ,colorMiddle = "rgb(0, 172, 223)"
    ,colorEnd = "rgb(85, 208, 255)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(0, 128, 191)"
    ,colorMiddle = "rgb(0, 172, 223)"
    ,colorEnd = "rgb(85, 208, 255)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "",
  boxBorderRadius = "",
  boxShadowSize = "",
  boxShadowColor = "",
  boxTitleSize = 20,
  boxDefaultColor = "",
  boxPrimaryColor = "",
  boxInfoColor = "",
  boxSuccessColor = "",
  boxWarningColor = "",
  boxDangerColor = "",
  
  tabBoxTabColor = "",
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = "",
  tabBoxTabTextColorSelected = "",
  tabBoxBackColor = "",
  tabBoxHighlightColor = "",
  tabBoxBorderRadius = 5,
  
  ### inputs
  buttonBackColor = "",
  buttonTextColor = "",
  buttonBorderColor = "",
  buttonBorderRadius = 5,
  
  buttonBackColorHover = "",
  buttonTextColorHover = "",
  buttonBorderColorHover = "",
  
  textboxBackColor = "",
  textboxBorderColor = "",
  textboxBorderRadius = 5,
  textboxBackColorSelect = "",
  textboxBorderColorSelect = "",
  
  ### tables
  tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)



#####body####

body <- dashboardBody(
  customTheme,
  
  
  tabItems(
    
    #####emoji####
    tabItem(tabName = "tab_emoji",
            
          fluidPage(
              
            h2( HTML('<p style="text-align: center; font-weight: bold;">Analiza najczęściej używanych emotek</p>')),
              
            #####barplot_emoji####
              box(width = 12,
                title = "Grupa | Liczba emotek | Zakres dat",               
                status = "primary",  
                
              
                    checkboxGroupInput("barplot_group_emoji", 
                                        "Grupa | Prywatne",
                                        choiceNames = c("Wysłane na grupie","Wysłane prywatnie"),
                                        choiceValues = c(T,F),
                                        selected = c(T,F)),
              
                    sliderInput("barplot_n_emoji",
                                  "Liczba emotek",
                                  value = 10,
                                  min = 1,
                                  max=15,
                                  ticks=FALSE),
                
                    sliderInput("barplot_date_emoji",
                                label="Zakres dat",
                                min = as.Date("2017-01-01"), 
                                max = as.Date("2024-01-01"),
                                value = c(as.Date("2017-01-01"), as.Date("2024-01-01"))
                    )
              ),
            
            
              
            
            box(
              width=12,
              title = textOutput("barplot_emoji_title"),
              status = "primary",
              solidHeader = TRUE,
                plotlyOutput("barplot_emoji")
            )
            
          )
            
              
            
    ),
    
    #####count_messages_main####
    
    tabItem(tabName = "tab_count_messages",
          fluidPage(
            
            h2(HTML('<p style="text-align: center; font-weight: bold;">Analiza liczby wysłanych wiadomości</p>')),
            
            #####barplot_count_messages####
              box(
                width =12,
                title = "Liczba konwersacji | Zakres dat",               
                status = "primary",
                
                 
                  sliderInput("barplot_n_count_messages",
                                "Liczba konwersacji",
                                value = 5,
                                min = 1,
                                max=10,
                                ticks = F),
                
                
                
                  sliderInput("barplot_date_count_messages",
                              label = "Zakres dat",
                              min = as.Date("2017-01-01"), 
                              max = as.Date("2024-01-01"),
                              value = c(as.Date("2017-01-01"), as.Date("2024-01-01")),
                  )

              ),
            
              
            
              
              box(
                width=12,
                title = textOutput("barplot_count_messages_title"),
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("barplot_count_messages")
              ),
            
            #####animated_barplot_count_messages#####
              
              
              box(
                width=12,
                title = textOutput("animated_barplot_count_messages_title"),
                status = "warning",
                solidHeader = TRUE,
                plotlyOutput("animated_barplot_count_messages")
              )
          )
    ),
    
    #####count_messages_sub####
    
    tabItem(tabName = "sub_tab_count_messages",
            
          fluidPage(
            
            h2(HTML('<p style="text-align: center; font-weight: bold;">Analiza liczby wysłanych wiadomości</p>')),
      
          #####line_count_messages####
      
            box(
              width = 12,
              title = "Zakres dat",               
              status = "primary",
              
                   sliderInput("line_date_count_messages",
                               "Zakres dat",
                               min = as.Date("2017-01-01"), 
                               max = as.Date("2024-01-01"),
                               value = c(as.Date("2017-01-01"), as.Date("2024-01-01")),
                   )
              
            ),
            
            box(
              width=12,
              title =  "Średnia liczba wysłanych wiadomości przez użytkowników w danej godzinie",
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("line_count_messages")
            ),
        
          #####line2_count_messages####
            
            box(
              width=12,
              title = "Liczba wiadomości wysłanych przez użytkowników w danym przedziale czasowym",
              status = "warning",
              solidHeader = TRUE,
              plotlyOutput("line2_count_messages")
            ),
            
          #####line3_count_messages####
      
            box(
              width=12,
              title = "Łączna liczba wysłanych wiadomości przez użytkowników od początku korzystania z Messengera",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput("line3_count_messages")
            )
          )
    ),
    
    #####time_respond####
    
    tabItem(tabName = "tab_time_respond",
            fluidPage(
              
              h2(HTML('<p style="text-align: center; font-weight: bold;">Analiza czasu odpowiedzi na wiadomości</p>')),
              
              
              
                box(width =12,
                    title = "Zakres dat",               
                    status = "primary",
                
                     sliderInput("heatmap_date_time_respond",
                                 "Zakres dat",
                                 min = as.Date("2017-01-01"), 
                                 max = as.Date("2024-01-01"),
                                 value = c(as.Date("2017-01-01"), as.Date("2024-01-01")),
                     )
                ),
                
                
              
          #####heatmap_time_respond####
              
                box(
                  width=12,
                  title = textOutput("heatmap_time_respond_title"),
                  status = "primary",
                  solidHeader = TRUE,
                       plotlyOutput("heatmap_time_respond")
                ),
              
              
          #####line_time_respond####
              
                box(
                  width=12,
                  title = textOutput("line_time_respond_title"),
                  status = "warning",
                  solidHeader = TRUE,
                       plotlyOutput("line_time_respond")
                )
              
              
            )
      
    ),
    
    #####common_strings####
    
    tabItem(tabName = "tab_common_strings",
            
            fluidPage(
              
              h2( HTML('<p style="text-align: center; font-weight: bold;">Analiza najczęściej używanych słów</p>')),
              
              
              box(width = 12,
                  title = "Liczba sekwencji | Liczba słów w sekwencji",               
                  status = "primary",
                
                 
                     numericInput("barplot_n_common_strings",
                                  "Liczba sekwencji",
                                  value = 5,
                                  min = 1,
                                  max=20),
                       
                
                
                     radioButtons("barplot_q_common_strings", 
                                  "Liczba słów w sekwencji",
                                  choiceNames = c("1","2","3"),
                                  choiceValues = c(1,2,3),
                                  selected = 1)
                
                
              ),
              
              
          #####barplot_common_strings####
              
              box(
                width=12,
                title = textOutput("barplot_common_strings_title"),
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("barplot_common_strings")
              )
              
              
            )
      
      
    ),
    
    
    #####summarise####
    tabItem(tabName = "tab_summarise",
            
            fluidPage(
              
              box(title = textOutput("summarise_title"),
                  status = "danger",
                  imageOutput("summarise_gif")
              ),
            
              box(title = textOutput("summarise_title_2"),
                  status = "danger",
                  solidHeader = TRUE,
                  htmlOutput("summarise_info")
              )
            )
            
            
    )
    
    
  )
)





#####sidebar#####

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Emotki", tabName = "tab_emoji", icon = icon("smile")),
    menuItem("Wysłane wiadomości", icon = icon("message"),
             menuSubItem("Wykresy dla wybranej osoby", tabName = "tab_count_messages"),
             menuSubItem("Wykresy wspólne", tabName = "sub_tab_count_messages")
             ),
    menuItem("Czas odpowiedzi", tabName = "tab_time_respond", icon = icon("clock")),
    menuItem("Najczęstsze sekwencje słów", tabName = "tab_common_strings", icon=icon("quote-right")),
    menuItem("Podsumowanie osób", tabName = 'tab_summarise',icon=icon("user")),
    selectInput(
      "user",
      "Osoba",
      choices = c("Por" = "Paweł Pozorski", "Kiddo" = "Krzysiek Adamczyk", "Misiu" = "Michał Iwaniuk"),
      selected = "Paweł Pozorski"
    )
  )
)

#####uiMain####

uiMain <- dashboardPage(
  dashboardHeader(title = "Messenger Analysis"),
  sidebar,
  body
)


