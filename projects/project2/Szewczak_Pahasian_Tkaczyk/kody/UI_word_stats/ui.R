library(shiny)
library(plotly)
library(bslib)
library(tidytext)
library(ggfun)
library(shinyjs)
thematic::thematic_shiny(font = "auto")


ui1 <- fluidPage(
    useShinyjs(),
    
    tags$style(HTML("
              #wordplot {
                position: relative;
              }      
            
             #slider1 .shiny-input-container {
                    position: absolute;
                    bottom: -10px;
                    left:0px;
                    background: rgba(0, 0, 0, 0.5);
                  }
             ")),
    
    fluidRow(
      column(8,div(
             div(shinycssloaders::withSpinner(plotOutput("wordPlot", 
                                                     width = "100%"),
                                          type = getOption("spinner.type", default = 4),
                                          color = getOption("spinner.color", default = "red")
                                          ), 
                 div(sliderInput("number_of_words", 
                                 "Number of words", 
                                 min = 10, max = 100, 
                                 value = 50,
                                 step = 5,
                                 10:100,
                                 width = "100%"), id = "slider1"),
                 id = "wordplot"),
      
             titlePanel(tags$h4("The most popular words in the titles of watched videos")),
             htmlOutput("person")
      )
      ),
      column(3,
             dateRangeInput(
               "time_range_input2",
               "Time range",
               start = "2023-01-01",
               end = "2023-12-31",
               min = "2020-07-01",
               max = "2023-12-31",
               format = "yyyy-mm-dd",
               startview = "month",
               weekstart = 0,
               language = "en",
               separator = " to ",
               width = NULL,
               autoclose = TRUE
             ),
             sliderInput("min_word_length", "Minimum word length",
                         min = 4, max = 8,
                         value = 6,
                         step = 1,
                         4:8,
                         width = "100%")
             )
    )
)
ui2 <- fluidPage(
  useShinyjs(),
  
  tags$style(HTML("
              #plot {
                position: relative;
              }      
                    
             #input .shiny-input-container {
                    position: absolute;
                    bottom: -5px;
                    left:0px;
                    background: rgba(0, 0, 0, 0.5);
             }
             
             #input2 .shiny-input-container {
                    position: absolute;
                    bottom: -5px;
                    left:0px;
                    background: rgba(0, 0, 0, 0.5);
             }
             
             ")),
  
  titlePanel("Trending"),
  
  fluidRow(column(8,
                  fluidRow(
                  div(id="pointer1"),
                  div(id = "plot1",
                          div(
                            style = "height = 1600px;border: 2px solid grey; border-radius:20px",
                            shinycssloaders::withSpinner(
                              uiOutput("pointPlotPlaceHolder", inline = TRUE),
                              type = getOption("spinner.type", default = 4),
                              color = getOption("spinner.color", default = "red")
                            ),
                            div(
                              dateRangeInput(
                                "time_range_input",
                                "Time range for top 5 channels",
                                start = "2021-01-01",
                                end = "2023-12-31",
                                min = "2020-07-01",
                                max = "2023-12-31",
                                format = "yyyy-mm-dd",
                                startview = "month",
                                weekstart = 0,
                                language = "en",
                                separator = " to ",
                                width = "100%",
                                autoclose = TRUE
                              ),
                              id = "input"
                            ),
                            id = "plot"
                          ),
                          uiOutput("pointPlotTitlePlaceholder")
                       ),
                  htmlOutput("person2"),
                  titlePanel(HTML('<span style="width:1px;display:inline-block"></span>'))
                  )
                  ),
           column(4, style="height=50px",
                  div(id="pointer2"),
                  div(id = "plot2",
                        style="height=100%",
                        div( 
                          tags$style(HTML("
                                          .shiny-spinner-output-container{
                                          height = 100%;
                                          }
                                          ")),
                          style = "height=100%;border: 2px solid grey; border-radius:20px",
                          shinycssloaders::withSpinner(
                            id="spinner",
                            uiOutput("piechartPlaceholder"),
                            type = getOption("spinner.type", default = 4),
                            color = getOption("spinner.color", default = "red"),
                          ),
                          div(
                            dateRangeInput(
                              "time_range_input3",
                              "Time range for top 5 channels",
                              start = "2021-01-01",
                              end = "2023-12-31",
                              min = "2020-07-01",
                              max = "2023-12-31",
                              format = "yyyy-mm-dd",
                              startview = "month",
                              weekstart = 0,
                              language = "en",
                              separator = " to ",
                              width = "100%",
                              autoclose = TRUE
                            ),
                            id = "input2"
                          ),
                          id = "plot"
                        ),
                        uiOutput("piechartTitlePlaceholder"),
                      )
                  )
           )
)

ui3<-fluidPage(
  
  useShinyjs(),
  
  tags$style(HTML("
              #plotLine {
                position: relative;
              }      
                    
             #inputLine .shiny-input-container {
                    position: absolute;
                    bottom: -5px;
                    left:0px;
                    background: rgba(0, 0, 0, 0.5);
                  }
             ")),
  
  titlePanel("Time"),
  fluidRow(column(8,
                  fluidRow(
                    div(id="pointer3"),
                    div(id = "plot3",
                        div(
                          style = "border: 2px solid grey; border-radius:20px",
                          shinycssloaders::withSpinner(
                            uiOutput("pointPlot2PlaceHolder"),
                            type = getOption("spinner.type", default = 4),
                            color = getOption("spinner.color", default = "red")
                          ),
                          div(selectInput("type","Statistics by months",
                                          c("video", "add", 
                                            "add percent"), selected = "add percent"), 
                              id = "inputLine"),
                          id = "plotLine"
                        ),
                        uiOutput("pointPlot2TitlePlaceholder")
                    ),
                    htmlOutput("person5"),
                    titlePanel(HTML('<span style="width:1px;display:inline-block"></span>'))
                  )
  ),
  column(4,
         div(id="pointer4"),
         div(id = "plot4",
             div( 
               style = "border: 2px solid grey; border-radius:20px",
               shinycssloaders::withSpinner(
                 id="spinner",
                 uiOutput("timeplotPlaceholder"),
                 type = getOption("spinner.type", default = 4),
                 color = getOption("spinner.color", default = "red"),
               ),
               id = "plot"
             ),
             uiOutput("timeplotTitlePlaceholder"),
         )
  )
  )
)

ui4 <- fluidPage(
  titlePanel("Channels"),
  fluidRow(
    column(12,
           checkboxGroupInput("name", "Select names for channel statistics",
                              c("Milanna" = "Milanna",
                                "Michal" = "Michal",
                                "Krzysztof" = "Krzysztof"),
                              selected = c("Milanna", "Michal","Krzysztof")))
  ),
  fluidRow( 
    column(10, 
           shinycssloaders::withSpinner(dataTableOutput("table"),
                                        type = getOption("spinner.type", default = 4),
                                        color = getOption("spinner.color", default = "red"))
           
    )
  )
)

my_theme <- bs_theme(bg = "rgba(15,15,15)",
                     fg = "rgba(255,255,255)",
                     primary = "rgba(255,255,255)",
                     version = 5,
                     base_font = font_google("Roboto", local = FALSE),
                     "component-active-bg" = "#FF0000",
                     "nav-pills-link-active-bg" = "#404040"
                     ) #rgba(92,164,244)

appUI <- page_navbar(
  title = div(icon("youtube"), "Me at YouTube"),
  collapsible = TRUE,
  navset_pill_list(
    nav_panel("Words", ui1, icon = icon("spell-check")),
    nav_panel("Trending", ui2, icon = icon("fire")),
    nav_panel("Channels",ui4,icon= icon("tv")),
    nav_panel("Time", ui3, icon = icon("clock")),
    nav_item(
      tags$head(tags$style(
        HTML(
        "
        #radioButton input[type='radio']{
          display: none; /* Hide the default radio button */
        }
        #radioButton label {
          position: relative;
          left: 18px;
        }
        #radioButton .shiny-options-group {
          position: relative;
          left:-25px;
        }
        
          "
        )
      )),
      radioButtons(
        "radioButton",
        div(icon("user"),
            HTML('<span style="width:1px;display:inline-block"></span>'), 
            "Person"),
        choiceNames = list(div(img(src ="lo3.png", width = 20, height = 20), "Michał"),
                           div(img(src ="lo2.jpg", width = 20, height = 20), "Milanna"),
                           div(img(src ="Logo1.png", width = 20, height = 20), "Krzysztof")
                           ),
        choiceValues = c("Michal","Milanna","Krzysztof"),
        inline = TRUE
      )),
    widths = c(2,10),
    well = FALSE),
  nav_spacer(),
  nav_menu(
    title = "Authors",
    icon = icon("user"),
    nav_item("Milanna"),
    nav_item("Krzysztof"),
    nav_item("Michał"),
    align = "right"
  ),
  theme = my_theme)
