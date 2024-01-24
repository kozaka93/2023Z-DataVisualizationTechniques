library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(dtplyr)
library(data.table)
library(lubridate)
library(shinycssloaders)
library(fmsb)

julka_data <- read.csv("filtered_data/julka_filtered_data.csv")
tomek_data <- read.csv("filtered_data/tomek_filtered_data.csv")
nadia_data <- read.csv("filtered_data/nadia_filtered_data.csv")

nadia_data %>% 
  filter(time < "2023-12-08") -> nadia_data

all_data <- bind_rows(julka_data, tomek_data, nadia_data)



styles_file <- includeCSS("styles.css")


css_sidebar <- HTML("#sidebarCollapsed{
 background-color:#182f37;
}")

css_logo <- HTML(".wrapper .main-header .logo{
 background-color:#385a65;
}
")





header <- dashboardHeader(
  title = "Spotify dashboard"
)



sidebar <- dashboardSidebar(
  tags$style(css_sidebar, css_logo),
  sidebarMenu(
    menuItem(" Info Page", tabName = "info_page", icon = icon("info")),
    menuItem(" Stats", tabName = "one_person", icon = icon("dog")),
    menuItem(" Blend", tabName = "blend", icon = icon("users")),
    prettyRadioButtons(
      inputId = "person",
      label = "Choose person:", 
      choices = c("  Julka", "  Nadia", "  Tomek"),
      fill = TRUE,
      animation = NULL
    ),
    splitLayout(cellWidths = c("50%", "50%"),  
                dateInput("datefrom", "Date from:", format = "dd/mm/yy", 
                          "2023-01-01", min = "2015-10-21",
                          max = "2023-12-06"),
                dateInput("dateto", "Date to:", format = "dd/mm/yy", 
                          "2023-12-31"))
  )
)




body <- dashboardBody(
  tags$head(tags$style(styles_file)),
  tabItems(
    tabItem(tabName = "info_page",
            htmlOutput("info", style = "margin-bottom: 20px;"),
            htmlOutput("aim"),
            htmlOutput("aim_content"),
            fluidRow(
              column(width = 4,
                     uiOutput("our_colors")
              )
            ),
            htmlOutput("about"),
            htmlOutput("about_content"),
            fluidRow(
              column(width = 4),
              column(width = 4,
                     imageOutput("logo_mini")
              )
            )
    ),
    tabItem(tabName = "one_person",
            htmlOutput("name"),
            fluidRow(
              column(width = 6,
                     box(
                       width = 12,
                       withSpinner(htmlOutput("text_songs"),
                                   color = "#385a65",
                                   type = 3,
                                   color.background = "#182f37",
                                   size = 0.5
                       ),
                       withSpinner(tableOutput("table"),
                                   color = "#385a65",
                                   type = 3,
                                   color.background = "#182f37",
                                   size = 0.5
                       ))
              ),
              column(width = 2,
                     box(
                       width = 12,
                       withSpinner(htmlOutput("text_artist"),
                                   color = "#385a65",
                                   type = 3,
                                   color.background = "#182f37",
                                   size = 0.5
                       )
                     ) 
              ),
              
              column(width = 4,
                     box(
                       width = 12,
                       withSpinner(htmlOutput("artist_image"),
                                   color = "#385a65",
                                   type = 3,
                                   color.background = "#182f37",
                                   size = 0.5
                       )
                     )
              )
              
              
            ),
            fluidRow(
              column(width = 6,
                     withSpinner(plotlyOutput("repart_plot",
                                              height = "300px"),
                                 color = "#385a65",
                                 type = 3,
                                 color.background = "#182f37",
                                 size = 0.5
                     )
                     
              ),
              column(width = 6,
                     align = "center",
                     withSpinner(plotlyOutput("density_plot",
                                              width = "600px",
                                              height = "300px"),
                                 color = "#385a65",
                                 type = 3,
                                 color.background = "#182f37",
                                 size = 0.5
                     )
              )
            ),
            fluidRow(
              column(width = 6,
                     div(
                       withSpinner(
                         plotlyOutput("unique_arists_plot", height = "300px"),
                         color = "#385a65",
                         type = 3,
                         color.background = "#182f37",
                         size = 0.5
                       ),
                       style = "margin-top: 20px;" 
                     )
              ),
              column(width = 6,
                     align = "center",
                     htmlOutput("radar_title", style = 
                                  "margin-top: 50px; margin-bottom: -20px; 
                                position: relative; z-index: 1;")
              ),
              column(width = 6,
                     align = "center",
                     withSpinner(
                       plotOutput("radar_plot",
                                  width = "100%",
                                  height = "400px"),
                       color = "#385a65",
                       type = 3,
                       color.background = "#182f37",
                       size = 0.5
                     ),
                     style = "margin-top: -20px; position: relative; 
                     z-index: 0;"
                  
              )
            )
            
    ),
    tabItem(tabName = "blend",
            htmlOutput("top"),
            fluidRow(
              column(width = 4,
                     box(
                       width = 12,
                       htmlOutput("choice"),
                       selectInput(
                         inputId = "select",
                         choices = c("Listening time", 
                                     "Average tempo",
                                     "Number of artists",
                                     "Average followers number",
                                     "Favourite genre"),
                         label = NULL
                       ),
                       tableOutput("rank")
                     )
              ),
              column(width = 4,
                     box(
                       width = 12,
                       withSpinner(htmlOutput("mutual_artist"),
                                   color = "#385a65",
                                   type = 3,
                                   color.background = "#182f37",
                                   size = 0.5
                       )
                     )),
              column(width = 4,
                     box(
                       width = 12,
                       withSpinner(htmlOutput("mutual_song"),
                                   color = "#385a65",
                                   type = 3,
                                   color.background = "#182f37",
                                   size = 0.5
                       )
                     )),
              column(width = 12,
                     withSpinner(plotlyOutput("minutes_plot",
                                              height = "500px"),
                                 color = "#385a65",
                                 type = 3,
                                 color.background = "#182f37",
                                 size = 0.5))
            )
    )
  ))

# app
ui <- dashboardPage(
  header,
  sidebar,
  body
  
)

server <- function(input, output, session) {
  
  updateTabItems(session, "info_page", T)
  
  filtered_data <- reactive({
    datefrom <- input$datefrom
    dateto <- input$dateto
    
    data <- switch(input$person,
                   "  Tomek" = tomek_data,
                   "  Julka" = julka_data,
                   "  Nadia" = nadia_data) 
    
    data %>%
      lazy_dt() %>%
      filter(time > datefrom & time < dateto) %>%
      collect()
  })
  
  person_name <- reactive({
    input$person
  })
  
  person_color <- reactive({
    p_color <- switch(input$person,
                      "  Tomek" = "#A386C0",
                      "  Julka" = "#EF5571",
                      "  Nadia" = "#00B9F1")
    p_color
  })
  

  ### INFO PAGE ###
  
  output$info <- renderText(
    HTML(paste("<span style = 'color: #ffffff; 
               font-family: Roboto, sans-serif;
               line-height: 1.3em; 
               text-transform: uppercase; 
               text-align: left;
               text-decoration:none;
               text-shadow:none;
               white-space:normal;
               letter-spacing:4.1px;
               word-spacing:-1.1px;
               column-count:1;
               direction:ltr;
               top:-8px;' >INFO PAGE</span><br>"))
  )
  
  output$aim <- renderText(
    HTML(paste(
      "<span 
      style=
    'font-size: 15px;
    font-family: Roboto, sans-serif;
    color:#ecf0f1; 
    font-size:26px;
    text-align:right;
    font-weight:600;
    position:relative;
    top:-4px;'>Aim of the project</span><br>"))
  )
  
  output$aim_content <- renderText(
    HTML(paste("<span style='font-size: 15px; color:#ecf0f1;
               text-align: justify;'>We are Julka, Tomek and Nadia and the 
               aim of this project was to ",
               "analyse and present our Spotify data. We have created two
               panels - Blend and Stats.<br>",
               "<br>",
               "Blend shows some relationships between our data such as 
               comparison by listening time, average tempo of songs, etc.
               We have also checked ",
               "what is our mutual favourite artist and song.<br>",
               "<br>",
               "In the Spotify panel you can choose a specific 
               person and date ",
               "and see what are theirs top 10 artists in the 
               chosen period of time ",
               "and some other analysis.<br>",
               "Each person is represented by one of the three colours:",
               "<br>",
               "<div style='text-align: center;'>
               <span style='font-size: 80px;'>
               <span style='display: inline-block; vertical-align: middle;'>
               <i class='fas fa-paint-brush' style='color: #EF5571;
               margin-right: 10px;'></i>
               <i class='fas fa-paint-brush' style='color: #A386C0;
               margin-right: 10px;'></i>
               <i class='fas fa-paint-brush' style='color: #00B9F1;'></i>
               </span></span><br>
               <span style='font-size: 20px; color: #EF5571;
               margin-right: 70px;'>Julka</span>
               <span style='font-size: 20px; color: #A386C0;
               margin-right: 70px;'>Tomek</span>
               <span style='font-size: 20px; color: #00B9F1; '>Nadia</span>
               </div><br>")
    )
  )
  
  output$about <- renderText(
    HTML(paste(
      "<span style=
    'font-size: 15px;
    color:#ecf0f1; 
    font-size:26px;
    text-align:right;
    font-weight:600;
    position:relative;
    top:-4px;'>About us</span><br>"))
  )
  
  output$about_content <- renderText({
    HTML(paste("<span style='font-size: 15px; color:#ecf0f1; text-align: 
    justify;'>
               We are second-year students of Engineering and Data Science 
               at Warsaw University
               of Technology at the Faculty of Mathematics 
               and Information Science.<br>",
               "This project was made as a part of the subject
               Data Visualisation Techniques.<br>",
               "<br>",
               "Hope you like it :))<br></span><br>")
    )
  })
  
  output$logo_mini <- renderImage({
    img_src <- "logo-mini.png"
    list(src = img_src, width = "100%", height = "auto")
  }, deleteFile = FALSE)
  

  ### STATS ###
  
  output$name <- renderText(
    HTML(paste("<span 
    style = 'color: #ffffff; 
               line-height: 1.3em; 
               font-family: Roboto, sans-serif;
               text-transform: uppercase; 
               text-align: left;
               text-decoration:none;
               text-shadow:none;
               white-space:normal;
               letter-spacing:4.1px;
               word-spacing:-1.1px;
               column-count:1;
               direction:ltr;
               top:-8px;
               '</span>", person_name()))
  )
  
  output$text_songs <- renderText({
    HTML(paste(
      "<span 
      style=
    'font-size: 15px;
    font-family: Roboto, sans-serif;
    color:#ecf0f1; 
    font-size:26px;
    text-align:right;
    font-weight:600;
    position:relative;
    top:-4px;'>Top 5 songs</span><br>"))})
  
  output$table <- renderTable({
    filtered_data() %>%
      lazy_dt() %>%
      filter(ms_played > 30000) %>% 
      group_by(track_name, artist_name, album_name, genre) %>% 
      summarise(
        minutes_listened = sum(ms_played) / (1000 * 60),
        songs_played = n()
      ) %>%
      arrange(desc(songs_played), desc(minutes_listened)) %>% 
      collect() %>%
      na.omit() %>%
      head(5) %>%
      ungroup() %>%
      mutate(nr = row_number()) %>%
      select(nr, everything()) %>%
      rename(
        "No." = nr,
        "Track" = track_name,
        "Artist" = artist_name,
        "Album" = album_name,
        "Genre" = genre,
        "Minutes" = minutes_listened,
        "Times" = songs_played
      )
  },
  width = "100%")
  
  
  output$repart_plot <- renderPlotly({
    
    listening_repartition <- filtered_data() %>%
      lazy_dt() %>%
      group_by(hour) %>% 
      summarise(minutes_listened = sum(ms_played) / (1000 * 60)) %>% 
      mutate(total_time = sum(minutes_listened),
             repartition = (minutes_listened / total_time) * 100) %>%
      collect()
    
    # ggplot
    listening_repartition %>%
      ggplot(aes(x = hour + 1, y = repartition, text =
                   paste("</br><b>Hour: </b>", hour,
                     "</br><b>Repartition: </b>", 
                      round(repartition, digits = 1), "%"))) +
      geom_bar(stat = "identity", fill = person_color()) +
      scale_x_discrete(limits = sprintf("%02d", 0:23)) -> gg
    
    
    p <- ggplotly(gg, tooltip = "text")
    
    # layout 
    p <- p %>%
      layout(
        title = list(text = "Listening repartition over day", 
                     y = 0.98, x = 0.5, xanchor = 'center', 
                     yanchor =  'top',
                     font = list(size = 15, color = "#ecf0f1", 
                                 family = "Roboto, sans-serif")),
        xaxis = list(title = list(text = "Hour",
                                  font = list(size = 15, color = "#ecf0f1",
                                              family = "Roboto, sans-serif")), 
                     tickfont = list(size = 14, color = '#ecf0f1'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        yaxis = list(title = list(text = "Repartition (%)", 
                                  font = list(size = 15, color = "#ecf0f1",
                                              family = "Roboto, sans-serif")), 
                     tickfont = list(size = 14, color = '#ecf0f1'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        plot_bgcolor = "#182f37",
        paper_bgcolor = "#182f37",
        showlegend = FALSE,
        hoverlabel = list(font = list(color = '#ecf0f1'))
      )
    p
  })
  
  output$unique_arists_plot <- renderPlotly({
    
    datefrom <- input$datefrom
    dateto <- input$dateto
    
    filtered_data() %>% 
      mutate(date = as.Date(time, format = "%Y-%m-%d")) %>% 
      group_by(date) %>% 
      summarise(unique_artists_n = n_distinct(artist_name)) %>% 
      filter(date > datefrom, date < dateto) -> unique_artists 
    
    p <- plot_ly(unique_artists, x = ~date, y = ~unique_artists_n,
                 hoverinfo = "text",
                 type = "scatter",
                 mode = "lines",
                 line = list(color = c(person_color())),
                 text = ~paste("</br><b>Date:</b> ", date, 
                               "</br><b>Unique artists:</b> ", 
                               unique_artists_n)) %>% 
      layout(
        title = list(text = "Number of unique artists per day", 
                     y = 0.95, x = 0.5, xanchor = "center", yanchor =  "top",
                     font = list(size = 15, color = "#ecf0f1", 
                                 family = "Roboto, sans-serif", weight = 500)),
        xaxis = list(title = list(text = "Date",
                                  font = list(size = 15, color = "#ecf0f1",
                                              family = "Roboto, sans-serif")), 
                     tickfont = list(size = 14, color = '#ecf0f1',
                                     family = "Roboto, sans-serif"),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05,
                     range = c(as.character(datefrom), 
                               min(as.character(dateto), "2023-12-06"))),
        yaxis = list(title = list(text = "Unique artists", 
                                  font = list(size = 15, color = "#ecf0f1",
                                              family = "Roboto, sans-serif")), 
                     tickfont = list(size = 14, color = '#ecf0f1', 
                                     family = "Roboto, sans-serif"),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        plot_bgcolor = "#182f37",
        paper_bgcolor = "#182f37",
        hoverlabel = list(font = list(color = '#ecf0f1',
                                      family = "Roboto, sans-serif"))
      )
    p
    
  })
  

  
  
  fav_artist_df <- reactive({
    filtered_data() %>% 
      lazy_dt() %>%
      group_by(artist_name, image, genre, followers) %>% 
      summarise(
        minutes_listened = round(sum(ms_played) / (1000 * 60)),
        songs_played = n()
      ) %>%
      arrange(desc(minutes_listened)) %>%
      collect() %>%
      na.omit() %>%
      head(1)
  })
  output$text_artist <- renderText({
    
    # favourite artist info
    fav_artist_name <- fav_artist_df()$artist_name
    minutes_played <- fav_artist_df()$minutes_listened
    songs_played <- fav_artist_df()$songs_played
    fav_artist_genre <- fav_artist_df()$genre
    fav_artist_followers <- fav_artist_df()$followers
    
    html_text <- paste(
      "<span style=
      'font-size: 15px; 
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>Favourite artist:
      </span><br>",
      "<span style=
      'font-size: 30px;
      font-family: Roboto, sans-serif;
      font-weight: bold; color:#ecf0f1'>", 
      fav_artist_name, "</span><br>",
      "<span style=
      'font-size: 15px;
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>Minutes listened:
      </span><br>",
      "<span style=
      'font-size: 30px; 
      font-weight: bold;
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>", 
      minutes_played, "</span><br>",
      "<span style=
      'font-size: 15px;
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>Songs played:</span><br>",
      "<span style=
      'font-size: 30px;
      font-family: Roboto, sans-serif;
      font-weight: bold;
      color:#ecf0f1'>", 
      songs_played, "</span><br>",
      "<span style=
      'font-size: 15px;
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>Genre:</span><br>",
      "<span style=
      'font-size: 30px; 
      font-family: Roboto, sans-serif;
      font-weight: bold;
      color:#ecf0f1'>", 
      fav_artist_genre, "</span>"
    )
    # text
    HTML(html_text)
  })
  
  output$artist_image <- renderText({
    img_src <- fav_artist_df()$image
    img_tag <- paste('<div style="text-align:center;">', 
                     '<img src="', img_src, '"', 
                     'width="300px" height="300px">', sep = "")
    return(img_tag)
  })
  
  
  output$density_plot <- renderPlotly({
    
    fav_artist_name <- fav_artist_df()[[1]]
    
    x <- all_data %>%
      filter(artist_name == fav_artist_name)
    
    min_tempo <- min(x$tempo)
    max_tempo <- max(x$tempo)
    
    dens_plot <- all_data %>%
      lazy_dt() %>%
      filter(artist_name == fav_artist_name) %>% 
      collect() %>%
      ggplot(aes(x = artist_name, y = tempo, text = 
                   paste("</br><b>Artist: </b>", fav_artist_name[[1]]))) +
      geom_violin(fill = person_color(), color = person_color(), size = 0.5) +
      coord_flip()
    
    
    dens_plot <- ggplotly(dens_plot, tooltip = "text") %>%
      layout(
        title = list(text = paste("Distribution of listened songs tempo for",
                                  fav_artist_name[[1]]),
                     y = 0.95, x = 0.55, xanchor = 'center', yanchor =  'top',
                     font = list(size = 15, color = "#ecf0f1",
                                 family = "Roboto, sans-serif")),
        hoverlabel = list(font = list(color = '#ecf0f1')),
        xaxis = list(title = list(text = "Tempo",
                                  font = list(size = 15, color = "#ecf0f1", 
                                              family = "Roboto, sans-serif")), 
                     tickfont = list(size = 14, color = '#ecf0f1'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05,
                     range = c(min_tempo, max_tempo)),
        yaxis = list(title = list(text = ""), 
                     tickfont = list(size = 14, color = 'transparent'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        plot_bgcolor = "#182f37",
        paper_bgcolor = "#182f37",
        showlegend = FALSE,
        hoverlabel = list(font = list(color = '#ecf0f1')),
        margin = list(l = 10, r = 10)
      )
    
    dens_plot
  })
  
  output$radar_title <- renderText({
    HTML(paste("<span style='font-size: 15px;
               color:#ecf0f1;
               text-align: center;
               font-family: Roboto, sans-serif;
               margin-bottom: 0px;
               '>Mean of categories for top 5 listened songs</span><br>"))
  })
  
  # most 5 played song
  top_five_songs_df <- reactive({
    filtered_data() %>% 
      lazy_dt() %>%
      group_by(track_name) %>% 
      summarise(songs_played = n()) %>%
      arrange(desc(songs_played)) %>%
      collect() %>%
      na.omit() %>%
      head(5)
  })
  
  song_features <- reactive({
    filtered_data() %>% 
      select(track_name, danceability, energy, speechiness, acousticness,
             instrumentalness, liveness, valence) %>% 
      distinct(track_name, .keep_all = TRUE) %>%
      right_join(top_five_songs_df(), by = "track_name") %>% 
      select(-c(track_name, songs_played)) %>%
      summarise(
        danceability = mean(danceability, na.rm = TRUE),
        energy = mean(energy, na.rm = TRUE),
        speechiness = mean(speechiness, na.rm = TRUE),
        acousticness = mean(acousticness, na.rm = TRUE),
        instrumentalness = mean(instrumentalness, na.rm = TRUE),
        liveness = mean(liveness, na.rm = TRUE),
        valence = mean(valence, na.rm = TRUE)
      )
  })
  
  output$radar_plot <- renderPlot({
    
    top_five_songs_features <- rbind(rep(1, ncol(song_features())), 
                                     rep(0, ncol(song_features())),
                                     song_features())
    
    par(bg = "#182f37", col = "#ecf0f1")
    rad <- radarchart(top_five_songs_features,
                      seg = 3,
                      title = "",
                      cglty = 1,
                      cglcol = "#ecf0f1",
                      pcol = person_color(),
                      pfcol = alpha(person_color(), 0.3),
                      col = "#ecf0f1",
                      caxislabels = c(0, 0.25, 0.5, 0.75, 1),
                      plwd = 3)
    
    rad
  })
  
  
  
  
  ######## BLEND #############
  
  output$top <- renderText(
    HTML(paste("<span style = 'color: #ffffff; 
               font-family: Roboto, sans-serif;
               line-height: 1.3em; 
               text-transform: uppercase; 
               text-align: left;
               text-decoration:none;
               text-shadow:none;
               white-space:normal;
               letter-spacing:4.1px;
               word-spacing:-1.1px;
               column-count:1;
               direction:ltr;
               top:-8px;' >BLEND</span><br>"))
  )
  
  output$choice <- renderText({
    HTML(paste(
      "<span style=
    'font-size: 15px;
    font-family: Roboto, sans-serif;
    color:#ecf0f1; 
    font-size:26px;
    text-align:right;
    font-weight:600;
    position:relative;
    top:-4px;'>Compare by</span><br>"))})
  
  blend_data <- reactive({
    datefrom <- input$datefrom
    dateto <- input$dateto
    
    all_data %>%
      lazy_dt() %>%
      filter(time > datefrom & time < dateto) %>%
      collect()
    
  })
  
  output$rank <- renderTable({
    if (input$select == "Listening time") {
      blend_data() %>% 
        group_by(username) %>% 
        summarise(minutes_listened = sum(ms_played) / (60 * 1000)) %>% 
        arrange(-minutes_listened) %>% 
        rename(
          "Name" = username,
          "Minutes listened" = minutes_listened
        )
    } else if (input$select == "Average tempo") {
      blend_data() %>% 
        group_by(username) %>% 
        summarise(average_tempo = mean(tempo, na.rm = T)) %>% 
        arrange(-average_tempo) %>% 
        rename(
          "Name" = username,
          "Average tempo" = average_tempo
        )
    } else if (input$select == "Number of artists") {
      blend_data() %>% 
        group_by(username) %>% 
        summarise(artists_number = n_distinct(artist_name)) %>% 
        arrange(-artists_number) %>% 
        rename(
          "Name" = username,
          "Number of unique artists" = artists_number
        )
    } else if (input$select == "Average followers number") {
      blend_data() %>%
        group_by(username, artist_name) %>%
        summarise(average_followers_n = mean(followers, na.rm = TRUE)) %>%
        group_by(username) %>%
        summarise(average_followers_n = 
                    as.integer(round(mean(average_followers_n, 
                                          na.rm = TRUE)))) %>% 
        arrange(-average_followers_n) %>% 
        rename(
          "Name" = username,
          "Average number of artist's followers" = average_followers_n
        )
    } else if (input$select == "Favourite genre") {
      blend_data() %>% 
        group_by(username) %>% 
        summarise(fav_genre = 
                    levels(factor(genre))[which.max(table(genre))]) %>% 
        rename(
          "Name" = username,
          "Favourite genre" = fav_genre
        )
    }
  },
  width = "100%",
  align = "c"
  )
  
  
  
  output$mutual_artist <- renderText({
    
    mutual_artist <- blend_data() %>% 
      group_by(artist_name) %>% 
      summarise(n_j = sum(username == "Julka"), n_t = sum(username == "Tomek"),
                n_n = sum(username == "Nadia")) %>% 
      filter(n_j > 30, n_t > 30, n_n > 30) %>% 
      slice_head(n = 1)
    
    mutual_artist_name <- mutual_artist$artist_name[1]
    artist_times <- mutual_artist$n_j[1] + mutual_artist$n_t[1]
    + mutual_artist$n_n[1]
    
    if (is.na(mutual_artist_name)) {
      HTML(paste(
        "<span style=
        'font-size: 15px; 
        font-family: Roboto, sans-serif;
        color:#ecf0f1'>Mutual artist:
      </span><br>",
        "<span style=
        'font-size: 30px; 
        font-family: Roboto, sans-serif;
        font-weight: bold; 
        color:#ecf0f1'>0 mutual artists in this time range</span><br>"))
    }
    
    else {
      HTML(paste(
        "<span style=
      'font-size: 15px; 
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>Mutual artist:
      </span><br>",
        "<span style=
      'font-size: 30px; 
      font-family: Roboto, sans-serif;
      font-weight: bold; color:#ecf0f1'>", 
        mutual_artist_name, "</span><br>",
        "<span style=
      'font-size: 15px; 
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>Together we listened to this artist:
      </span><br>",
        "<span style=
      'font-size: 30px;
      font-family: Roboto, sans-serif;
      font-weight: bold;
      color:#ecf0f1'>", 
        artist_times, "</span>",
        "<span style=
      'font-size: 15px;
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>times</span><br>"
      ))}
  })
  
  
  
  output$mutual_song <- renderText({
    
    mutual_songs <- blend_data() %>% 
      group_by(artist_name, track_name) %>% 
      summarise(n_j = sum(username == "Julka"), n_t = sum(username == "Tomek"),
                n_n = sum(username == "Nadia")) %>% 
      filter(n_j > 10, n_t > 10, n_n > 10) 
    
    mutual_song <- mutual_songs$track_name[1]
    song_times <- mutual_songs$n_j[1] + mutual_songs$n_t[1] 
    + mutual_songs$n_n[1] 
    
    if (is.na(mutual_song)) {
      HTML(paste(
        "<span style='
        font-size: 15px;
        font-family: Roboto, sans-serif;
        color:#ecf0f1'>Mutual song:</span><br>",
        "<span style=
        'font-size: 30px;
        font-family: Roboto, sans-serif;
        font-weight: bold; color:#ecf0f1'>
        0 mutual songs in this time range</span><br>"
      ))
    }
    else {
      HTML(paste(
        "<span style=
      'font-size: 15px;
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>Mutual song:</span><br>",
        "<span style=
      'font-size: 30px; 
      font-weight: bold;
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>", 
        mutual_song, "</span><br>",
        "<span style=
      'font-size: 15px; 
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>Together we played this song :</span><br>",
        "<span style=
      'font-size: 30px;
      font-family: Roboto, sans-serif;
      font-weight: bold;
      color:#ecf0f1'>", 
        song_times, "</span>",
        "<span style=
      'font-size: 15px; 
      font-family: Roboto, sans-serif;
      color:#ecf0f1'>times</span><br>"
      ))}})
  
  
  
  output$minutes_plot <- renderPlotly({
    
    datefrom <- input$datefrom
    dateto <- input$dateto
    
    df1 <- julka_data %>% 
      mutate(date = as.Date(time, format = "%Y-%m-%d")) 
    
    df2 <- tomek_data %>% 
      mutate(date = as.Date(time, format = "%Y-%m-%d")) %>% 
      filter(date > "2019-01-01") # bo byla tylko jakas jedna wartosc przed 2020
    
    df3 <- nadia_data %>% 
      mutate(date = as.Date(time, format = "%Y-%m-%d"))
    
    df1$date <- as.Date(df1$date)
    df2$date <- as.Date(df2$date)
    df3$date <- as.Date(df3$date)
    
    minutes_per_day_j <- df1 %>%
      group_by(date) %>%
      filter(time > datefrom & time < dateto) %>%
      summarise(total_minutes = sum(ms_played) / (60 * 1000)) %>%
      arrange(date) %>%
      mutate(cumsum = cumsum(total_minutes)) %>% 
      mutate(name = "Julka")
    
    
    minutes_per_day_t <- df2 %>% 
      group_by(date) %>%
      filter(time > datefrom & time < dateto) %>%
      summarise(total_minutes = sum(ms_played) / (60 * 1000)) %>%
      arrange(date) %>%
      mutate(cumsum = cumsum(total_minutes)) %>% 
      mutate(name = "Tomek")
    
    minutes_per_day_n <- df3 %>% 
      group_by(date) %>%
      filter(time > datefrom & time < dateto) %>%
      summarise(total_minutes = sum(ms_played) / (60 * 1000)) %>%
      arrange(date) %>%
      mutate(cumsum = cumsum(total_minutes)) %>% 
      mutate(name = "Nadia")
    
    df <- bind_rows(minutes_per_day_j, minutes_per_day_t, minutes_per_day_n)
    
    minutes_plotly <- plot_ly(df, x = ~date, y = ~cumsum, color = ~name, 
                              hoverinfo ="text",
                              text = ~paste("</br><b>Name:</b>", name, 
                                            "</br><b>Date:</b> ", date, 
                                            "</br><b>Minutes:</b> ",
                                            round(cumsum)),
                              type = "scatter", mode = "lines",
                              colors = c("#EF5571", "#00B9F1", "#A386C0")) %>%
      layout(
        title = list(text = "Total minutes listened", 
                     y = 0.98, x = 0.5, xanchor = "center", yanchor =  "top",
                     font = list(size = 15, color = "#ecf0f1", 
                                 family = "Roboto, sans-serif")),
        xaxis = list(title = list(text = "Date",
                                  font = list(size = 15, color = "#ecf0f1", 
                                              family = "Roboto, sans-serif")), 
                     tickfont = list(size = 14, color = '#ecf0f1', 
                                     family = "Roboto, sans-serif"),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05,
                     range = c(as.character(datefrom), 
                               min(as.character(dateto), "2023-12-07")),
                     rangeslider = list(type = "date")),
        yaxis = list(title = list(text = "Minutes", 
                                  font = list(size = 15, color = "#ecf0f1", 
                                              family = "Roboto, sans-serif")), 
                     tickfont = list(size = 14, color = '#ecf0f1', 
                                     family = "Roboto, sans-serif"),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        legend = list(
          title = list(text = "Person", font = list(color = "#ecf0f1")),
          font = list(size = 12, color = "#ecf0f1", 
                      family = "Roboto, sans-serif"),
          bgcolor = "#182f37"
        ),
        plot_bgcolor = "#182f37",
        paper_bgcolor = "#182f37",
        hoverlabel = list(font = list(color = '#ecf0f1'))
      )
    
    minutes_plotly
  })
  
}



shinyApp(ui, server)      
