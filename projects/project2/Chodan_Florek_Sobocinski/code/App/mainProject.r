library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringr)
library(ggwordcloud)

# Hubert
dataH <- as.data.frame(read.csv("dataH.csv"))
wordH <- as.data.frame(read.csv("words_dataH.csv"))
seqH <- as.data.frame(read.csv("seq_dataH.csv"))

# Paweł
dataP <- as.data.frame(read.csv("dataF.csv"))
wordP <- as.data.frame(read.csv("words_dataF.csv"))
seqP <- as.data.frame(read.csv("seq_dataF.csv"))

# Miłosz
dataM <- as.data.frame(read.csv("data.csv"))
wordM <- as.data.frame(read.csv("words_data.csv"))
seqM <- as.data.frame(read.csv("seq_data.csv"))

images <- c("https://gfx.wiadomosci.radiozet.pl/var/radiozetwiadomosci/storage/images/swiat/hipopotam-polknal-i-wyplul-2-letnie-dziecko-chlopczyk-cudem-przezyl/22021405-1-pol-PL/Hipopotam-polknal-i-wyplul-2-letnie-dziecko.-Chlopczyk-cudem-przezyl_full-hd.jpg", "https://i.pinimg.com/564x/d4/e5/58/d4e55831049c93c72ae6eb7ca7af7ec0.jpg", "https://rikoland.pl/data/include/cms//manul-stepowy-dlugosc-zycia.jpg")
data1 <- c(dataH, dataP, dataM)
word2 <- c(wordH, wordP, wordM)
seq3 <- c(seqH, seqP, seqM)

ui <- dashboardPage(title = "Messenger Statistics",
  dashboardHeader(title = div(h2("Messenger Statistics", style = "font-family: 'Arial', sans-serif; font-size: 75%; display: inline-block;"),
                              img(src = "https://upload.wikimedia.org/wikipedia/commons/b/be/Facebook_Messenger_logo_2020.svg", height = "15%", width = "15%", style = "display: inline-block; margin-left: 8px;"))
  ),
  dashboardSidebar(
    width = "230px",
    sidebarMenu(
      id = "menu",
      menuItem(text = tags$span(
        style = "font-size: 18px; color: #FFFFFF;",
        icon("rocket"), "Wstęp"
      ), tabName = "wstęp"),
      menuItem(text = tags$span(
        style = "font-size: 18px; color: #FFFFFF;",
        icon("clock"), "Czas"
      ), tabName = "czas"),
      menuItem(text = tags$span(
        style = "font-size: 18px; color: #FFFFFF;",
        icon("smile"), "Emoji"
      ), tabName = "emoji"),
      menuItem(text = tags$span(
        style = "font-size: 18px; color: #FFFFFF;",
        icon("envelope"), "Słowa"
      ), tabName = "słowa")
    ),
    uiOutput("sidebar")
  ),
  dashboardBody(tags$style(
    HTML(
      "
        .content-wrapper {
          background-color: #121212;  /* Zastąp 'yourColorHere' kolorem tła */
        }
        "
    )
  ),
    tags$head(tags$style(HTML('
      .main-sidebar .sidebar .sidebar-menu {
        width: 230px;
        height: 100%;
        background-color: #000000;
      }
      
      
      html {
        font-size: 20px;
        color: #FFFFFF !important;
      }
      
      .content-wrapper .content {
          background-color: #121212;
      }
      
      .skin-blue .main-header .navbar {
        background-color: #4B0082;
      }       
      
      .skin-blue .main-sidebar {
        background-color: #040404;
      }
      
      .box .box-body {
        background-color: #4B0082;
        -webkit-box-shadow: none; 
        -moz-box-shadow: none;
        box-shadow: none;
      }
      
      .logo {
        background-color:  #040404 !important;
      }
      .navbar {
        background-color:  #040404!important;
      }
      
      .control-label {
        font-weight: normal;
      }
      .shiny-bound-input {
        background-color: #040404;

      }
      
      .sidebarCollapsed { 
        background-color: #040404;
      }
      
      
      .skin-blue .main-sidebar .sidebar .sidebar-menu  a{
        border-radius: 20px;
        border-color: transparent;
      }
      
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #4B0082;
      }
      
      .modal-content {
        background-color: #121212;
      }
      
      .title-panel {
          color: #FF00FF !important;
          text-align: center;
      }
      
      .irs .irs-min, .irs .irs-max {
        color: #4B0082 !important;
      }
      
      /* Zmiana koloru wskaźnika suwaka */
      .irs .irs-bar {
        background-color: #FF00FF !important;
      }
      
      /* Zmiana koloru kropki na wskaźniku suwaka */
      .irs .irs-handle {
        background-color: #FF00FF !important;
      }
      
       .irs .irs-single, .irs .irs-bar, .irs .irs-slider {
                background: #FF00FF !important;
                border: 1px solid #FF00FF !important;
        }
        .irs .irs-grid-pol {
              background: #FF00FF !important;
        }
         
        .irs .irs-from, .irs .irs-to, .irs .irs-single {
            color: #FF00FF !important;
        }
        
        .irs .irs-slider {
            border-color: #FF00FF !important;
        }
        
        .main-content {
      background-color: #000000 !important; /* Ustaw kolor tła obszaru zawierającego wykresy */
        }
    
    body {
      background-color: #000000 !important; /* Ustaw kolor tła strony na czarny */
    }
                      
        
    '))),
    tabItems(
      tabItem(tabName = "wstęp",
              fluidPage(
                titlePanel(div("Wybór osoby dla której wyświetlą się dane", class = "title-panel", style = "text-align: center;")),
                fluidRow(
                  column(6,
                          tags$div(radioButtons("option",
                                               label = tags$span("Wybierz osobę:", style = "color: #FFFFFF; font-size: 150%;"),
                                                 choices = c("Hubert", "Paweł", "Miłosz"),
                                                 selected = "Hubert",
                                                inline = F),
                                    class = "my-radio-buttons")
                  ),
                  
                  column(6, 
                         mainPanel(
                           uiOutput("imageGallery")
                         ),
                      )
                )
              ),
                tags$head(
                  tags$style(HTML('
      .my-radio-buttons .shiny-options-group label {
        font-size: 20px;  /* Zwiększ rozmiar czcionki opcji */
        color: #FFFFFF !important;  /* Zmiana koloru tekstu opcji */
        background-color: #2C2C2C !important;  /* Zmiana koloru tła opcji */
        margin-right: 10px;  /* Zwiększ odstęp między opcjami */
        width: 100%;  /* Zmiana szerokości przycisku */
        box-shadow: none;
      }

      .my-radio-buttons .shiny-options-group label:hover {
        background-color: #4B0082 !important;  /* Zmiana koloru tła opcji po najechaniu myszką */
        color: #FF00FF !important;
      }
      
      body {
        background-color: #000000 !important; /* Ustaw kolor tła na czarny */
      }
      
    ')))
      ),
      tabItem(tabName = "czas",
              fluidPage(
                tags$head(
                  tags$style(
                    HTML("
                      .slider-container .jslider-value {
                        color: #FF00FF !important; /* Zmiana koloru tekstu wartości na suwaku */
                        background-color: #4B0082 !important;
                      }

                      .slider-container .jslider-pointer {
                        background-color: #FF00FF !important; /* Zmiana koloru wskaźnika suwaka */
                        color: #4B0082 !important;
                      }

                      .slider-container .jslider-pointer:before {
                        background-color: #FF00FF !important; /* Zmiana koloru kropki na wskaźniku suwaka *
                        color: #4B0082 !important;
                      }
                      
                      .lata .irs-single {
                          color: #FF00FF !important;
                      }
                        ")
                    )
                ),
                titlePanel(div("O jakich porach najczęściej wysyłamy wiadomości?", class = "title-panel")),
                fluidRow(
                  column(width = 12,
                         plotlyOutput("heatmap")
                  )
                ),
                fluidRow(
                  column(width = 12,
                           sliderInput("lata",
                                       label = tags$span("Wybierz lata:", style = "color: #FF00FF; font-size: 25px;"),
                                       min = 2010, max = 2023, value = c(2010, 2023))
                  )
                ),
                fluidRow(
                  column(width = 4,
                             sliderInput("days", label = tags$span("Wybierz dni:", style = "color: #FF00FF; font-size: 25px;"), min = 1, max = 31, value = c(1, 31)),
                         tags$div(selectInput("year", label = tags$span("Wybierz rok:", style = "color: #FF00FF; font-size: 25px;"), 
                                         choices = unique(d1$year), selected = "2023"),
                                  class = "my-select-input"),
                         tags$div(radioButtons("sender", 
                                               label = tags$span("Czy ja wysłałem?", 
                                                                 style = "color: #FF00FF; font-size: 25px;"), 
                                               choices = c("Tak" = TRUE, "Nie" = FALSE), 
                                               selected = TRUE),
                                  class = "my-radio-buttons"),
                             tags$div(radioButtons("is_group", 
                                                   label = tags$span("Grupa?", 
                                                                     style = "color: #FF00FF; font-size: 25px;"), 
                                                   choices = c("Tak" = TRUE, "Nie" = FALSE), 
                                                   selected = TRUE),
                                      class = "my-radio-buttons")
                         ),
                  column(width = 8,
                         plotlyOutput("timePlot"))
                )
                )
              ),
      tabItem(tabName = "emoji",
              fluidPage(
                titlePanel(div("Emoji i word cloudy", class = "title-panel")),
                fluidRow(
                  column(width = 6,
                                plotOutput("wordcloud")),
                  column(width = 6,
                         plotlyOutput("emojicloud")))
              )
              ),
      tabItem(tabName = "słowa",
              fluidPage(
                titlePanel(div("Liczba wiadomości", class = "title-panel")),
                tags$head(
                  tags$style(HTML("
       .text-center {
        text-align: center;
      }
    "))
                ),
                    div(style = "display: flex; flex-direction: column; align-items: center;", 
                        plotlyOutput("countPlot", width = "100%", height = "400px"),
                        plotlyOutput("seqPlot", width = "100%", height = "400px"),
                        plotlyOutput("scatterPlot", width = "100%", height = "400px")
                    )
                )
              )
      )
    )
)

server <- function(input, output, session) {
  ######################## wybór osoby #################################
  
  output$imageGallery <- renderUI({
    options <- c("Hubert", "Paweł", "Miłosz")
    index <- match(input$option, options)
    tagList(
      h3(input$option, style = "color: #FF00FF; text-align: center; font-size: 24px;"),
      tags$img(src = images[index], width = 200, height = 200)
    )
  })
  ########################## heatmap ##################################
  
  output$heatmap <- renderPlotly({
    options <- c("Hubert", "Paweł", "Miłosz")
    index <- match(input$option, options)
    if(index == 1){
      data <- dataH
    } else if(index == 2){
      data <- dataP
    } else if(index == 3){
      data <- dataM
    }
    
    dfheat <- data %>% 
      mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>% 
      mutate(
        date1 = as.Date(date),
        time = format(date, "%H:%M:%S"),
        hour = format(date, "%H"),
        year = format(date, "%Y"),) %>% 
      select("date1", "time", "hour", "year") %>% 
      filter(year >= input$lata[1] & year <= input$lata[2]) %>% 
      mutate(weekday = weekdays(date1)) %>% 
      group_by(weekday, hour) %>% 
      summarise(count = n())
    
    
    day_order <- c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")
    dfheat$weekday <- factor(dfheat$weekday, levels = day_order, ordered = TRUE)
    
    heatplot <- plot_ly(
      data = dfheat,
      x = ~hour,
      y = ~weekday,
      z = ~count,
      type = "heatmap",
      colors = colorRamp(c("white", "#FF00FF")),
      text = ~paste("Godzina: ", hour, "<br>Dzień: ", weekday, "<br>Suma: ", count)
    ) %>%
      layout(
        title =  list(text = "Heatmap godzin od dnia tygodnia", font = list(color = "#FFFFFF")),
        xaxis = list(title = "Godzina", tickfont = list(color = "#FFFFFF"), titlefont = list(color = "#FFFFFF", size = 20)),
        yaxis = list(title = "Dzień tygodnia", tickfont = list(color = "#FFFFFF"), titlefont = list(color = "#FFFFFF", size = 20)),
        paper_bgcolor = "#121212",
        plot_bgcolor = "#121212",
        legend = list(title = list(text = "Count", font = list(color = "#FFFFFF")))
        ) %>% 
      colorbar(title = list(text = "Liczba wiadomości", font = list(color = "#FFFFFF")),
               tickfont = list(color = "#FFFFFF")
               )
  })
  
  ####################### countPlot ######################
  
  output$countPlot <- renderPlotly({
    options <- c("Hubert", "Paweł", "Miłosz")
    index <- match(input$option, options)
    if(index == 1){
      data <- dataH
    } else if(index == 2){
      data <- dataP
    } else if(index == 3){
      data <- dataM
    }
    df_count <- data %>% 
      filter(is_group == "False") %>% 
      filter(conv_id != "uzytkownikfacebooka") %>% 
      filter(conv_id != "hawasicevsasadzanzibar") %>% 
      select(conv_id, content) %>% 
      group_by(conv_id) %>% 
      summarise(count = n()) %>% 
      right_join(data) %>% 
      select(conv_id, count) %>% 
      unique() %>% 
      arrange(desc(count)) %>% 
      head(10)
    
    df_count$conv_id <- c('africa1', 'africa2', 'africa3', 'africa4', 'africa5','africa6', 'africa7', 'africa8', 'africa9', 'africa10')
    
    countPlot <- plot_ly(data = df_count, 
            x = ~conv_id, 
            y=~count, 
            type = "bar",
            marker = list(color = "#FF00FF")
    ) %>% 
      layout(title =  list(text = "Wykres osób z największą liczbą wiadomości", font = list(color = "#FFFFFF")),
             xaxis = list(tickval = c(1:10),
                          ticktext = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'),
                          title = "Osoba",
                          fixedrange = TRUE, 
                          categoryorder = "total descending",
                          tickfont = list(color = "#FFFFFF"), 
                          titlefont = list(color = "#FFFFFF", size = 20)), 
             yaxis = list(title = "Liczba wiadomości",
                          fixedrange = TRUE,
                          tickfont = list(color = "#FFFFFF"), 
                          titlefont = list(color = "#FFFFFF", size = 20),
                          type = 'log'),
             paper_bgcolor = "#121212",
             plot_bgcolor = "#121212") %>%
      config(displayModeBar = FALSE)
  })
  
  ####################### seqPlot ########################
  
  output$emojicloud <- renderPlotly({
    options <- c("Hubert", "Paweł", "Miłosz")
    index <- match(input$option, options)
    if(index == 1){
      data <- wordH
    } else if(index == 2){
      data <- wordP
    } else if(index == 3){
      data <- wordM
    }
    word_df <- data
    
    emoji_df <- data[str_detect(word_df$words, emoji_regex), ]
    emoji_df$words <- gsub("[^\\p{So}]", "", emoji_df$words, perl = TRUE)
    
    emoji_df <- emoji_df %>% 
      select(words) %>% 
      group_by(words) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      filter(words != "") %>% 
      head(10) %>% 
      arrange(desc(count))
    
    
    emoji_plot <- plot_ly(
      data = emoji_df,
      x = ~words,
      y = ~count,
      type = 'bar',
      marker = list(color = '#FF00FF') 
    ) %>%
      layout(
        title = list(text = "Liczba użytych emotek", font = list(color = "#FFFFFF")),
        xaxis = list(title = ""),
        yaxis = list(title = "Liczba emotek",
                     tickfont = list(color = "#FFFFFF"), 
                     titlefont = list(color = "#FFFFFF", size = 20)),
        barmode = 'stack',  
        plot_bgcolor = '#121212',
        paper_bgcolor = '#121212'
      )
    
  })
  
  ##################### scatterPlot ############################
  
  output$scatterPlot <- renderPlotly({
    options <- c("Hubert", "Paweł", "Miłosz")
    index <- match(input$option, options)
    if(index == 1){
      data <- dataH
      osoba <- "Hubert Sobociński"
    } else if(index == 2){
      data <- dataP
      osoba <- "Paweł Florek"
    } else if(index == 3){
      data <- dataM
      osoba <- "Miłosz Chodań"
    }
    df_total <- data %>%   
      group_by(conv_id) %>% 
      summarise(total_count = n()) %>% 
      right_join(data) %>% 
      select(conv_id, total_count, is_group) %>% 
      unique()
    
    df_sent <- data %>% 
      select(conv_id, sender, content) %>% 
      filter(sender == osoba) %>% 
      group_by(conv_id) %>% 
      summarise(sent_count = n()) %>% 
      right_join(data) %>% 
      select(conv_id, sent_count) %>% 
      unique()
    df_sent <- replace(df_sent, is.na(df_sent), 0)
    
    df_total <- df_total %>% 
      left_join(df_sent) %>% 
      mutate(perc_sent = round(sent_count/total_count *100, 2))
    
    df_total <- replace(df_total, df_total == "True", "Grupa")
    df_total <- replace(df_total, df_total == "False", "Prywatna")
    
    plot_ly(
      data = df_total,
      x = ~total_count,
      y = ~perc_sent,
      color = ~is_group,
      type = "scatter",
      mode = 'markers',
      colors = c("#FFFFFF", "#FF00FF"),
      hoverinfo = "text",
      text = ~paste("Conv_id:", conv_id, "<br>Liczba wiadomości: ", total_count, "<br>Wysłane przez nas: ", perc_sent, "%", "<br>Rodzaj: ", is_group)
    ) %>% 
      layout(title =  list(text = "Zależność liczby wysłanych wiadomości i % wysyłanych przez nas", font = list(color = "#FFFFFF")),
             xaxis = list(title = "Liczba wiadomości",
                          fixedrange = TRUE, 
                          categoryorder = "total descending",
                          tickvals = c(1, 10, 100, 1000, 10000, 100000),
                          tickfont = list(color = "#FFFFFF"), 
                          titlefont = list(color = "#FFFFFF", size = 20),
                          type = 'log'), 
             yaxis = list(title = "% wysłanych przez nas",
                          fixedrange = TRUE,
                          tickfont = list(color = "#FFFFFF"), 
                          titlefont = list(color = "#FFFFFF", size = 20)),
             legend = list(
               title = list(text = "Rodzaj konwersacji", 
                            font = list(size = 14, 
                                        color = '#FFFFFF')),
               font = list(color = '#FFFFFF', size = 12)
             ),
             paper_bgcolor = "#121212",
             plot_bgcolor = "#121212") %>%
      config(displayModeBar = FALSE)
    
  })
  
  ###################### timePlot ########################
  
  output$timePlot <- renderPlotly({
    options <- c("Hubert", "Paweł", "Miłosz")
    index <- match(input$option, options)
    if(index == 1){
      data <- dataH
      osoba <- "Hubert Sobociński"
    } else if(index == 2){
      data <- dataP
      osoba <- "Paweł Florek"
    } else if(index == 3){
      data <- dataM
      osoba <- "Miłosz Chodań"
    }
    d1 <- data %>% 
      mutate(year = substr(date, 1, 4),
             month = substr(date, 6, 7),
             day = substr(date, 9, 10),
             hour = substr(date, 12, 13)) %>% 
      mutate(sender = ifelse(sender == osoba, TRUE, FALSE)) %>% 
      mutate(is_group = ifelse(is_group == "True", TRUE, FALSE)) %>% 
      arrange(year)
    
    filtered_data <- d1 %>%
      filter(day >= input$days[1] & day <= input$days[2],
             year == input$year,
             sender == input$sender,
             is_group == input$is_group)
    
    occurrences <- summarise(group_by(filtered_data, month), n = n())
    
    plot_ly(occurrences, x = ~month, y = ~n, type = "bar",
            marker = list(color = "#FF00FF")) %>%
      layout(title = list(text = "Liczba wiadomości w zależności od miesiąca", font = list(color = "#FFFFFF")),
             xaxis = list(title = "Miesiąc",
                          tickfont = list(color = "#FFFFFF"), 
                          titlefont = list(color = "#FFFFFF", size = 20)),
             yaxis = list(title = "Liczba wiadomości",
                          tickfont = list(color = "#FFFFFF"), 
                          titlefont = list(color = "#FFFFFF", size = 20)),
             paper_bgcolor = "#121212",
             plot_bgcolor = "#121212")
    
  })
  
  ####################### wordcloud ###############################
  
  output$wordcloud <- renderPlot({
    options <- c("Hubert", "Paweł", "Miłosz")
    index <- match(input$option, options)
    if(index == 1){
      data <- wordH
    } else if(index == 2){
      data <- wordP
    } else if(index == 3){
      data <- wordM
    }
    punctuation_regex <- "[[:punct:]]"
    emoji_regex <- "[\\p{So}]"
    
    word_df <- data
    
    words_df <- word_df[!str_detect(word_df$words, emoji_regex), ]
    
    words_df <- words_df %>% 
      select(words) %>% 
      group_by(words) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count))
    
    
    words_df <- words_df[nchar(words_df$words) >= 2, , drop = TRUE]
    words_df <- words_df[!grepl(punctuation_regex, words_df$words), , drop = TRUE]
    words_df <- words_df %>% 
      filter(words!="")
    
    rownames(words_df) <- words_df$words
    set.seed(1)
    
    words_plot <- ggplot(head(words_df, 20), aes(label = words, size = count)) +
      geom_text_wordcloud_area(color = "#FF00FF", rm_outside = TRUE,
                               layout = "position", grid_size = 50) +
      scale_size_area(max_size = 40) +
      theme_minimal() +labs(y= "", x = "")+
      theme(plot.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black"))
    words_plot
    
  })
  
  ####################### emojicloud ##################
  
  output$emojicloud <- renderPlotly({
    options <- c("Hubert", "Paweł", "Miłosz")
    index <- match(input$option, options)
    if(index == 1){
      data <- wordH
    } else if(index == 2){
      data <- wordP
    } else if(index == 3){
      data <- wordM
    }
    
    emoji_regex <- "[\\p{So}]"
    
    word_df <- data
    
    emoji_df <- data[str_detect(word_df$words, emoji_regex), ]
    emoji_df$words <- gsub("[^\\p{So}]", "", emoji_df$words, perl = TRUE)
    
    emoji_df <- emoji_df %>% 
      select(words) %>% 
      group_by(words) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      filter(words != "") %>% 
      head(10) %>% 
      arrange(desc(count))
    
    emoji_df$words <- factor(emoji_df$words, levels = emoji_df$words[order(-emoji_df$count)])
    
    
    emoji_plot <- plot_ly(
      data = emoji_df,
      x = ~words,
      y = ~count,
      type = 'bar',
      marker = list(color = '#FF00FF') 
    ) %>%
      layout(
        title = list(text = "Liczba użytych emotek", font = list(color = "#FFFFFF")),
        xaxis = list(title = ""),
        yaxis = list(title = "Liczba emotek",
                     tickfont = list(color = "#FFFFFF"), 
                     titlefont = list(color = "#FFFFFF", size = 20)),
        barmode = 'stack',  
        plot_bgcolor = '#121212',
        paper_bgcolor = '#121212'
      )
    
  })

}


shinyApp(ui, server)


