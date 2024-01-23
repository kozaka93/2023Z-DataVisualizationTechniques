#wczytanie biblitotek
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)



# wczytanie danych
 
heatMap_data <- read.csv("../../../app/KomunikacJA/appData/heatMap/heatMapData.csv",
                         colClasses = c(date = "Date"))





# obsluga UI



#############################ui do heatmapy#####################
ui1 <- tags$div(
  
  # Application title

  
    tags$div(
      class = "main_right",
      fixedPanel(
        class = "left_panel",
        tags$div(
          tags$div(
            HTML("<h1>Osoby</h1>"),
            style = "background-color:white;"
          ),
          tags$div(  
            class = "person_button",
            actionButton("a", "Ania")
          ),
          tags$div(  
            class = "person_button",
            actionButton("z", "Zosia")
          ),
          tags$div(  
            class = "person_button",
            actionButton("f", "Filip")
          )
        ),
      tags$div(
        class = "osoby",
        tags$div(
          class = "app_button",
          actionButton("mg", "mg")
        ),
        tags$div(
          class = "app_button",
          actionButton("ig", "ig")
        ),
        tags$div(
          class = "app_button",
          actionButton("sp", "sp")
        ),
        tags$div(
          class = "app_button",
          actionButton("all", "all")
        )
      ),
    ),

    tags$div(
      tags$div(
        HTML('<h1 class = "tytul_konwersacji"><b>Którego dnia roku najwięcej się komunikujemy?</b></h1>')),
      class = "convo_div",
      selectInput("input_year",
                  choices = unique(year(heatMap_data$date)) %>% sort(),
                  label = "Wybierz rok",
                  selected = 2023,
                  width = "10%"),
      tags$div(
        tags$div(
          class = "wiadomosc",
          plotlyOutput("heatMapa_plot")
        ),
        tags$div(
          class = c("wiadomosc", "wiadomosc_tekst"),
          "Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać."
        )
      )
      

    )
  )
)

#############################ui do heatmapy koniec #####################


#############################ui głowne #####################

ui_main <- tags$div(includeCSS("../../../app/KomunikacJA/css/styles.css"),
                    style = "background-color: red; display:block;",
                    tags$div(
                      style = "background-color: white;",
                      navbarPage("",
                                 tabPanel(tags$div("JA")),
                                 tabPanel("Heatmapa", ui1),
                                 tabPanel("cos tu kiedys bedzie"),
                                 tabPanel("cos tu kiedys bedzie"),
                                 tabPanel("cos tu kiedys bedzie"),
                                 tabPanel("cos tu kiedys bedzie"),selected = "Heatmapa"
                      )
                    )
)

#############################ui głowne koniec #####################






# oblsuga server

server <- function(input, output) {
  
  ### kod odtąd do aż kiedy napiszę przyda się wszystkim
  person_main <- reactiveVal("a")
  app_main <- reactiveVal("mg")
  
  # tu wasze dane
  heatMap <- reactiveValues(data = heatMap_data %>%
                              filter(person == "a",
                                     # year(date) == 2023,
                                     app == "mg")
  )
  


  # tu tez wasze
  updateData <- function(){
    
    heatMap$data <- heatMap_data %>%
      filter(person == person_main(),
             # year(date) == 2023,
             app %in% app_main())
    updateOptions()
    
  }
  
  updateOptions <- function() {
    updateSelectInput(inputId = "input_year",
                      choices = unique(year(heatMap$data$date)) %>% sort,
                      selected = ifelse(input$input_year %in% unique(year(heatMap$data$date)),
                                        input$input_year,
                                        2023))
  }
  

  
  
  observeEvent(input$a, {
    person_main("a")
    updateData()
  })

  observeEvent(input$z, {
    person_main("z")
    updateData()
  })

  observeEvent(input$f, {
    person_main("f")
    updateData()
  })

  observeEvent(input$mg, {
    app_main("mg")
    updateData()
  })

  observeEvent(input$ig, {
    app_main("ig")
    updateData()
  })

  observeEvent(input$sp, {
    app_main("sp")
    updateData()
  })

  observeEvent(input$all, {
    app_main(c("mg", "ig", "sp"))
    updateData()
    
  })

  ### az dotad trwa obsluga wyboru osob i aplikacji
  

  output$heatMapa_plot <- renderPlotly({
    chosen_app <- case_when(identical(app_main(),"mg") ~ " w Messengerze",
                            identical(app_main(),"ig") ~ " w Instagramie",
                            identical(app_main(),"sp") ~ " w Snapchacie",
                            TRUE ~ " we wszystkich aplikacjach")

    chosen_person <- case_when(person_main() == "a" ~ "Anię",
                               person_main() == "z" ~ "Zosię",
                               person_main() == "f" ~ "Filipa")
    plot_title <- paste0("<b>",
                         "Liczba wiadomości",
                         " wysłanych i odebranych przez ",
                         chosen_person,
                         chosen_app,
                         " danego dnia w ",
                         input$input_year,
                         " roku",
                         "</b>")
    months <- c("Styczeń", 
                  "Luty", 
                  "Marzec", 
                  "Kwiecień", 
                  "Maj", 
                  "Czerwiec", 
                  "Lipiec", 
                  "Sierpień", 
                  "Wrzesień", 
                  "Październik", 
                  "Listopad", 
                  "Grudzień")
    ggplotly(
      heatMap$data %>%
        right_join(data.frame(date = seq(min(heatMap_data %>%
                                               filter(person == person_main(),
                                                      app %in% app_main()) %>%
                                             .$date),
                                         as.Date("2023-12-31"),
                                         by = "day")),
                   by = "date") %>%
        filter(year(date) == input$input_year) %>%
        group_by(date) %>%
        summarise(liczba_wiadomosci = sum(liczba,
                                          na.rm = TRUE)) %>%
        ggplot(aes(x = day(date), y = month(date), fill = liczba_wiadomosci, text = paste0(format(date, "%d %B %Y"),
                                                                                           "<br>Wysłano i odebrano ",
                                                                                           liczba_wiadomosci,
                                                                                           " wiadomości"))) +
        geom_tile() +
        scale_y_continuous(limits = c(12.5, 0.5),
                           breaks = 1:12,
                           labels = paste0("<b>", months, "</b>"),
                           trans = "reverse",
                           expand = expansion(c(0, 0), c(0.3, 0))) +
        scale_x_continuous(limits = c(0.5, 31.5),
                           breaks = 1:31,
                           expand = expansion(c(0, 0), c(0.5, 0)),
                           labels = paste0("<b>", 1:31, "</b>")) +
        labs(title = plot_title,
             x = "Dzień miesiąca",
             y = "Miesiąc") +
        theme_minimal() +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank()) +
        geom_hline(yintercept = 0.5:12.5,
                   linewidth = 0.3) +
        geom_vline(xintercept = 0.5:31.5,
                   linewidth = 0.3),
      tooltip = "text"
    ) %>%
      layout(
        xaxis = list(fixedrange = TRUE,
                     title = list(standoff = 15),
                     tickfont = list(size = 15,
                                     color = "black",
                                     thickness = 3)),
        yaxis = list(fixedrange = TRUE,
                     title = list(standoff = 15),
                     tickfont = list(size = 15,
                                     color = "black",
                                     thickness = 3)
                     ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(
          bgcolor = "white",  
          font = list(size = 14, 
                      color = "black")  
        ),
        title = list(font = list(size = 20),
                     y = 0.99, 
                     x = 0.51, 
                     xanchor = 'center', 
                     yanchor =  'top')) %>% 
      config(displayModeBar = FALSE
             )-> p
    
    p[["x"]][["data"]][[2]][["hoverinfo"]] = 'skip'
    p[["x"]][["data"]][[3]][["hoverinfo"]] = 'skip'
    
    p[["x"]][["data"]][[4]][["marker"]][["colorbar"]][["title"]] = HTML("<br>ㅤ<br>ㅤ<br>Sumaryczna liczba <br>wiadomości<br>ㅤ")
    p[["x"]][["data"]][[4]][["marker"]][["colorbar"]][["len"]] = 1
    p[["x"]][["data"]][[4]][["marker"]][["colorbar"]][["tickvals"]] = seq(0, 1, len = 9)
    p[["x"]][["data"]][[4]][["marker"]][["colorbar"]][["ticktext"]] = floor(seq(0, 
                                                                          max(heatMap$data %>% 
                                                                                   filter(year(date) == input$input_year) %>%
                                                                                   group_by(date) %>%
                                                                                   summarise(liczba_wiadomosci = sum(liczba,
                                                                                                                     na.rm = TRUE)) %>% 
                                                                                .$liczba_wiadomosci),
                                                                          len = 9))

    scale <- rep(seq(0, 
                     1, 
                     len = 9),
                 each = 2)
    scale <- scale[-c(1, length(scale))]
    #colors <- c("red","red","#FDE624","#FDE624")
    colors <- rep(c(
      # "#e5f7ff", 
      #               "#ccefff", 
       #              "#b2e7ff", 
                    "#99e0ff", 
      #               "#7fd8ff", 
                    "#66d0ff",
                    #"#4cc9ff", 
                    "#32c1ff", 
                    #"#19b9ff",
                    "#00b2ff",
                    #"#00a0e5",
                    "#008ecc",
                    #"#007cb2",
                    "#006a99",
                    #"#00597f",
                    "#004766",
                   # "#00354c",
                    "#002333"), each = 2)


    colorScale <- data.frame(scale, colors)

    p[["x"]][["data"]][[1]][["colorscale"]] = colorScale
    names(p[["x"]][["data"]][[1]][["colorscale"]]) = NULL
    p[["x"]][["data"]][[4]][["marker"]][["colorscale"]] = colorScale
    names(p[["x"]][["data"]][[4]][["marker"]][["colorscale"]]) = NULL

    p
  })
  

}

# Run the application 
shinyApp(ui = ui_main, server = server)
