#wczytanie biblitotek
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)



# wczytanie danych

heatMap_data <- read.csv("C:/twd_proj2/repo/Projekt_TWD_02/app/KomunikacJA/appData/heatMap/heatMapData.csv",
                         colClasses = c(date = "Date"))

linePlot_mg_a <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_mg_a.csv")
linePlot_ig_a <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_ig_a.csv")
linePlot_sp_a <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_sp_a.csv")
linePlot_mg_f <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_mg_f.csv")
linePlot_ig_f <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_ig_f.csv")
linePlot_sp_f <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_sp_f.csv")
linePlot_mg_z <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_mg_a.csv")
linePlot_ig_z <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_ig_a.csv")
linePlot_sp_z <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\wyslaneOdebrane\\wyslaneOdebrane_sp_a.csv")

linePlot_sp_a$GroupOrPriv <- "priv"
linePlot_sp_f$GroupOrPriv <- "priv"
linePlot_sp_z$GroupOrPriv <- "priv"

policzWiadomosci <- function(sp_a) {
  sp_a %>% 
    group_by(date) %>% 
    summarize(liczba_wiadomosci = n()) -> sp_a
  sp_a <- sp_a[order(sp_a$date), ] 
  sp_a$suma_kumulacyjna <- cumsum(sp_a$liczba_wiadomosci)
  sp_a$typ <- 'wszystkie'
  return(sp_a)
}

#policzenie wiadomosci z podzialem na wyslane i odebrane
policzWiadomosciPodzial <- function(sp_a) {
  sp_a$typ[sp_a$Sender == "Other"] <- "odebrane"
  sp_a$typ[sp_a$Sender != "Other"] <- "wyslane"
  sp_a <- sp_a %>% 
    group_by(date, typ) %>% 
    summarize(liczba_wiadomosci = n()) %>%
    arrange(date) %>% 
    group_by(typ) %>% 
    mutate(suma_kumulacyjna = cumsum(liczba_wiadomosci)) 
  return(sp_a)
}

policzWszystkie <- function(sp_a){
  wszystkie <- policzWiadomosci(sp_a)%>% 
    select(date, suma_kumulacyjna, typ)
  podzial <- policzWiadomosciPodzial(sp_a)%>% 
    select(date, suma_kumulacyjna, typ)
  razem <- rbind(wszystkie,podzial) 
  return(razem)
}

linePlot_mg_a <- policzWszystkie(linePlot_mg_a) 
linePlot_sp_a <- policzWszystkie(linePlot_sp_a) 
linePlot_ig_a <- policzWszystkie(linePlot_ig_a) 
linePlot_mg_f <- policzWszystkie(linePlot_mg_f) 
linePlot_sp_f <- policzWszystkie(linePlot_sp_f) 
linePlot_ig_f <- policzWszystkie(linePlot_ig_f) 
linePlot_mg_z <- policzWszystkie(linePlot_mg_z) 
linePlot_sp_z <- policzWszystkie(linePlot_sp_z) 
linePlot_ig_z <- policzWszystkie(linePlot_ig_z) 
linePlot_mg_a$app <- "mg"
linePlot_mg_f$app <- "mg"
linePlot_mg_z$app <- "mg"
linePlot_sp_a$app <- "sp"
linePlot_sp_f$app <- "sp"
linePlot_sp_z$app <- "sp"
linePlot_ig_a$app <- "ig"
linePlot_ig_f$app <- "ig"
linePlot_ig_z$app <- "ig"
linePlot_mg_a$person <- "a"
linePlot_ig_a$person <- "a"
linePlot_sp_a$person <- "a"
linePlot_mg_f$person <- "f"
linePlot_ig_f$person <- "f"
linePlot_sp_f$person <- "f"
linePlot_mg_z$person <- "z"
linePlot_ig_z$person <- "z"
linePlot_sp_z$person <- "z"

linePlot_data <- rbind(linePlot_mg_a, linePlot_ig_a, linePlot_sp_a, linePlot_mg_f, linePlot_ig_f, linePlot_sp_f, linePlot_mg_z, linePlot_ig_z, linePlot_sp_z)
linePlot_data$date <- as.Date(as.character(linePlot_data$date), format = "%Y%m%d")
# obsluga UI




ui1 <- tags$div(
  
  # Application title
  
  
  tags$div(
    style = "background-color: yellow; min-height: 2000px; margin-top:105px;",
    fixedPanel(
      style = "margin-right: 200px; width: 300px; height: 100%; background-color: white; height:100vh; display: grid;",
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
        style="display: flex;width: -webkit-fill-available;justify-content: space-evenly;height:80px;align-items:center;margin-top:120px;margin-right:15px;border-top-width: 3px;border-top-style: solid;border-top-color:#EBEDF0;padding-top:20px;",
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
      titlePanel("Heatmapa :)"),
      style = "background-color: green; margin-left: 25%; height: 80%",
      selectInput("input_year",
                  choices = unique(year(heatMap_data$date)) %>% sort(),
                  label = "year",
                  selected = 2023,
                  width = "10%"),
      tags$div(
        plotlyOutput("heatMapa_plot"),
        textOutput("eee")
      )
      
      
    )
  )
)


ui2 <- tags$div(
  
  
  tags$div(
    style = "background-color: pink; min-height: 2000px; margin-top:105px;",
    fixedPanel(
      style = "margin-right: 200px; width: 300px; height: 10%; background-color: white; height:100vh; display: grid;",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(  
          class = "person_button",
          actionButton("a2", "Ania")
        ),
        tags$div(  
          class = "person_button",
          actionButton("z2", "Zosia")
        ),
        tags$div(  
          class = "person_button",
          actionButton("f2", "Filip")
        )
      ),
      tags$div(
        style="display: flex;width: -webkit-fill-available;justify-content: space-evenly;height:80px;align-items:center;margin-top:120px;margin-right:15px;border-top-width: 3px;border-top-style: solid;border-top-color:#EBEDF0;padding-top:20px;",
        tags$div(
          class = "app_button",
          actionButton("mg2", "mg")
        ),
        tags$div(
          class = "app_button",
          actionButton("ig2", "ig")
        ),
        tags$div(
          class = "app_button",
          actionButton("sp2", "sp")
        )
      ),
    )),
    
    tags$div(
      titlePanel("linePlot:)"),
      style = "background-color: pink; margin-left: 25%; height: 10%",
      sliderInput(inputId = "rok",
                  label = "Lata:",
                  min = min(as.numeric(format(linePlot_data$date, "%Y"))),
                  max = max(as.numeric(format(linePlot_data$date, "%Y"))),
                  value = c(2020, 2023)),
      tags$div(
        plotlyOutput("linePlot_plot"),
        textOutput("opis")
      )
      
    )
  )


ui_main <- tags$div(includeCSS("C:/twd_proj2/repo/Projekt_TWD_02/app/KomunikacJA/css/styles.css"),
                    style = "background-color: red; display:block;",
                    tags$div(
                      style = "background-color: white;",
                      navbarPage("",
                                 tabPanel(tags$div("JA",
                                                   style = "width:500px;")),
                                 tabPanel("Heatmapa", ui1),
                                 tabPanel("linePlocik", ui2),
                                 tabPanel("cos tu kiedys bedzie"),
                                 tabPanel("cos tu kiedys bedzie"),selected = "Heatmapa"
                      )
                    )
)

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
  linePlot <- reactiveValues(data = linePlot_data %>%
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
    linePlot$data <- linePlot_data %>%
      filter(person == person_main(),
             # year(date) == 2023,
             app %in% app_main())
  }

  updateOptions <- function() {
    updateSelectInput(inputId = "input_year",
                      choices = unique(year(heatMap$data$date)) %>% sort,
                      selected = ifelse(input$input_year %in% unique(year(heatMap$data$date)),
                                        input$input_year,
                                        2023))
  }
updateOptions2 <- function() {
   updateSliderInput(inputId = "rok",
                  label = "Lata:",
                  min = min(as.numeric(format(linePlot_data$date, "%Y"))),
                  max = max(as.numeric(format(linePlot_data$date, "%Y"))),
                  value = input$rok)
}


  observeEvent(input$a2, {
    person_main("a")
    updateData()
  })

  observeEvent(input$z2, {
    person_main("z")
    updateData()
  })

  observeEvent(input$f2, {
    person_main("f")
    updateData()
  })

  observeEvent(input$mg2, {
    app_main("mg")
    updateData()
  })

  observeEvent(input$ig2, {
    app_main("ig")
    updateData()
  })

  observeEvent(input$sp2, {
    app_main("sp")
    updateData()
  })

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
  #
  # ### az dotad trwa obsluga wyboru osob i aplikacji
  output$linePlot_plot <- renderPlotly({
    chosen_app <- case_when(identical(app_main(),"mg") ~ " w Messengerze",
                            identical(app_main(),"ig") ~ " w Instagramie",
                            identical(app_main(),"sp") ~ " w Snapchacie",
                            TRUE ~ " we wszystkich aplikacjach")

    chosen_person <- case_when(person_main() == "a" ~ "Anię",
                               person_main() == "z" ~ "Zosię",
                               person_main() == "f" ~ "Filipa")
    plot_title <- paste0("Liczba wiadomości",
                         " wysłanych i odebranych przez ",
                         chosen_person,
                         chosen_app,
                         " do danego dnia ",
                         max(input$rok),
                         " roku")
    ggplotly(
      linePlot$data %>%
        filter(year(date) >= min(input$rok) & year(date) <= max(input$rok)) %>%
        ggplot(aes(x=date, y = suma_kumulacyjna, color=typ)) +
        geom_line()+
        labs(title=plot_title,
          x = "Data",   # Zmiana podpisu osi x
          y = "Liczba wiadomości",)+ # Zmiana podpisu osi y
        theme_minimal())
  })
  output$test <- renderPlotly({linePlot_data %>% 
      ggplot(aes(x=person, y=typ))})
  output$heatMapa_plot <- renderPlotly({
    chosen_app <- case_when(identical(app_main(),"mg") ~ " w Messengerze",
                            identical(app_main(),"ig") ~ " w Instagramie",
                            identical(app_main(),"sp") ~ " w Snapchacie",
                            TRUE ~ " we wszystkich aplikacjach")

    chosen_person <- case_when(person_main() == "a" ~ "Anię",
                               person_main() == "z" ~ "Zosię",
                               person_main() == "f" ~ "Filipa")
    plot_title <- paste0("Liczba wiadomości",
                         " wysłanych i odebranych przez ",
                         chosen_person,
                         chosen_app,
                         " danego dnia w ",
                         input$input_year,
                         " roku")
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
        ggplot(aes(x = day(date), y = month(date), fill = liczba_wiadomosci)) +
        geom_tile() +
        scale_y_continuous(limits = c(12.5, 0.5),
                           breaks = 1:12,
                           labels = month.name,
                           trans = "reverse",
                           expand = expansion(c(0, 0), c(0.3, 0))) +
        scale_x_continuous(limits = c(0.5, 31.5),
                           breaks = 1:31,
                           expand = expansion(c(0, 0), c(0.5, 0))) +
        labs(title = plot_title,
             x = "Day of Month",
             y = "Month") +
        theme_minimal() +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank()) +
        geom_hline(yintercept = 0.5:12.5,
                   linewidth = 0.3) +
        geom_vline(xintercept = 0.5:31.5,
                   linewidth = 0.3)
    ) %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) -> p
    p[["x"]][["data"]][[2]][["hoverinfo"]] = 'skip'
    p[["x"]][["data"]][[3]][["hoverinfo"]] = 'skip'

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

  output$eee <- renderText({HTML("eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
                                 eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\neeeee
                                 eeeeeeeeeeeeeee"
                                 )})
  output$opis <- renderText({HTML(":)))))))))))))))))))))))))))"
  )})


}


  

# Run the application 
shinyApp(ui = ui_main, server = server)

