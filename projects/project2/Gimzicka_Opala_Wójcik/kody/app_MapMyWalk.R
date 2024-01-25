library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)
library(jsonlite)
library(RColorBrewer)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = tags$img(src = "logo2.png", width = "100%", height = "51px"), titleWidth = "350px"),
  dashboardSidebar(
    selectInput("wybranaOsoba", "Wybierz osobę:",
                choices = c("Dominika", "Ola", "Janek")),
    dateRangeInput("dataZakres", "Wybierz zakres dat:",
                   start = "2023-12-01", end = "2024-01-22"),
    sidebarMenu(
      menuItem("Panel Główny", tabName = "ogolny", icon = icon("info")),
      menuItem("Aktywność", tabName = "aktywnosc", icon = icon("walking")),
      menuItem("Powietrze", tabName = "jakoscPowietrza", icon = icon("wind")),
      menuItem("Muzyka", tabName = "muzyka", icon = icon("music"))
    ),
    img(src = "image2.png", style = "width: 100%; position: absolute; bottom: 0;")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* CSS styles */
        
        .main-header .logo,
        .main-header .navbar,
        .main-header .navbar .navbar-custom-menu,
        .main-header .navbar .navbar-right {
          background-color: #157FC1 !important;
          color: #fff !important;
        }
        .main-header .navbar .sidebar-toggle {
          display: none;
        }
      "))
    ),
    tabItems(
        tabItem(tabName = "ogolny",
                fluidRow(
                  column(12,
                         tags$h3("Witamy w MapMy...Walk!", style = "text-align: center; margin-top: 16px; font-size: 60px"),
                         tags$div(style = "text-align: justify; margin-left: auto; margin-right: auto; max-width: 80%;",
                                  HTML("<div style='color: black; font-size: 14px; text-align: justify;'>
                         Jesteśmy studentami drugiego roku na kierunku Inżynieria i Analiza Danych na Politechnice Warszawskiej.
                         Niniejszą aplikację stworzyliśmy w ramach projektu na przedmiocie \"Techniki wizualizacji danych\".
                         Zajęliśmy się analizą naszej codziennej aktywności pieszej na powietrzu.
                         Jej wyniki prezentujemy w specjalnie stworzonym do tego celu, funkcjonalnym dashboardzie.
                         Możecie znaleźć w nim ciekawą analizę różnych czynników dotyczących spacerów - czasu, dystansu, liczby kroków,
                         tempa chodzenia, okazji, jakości powietrza, czy słuchania muzyki.
                         </div>"),
                         )
                  )
                ),
                fluidRow(
                  column(12,
                         tags$div(style = "text-align: center; margin-left: auto; margin-right: auto; max-width: 80%;",
                                  HTML("<div style='color: black; font-size: 23px; text-align: center;'>
                       Zapraszamy do przeglądania
                       </div>")
                         )
                  )
                ),
                fluidRow(
                  column(12,
                         tags$div(style = "text-align: justify; margin-left: auto; margin-right: auto; max-width: 80%;",
                                  HTML("<div style='color: black; font-size: 15px; text-align: justify;'>
                         <br>
                         <br>A tak naprawdę gdzie chodziliśmy?
                         Oto jest 5 najczęściej odwiedzanych miejsc dla danej osoby.
                         </div>"),
                         )
                  )
                ),
                fluidRow(
                  box(leafletOutput("mapka", height = 400), width = 12)
                )
        ),
      tabItem(tabName = "aktywnosc",
              fluidRow(
                box(plotlyOutput("wykresDniowy", height = 350), width = 8),
                box(HTML("<div style='color: black; font-size: 15px; text-align: justify;'>Kiedy najczęściej chodziliśmy?
                          Na wykresie obok jest przedstawiony ile danego dnia przeszliśmy. 
                          Do wyboru po czym będziemy analizować są kroki oraz średni dystans</div>"),
                    selectInput("zmienna", "Wybierz zmienną do wykresu:", 
                                choices = c("Kroki", "Dystans")), width = 4)
              ),
              fluidRow(
                box(plotlyOutput("wykresCzasu", height = 350), width = 8),
                box(HTML("<div style='color: black; font-size: 15px; text-align: justify;'>A jak się rozkładał czas spędzany na daną aktywność w trakcie tygodnia?
                          Wykres obok przedstawia własnie ten rozkład.
                          Możemy wybrać analizę albo sumaryczną albo średnią (liczoną na dzień) oraz na samym już wykresie wybrać aktywności, którym chcemy się przyjrzeć. </div>"),
                    selectInput("typWykresuCzasu", "Wybierz typ przedstawiania danych:", 
                                choices = c("Sumaryczny", "Średni (na dzień)")), width = 4)
              ),
              fluidRow(
                box(plotlyOutput("wykresDystansu", height = 350), width = 8),
                box(HTML("<div style='color: black; font-size: 15px; text-align: justify;'>Zobaczmy teraz dzięki której aktywności przesliszmy najwięcej.
                          Tą zależność możemy zobaczyć na wykresie obok.
                          Do wyboru jest analiza sumaryczna albo średnia (liczona na dzień). </div>"),
                selectInput("typWykresuDystansu", "Wybierz typ przedstawiania danych:", 
                                choices = c("Sumaryczny", "Średni (na dzień)")), width = 4)
              )
      ),
      tabItem(tabName = "jakoscPowietrza",
              fluidRow(
                box(HTML("<div style='color: black; font-size: 14px; text-align: justify;'>
                         <b>Indeks jakości powietrza AQI</b><br>
                         AQI (ang. Air Quality Index) to sposób określenia poziomu 
                         zanieczyszczenia powietrza, który mieści się w skali od 0 do 500. 
                         Im wyższy wskaźnik, tym powietrze bardziej zanieczyszczone. 
                         Ocena jakości powietrza obejmuje przede wszystkim poziom pyłów zawieszonych PM2.5 i PM10,
                         ale także: dwutlenku siarki (SO2), ozonu (O3), tlenku azotu (NO), tlenku węgla (CO) i benzenu.<br>
                         <b>Skala jakości powietrza</b><br> Bardzo dobra 0-20 AQI<br>
                        Dobra 21-50 AQI<br>
                        Dostateczna 51-100 AQI<br>
                        Zła 101-150 AQI<br>
                        Bardzo zła 151-250 AQI<br>
                        Niebezpieczna 251+ AQI<br>
                        <b>Pory dnia</b><br>
                        Rano 6:00-12:00<br>
                        Popołudnie 12:00-18:00<br>
                        Wieczór 18:00-23:00<br>
                        Noc 23:00-6:00
                        </div>"), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("heatmapa", height = 350), width = 8),
                    box(HTML("<div style='color: black; font-size: 15px; text-align: justify;'>
                         Ile czasu spędzaliśmy w różnych warunkach jakości powietrza i w jakich porach dnia?
                         Przedstawia to wykres typu \"heatmapa\" widoczny obok.
                         Możesz wybrać, czy pokazany ma być sumaryczny czy średni czas.</div>"),
                        selectInput("typHeatmapa", "Wybierz typ przedstawiania danych:", 
                                    choices = c("Sumaryczny", "Średni")), width = 4)
                ),
                fluidRow(
                  box(plotlyOutput("violinplot", height = 350), width = 8),
                      box(HTML("<div style='color: black; font-size: 15px; text-align: justify;'>
                           Jak wygląda rozkład indeksu jakości powietrza AQI w zależności od pory dnia?
                           Wykres skrzypcowy widoczny obok przedstawia ten rozkład wynikający z naszych pomiarów.</div>"), width = 4)
                  )
                ),
      tabItem(tabName = "muzyka",
              fluidRow(
                box(plotlyOutput("wykresMuzyczny", height = 350), width = 8),
                box(HTML("<div style='color: black; font-size: 15px; text-align: justify;'>Czy muzyka wpływa na prędkość spaceru?
                         Żeby zbadać tę zależność sporządziliśmy wykres obok. Istnieją trzy warianty wykresu:
                         muzyka podczas spaceru włączona, wyłączona oraz porównanie prędkości</div>"),
                    radioButtons("checkboxMuzyka", "Czy muzyka włączona podczas spaceru?",
                                 choices = c("TAK" = "yes", "NIE" = "no", "PORÓWNAJ PRĘDKOŚĆ" = "tempo"),
                                 selected = "yes"), width = 4)
              ),
              fluidRow(
                box(plotlyOutput("wykresSpotify", height = 350), width = 8),
                box(HTML("<div style='color: black; font-size: 15px; text-align: justify;'>Jakich piosenek najczęściej słuchamy na 
                         spacerach? Żeby odpowiedzieć na to pytanie zebraliśmy dodatkowo dane ze spotify co do piosenek, których
                         słuchaliśmy gdy spacer oznaczyliśmy jako z muzyką. Wykres obok przedstawia najczęsciej odtwarzane piosenki
                         i najchętniej słuchanych artystów</div>"),
                    radioButtons("checkboxSpotify", "Według piosenek czy artystów?",
                                 choices = c("PIOSENKI" = "piosenki", "ARTYŚCI" = "artyści"),
                                 selected = "piosenki"), width = 4)
              )
      )
    )
  )
)


server <- function(input, output) {
  
  daneDoWszystkichWykresow <- reactive({
    # wybor danych
    wybranaOsoba <- input$wybranaOsoba
    if (wybranaOsoba == "Dominika") {
      df <- read.csv('dane_dominika.csv')
    } else if (wybranaOsoba == "Ola") {
      df <- read.csv('dane_ola.csv')
    } else {
      # dane Janka
      df <- read.csv('dane_janek.csv')
    }
    head(df)
    
    # przygotowanie danych
    data_extended <- df %>%
      filter(Activity.Type == "Hike") %>%
      mutate(Notes = substr(Notes, 3, nchar(Notes) - 1)) %>%
      separate(Notes, into = c("kategoria", "stanPowietrza", "AQI", "czyMuzyka", 
                               "poraDnia"), sep = " ", fill = "right")
    
    convert_dates <- function(df, date_column) {
      df %>%
        mutate(!!date_column := mdy(!!sym(date_column))) %>%
        mutate(!!date_column := format(!!sym(date_column), "%m-%d-%Y"))
    }
    df_date <- convert_dates(data_extended, "Workout.Date")
    
    df_done <- df_date %>% 
      mutate(Workout.Date = as.Date(Workout.Date, format = "%m-%d-%Y")) %>% 
      mutate(kategoria = case_when(kategoria == 'Pies' ~ 'Spacer',
                                   kategoria == 'Uczelni' ~ 'Uczelnia',
                                   TRUE ~ kategoria)) %>%
      filter(kategoria %in% c("Hobby", "Spacer", "Uczelnia", "Praca", "Zakupy", "Spotkanie")) %>%
      filter(Workout.Date >= input$dataZakres[1] & Workout.Date <= input$dataZakres[2]) %>% 
      mutate(poraDnia = case_when(poraDnia == 'popoudnie' ~ 'popoludnie',
                                  poraDnia == 'wieczr' ~ 'wieczor',
                                  TRUE ~ poraDnia))
    df_done
  })
  
  #--------------------------------------------------------------
  # WYKRES HEATMAPA (2)
  daneDoWykresu2 <- reactive({
    df_done <- daneDoWszystkichWykresow()
    df2 <- df_done %>% 
      mutate(poraDnia = case_when(poraDnia == 'popoludnie' ~ 'popołudnie',
                                  poraDnia == 'wieczor' ~ 'wieczór',
                                  TRUE ~ poraDnia),
             stanPowietrza = case_when(stanPowietrza == 'excellent' ~ 'bardzo dobra',
                                       stanPowietrza == 'fair' ~ 'dobra',
                                       stanPowietrza == 'poor' ~ 'dostateczna',
                                       stanPowietrza == 'unhealthy' ~ 'zła',
                                       stanPowietrza == 'dangerous' ~ 'bardzo zła',
                                       TRUE ~ stanPowietrza))
    typWykresu <- input$typHeatmapa
    
    # przetwarzanie danych w zależności od wybranego typu wykresu
    if (typWykresu == "Sumaryczny") {
      df2 %>%
        group_by(poraDnia, stanPowietrza) %>%
        summarise(czas = sum(Workout.Time..seconds.)/60)
    } else {
      df2 %>%
        group_by(Workout.Date, poraDnia, stanPowietrza) %>% 
        summarise(sumCzas = sum(Workout.Time..seconds.)) %>% 
        group_by(poraDnia, stanPowietrza) %>%
        summarise(czas = mean(sumCzas)/60)
    }
  })
  
  # Rysowanie wykresu
  output$heatmapa <- renderPlotly({
    data <- daneDoWykresu2()
    data$poraDnia = factor(data$poraDnia, levels = c('rano', 'popołudnie', 'wieczór', 'noc'))
    data$stanPowietrza = factor(data$stanPowietrza, levels = c('bardzo zła', 'zła', 'dostateczna', 'dobra', 'bardzo dobra'))
    
    heatmap_plot <- plot_ly(
      data = data,
      x = ~poraDnia,
      y = ~stanPowietrza,
      z = ~czas,
      type = "heatmap",
      text = ~paste(round(czas, 2), "min<br>", "Pora dnia:", poraDnia, "<br>", "Jakość powietrza:", stanPowietrza),
      hoverinfo = "text",
      colorbar = list(title = "Czas [min]",
                      len = 0.92),
      colors = RColorBrewer::brewer.pal(8, "Blues")
    ) %>%
      layout(
        title = "Czas spędzony na zewnątrz wg. pory dnia i jakości powietrza",
        xaxis = list(title = "Pora dnia", showgrid = FALSE),
        yaxis = list(title = "Jakość powietrza", showgrid = FALSE),
        margin = list(b = 60, t = 80, l = 130, r = 130),
        showlegend = TRUE,
        plot_bgcolor = 'rgba(0, 0, 0, 0.1)',
        paper_bgcolor = 'rgba(0, 0, 0, 0.1)'
      )
  })
  
  
  #--------------------------------------------------------------
  # WYKRES VIOLINPLOT (3)
  daneDoWykresu3 <- reactive({
    df_done <- daneDoWszystkichWykresow()
    df_done %>% 
      mutate(poraDnia = case_when(poraDnia == 'popoludnie' ~ 'popołudnie',
                                  poraDnia == 'wieczor' ~ 'wieczór',
                                  TRUE ~ poraDnia))
  })
  
  
  # Rysowanie wykresu
  output$violinplot <- renderPlotly({
    data <- daneDoWykresu3()
    data$poraDnia = factor(data$poraDnia, levels = c('rano', 'popołudnie', 'wieczór'))
    data$AQI = as.numeric(data$AQI)
    
    violin_plot <- plot_ly(
      data = data,
      x = ~poraDnia,
      y = ~AQI,
      type = "violin",
      box = list(
        visible = TRUE,
        width = 0.05
      ), 
      hoverinfo = "y+density"
    ) %>%
      layout(
        title = "Rozkład indeksu AQI w zależności od pory dnia",
        xaxis = list(title = "Pora dnia", showgrid = TRUE),
        yaxis = list(title = "AQI"),
        margin = list(b = 60, t = 80, l = 80, r = 80),
        plot_bgcolor = 'rgba(0, 0, 0, 0.1)',
        paper_bgcolor = 'rgba(0, 0, 0, 0.1)'
      )
  })
  
  
  #--------------------------------------------------------------
  # WYKRES CZASU (1)
  daneDoWykresu1 <- reactive({
    df_done <- daneDoWszystkichWykresow()
    df2 <- df_done %>% 
      mutate(DzienTygodnia = weekdays(Workout.Date),
             Workout.Time.minutes = Workout.Time..seconds./60) %>% 
      select(Workout.Date, DzienTygodnia, Workout.Time.minutes, kategoria) %>% 
      mutate(DzienTygodnia = factor(DzienTygodnia, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")))
    
    typWykresu <- input$typWykresuCzasu
    
    # przetwarzanie danych w zależności od wybranego typu wykresu
    if (typWykresu == "Sumaryczny") {
      df2 <- df2 %>% 
        group_by(kategoria, DzienTygodnia) %>% 
        summarise(czas = sum(Workout.Time.minutes))
    } else {
      df2 <- df2 %>% 
        group_by(Workout.Date, kategoria) %>% 
        summarise(DzienTygodnia, sumCzas = sum(Workout.Time.minutes)) %>%
        group_by(kategoria, DzienTygodnia) %>% 
        summarise(czas = mean(sumCzas))
    }
    df2
  })
  
  # Rysowanie wykresu
  output$wykresCzasu <- renderPlotly({
    
    tytul_osi_Y <- if (input$typWykresuCzasu == "Sumaryczny") {
      'Sumaryczny czas [min]'
    } else {
      'Średni czas [min]'
    }
    
    plot <- plot_ly(
      data = daneDoWykresu1(),
      x = ~DzienTygodnia,
      y = ~czas,
      type = 'bar',
      color = ~kategoria,
      colors = RColorBrewer::brewer.pal(8, "Blues"),
      legendgroup = ~kategoria
    ) %>% 
      layout(
        title = 'Czas spędzany na daną aktywność w trakcie tygodnia',
        xaxis = list(title = 'Dzień tygodnia', tickangle = 45),
        yaxis = list(title = tytul_osi_Y),
        barmode = 'group',
        legend = list(orientation = 'v', x = 1.05, y = 1),
        margin = list(b = 100, t = 70, l = 70, r = 70),
        showlegend = TRUE,
        plot_bgcolor = 'rgba(0, 0, 0, 0.1)',
        paper_bgcolor = 'rgba(0, 0, 0, 0.1)'
      )
    
    # Dodanie linii siatki dla lepszej czytelności
    plot <- plot %>% layout(
      xaxis = list(showgrid = TRUE, gridcolor = 'lightgrey'),
      yaxis = list(showgrid = TRUE, gridcolor = 'lightgrey')
    )
  })
  
  
  #--------------------------------------------------------------
  # WYKRES DNIOWY (6)
  daneDoWykresu6 <- reactive({
    df_done <- daneDoWszystkichWykresow()
    zmienna <- input$zmienna
    # przetwarzanie danych w zależności od wybranej zmiennej
    if (zmienna == "Kroki") {
      df_done <- df_done %>% 
        group_by(Workout.Date) %>% 
        summarise(zmiennaCol = sum(Steps))
    } else {
      df_done <- df_done %>% 
        group_by(Workout.Date) %>% 
        summarise(zmiennaCol = sum(Distance..km.))
    }
    df_done
  })
  
  # Rysowanie wykresu
  output$wykresDniowy <- renderPlotly({
    tytul_osi_Y <- if (input$zmienna == "Kroki") {
      'Liczba kroków'
    } else {
      'Dystans [km]'
    }
    
    plot <- plot_ly(
      data = daneDoWykresu6(),
      x = ~Workout.Date,
      y = ~zmiennaCol,
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#157FC1', width = 2)) %>% 
      layout(title = 'Aktywność w różnych dniach',
             xaxis = list(
               title = 'Data',
               tickformat = "%d-%m-%Y",
               tickangle = 45, 
               type = 'date'
             ),
             yaxis = list(
               title = tytul_osi_Y
             ),
             legend = list(
               orientation = 'h', 
               x = 0.3,            
               y = -0.1           
             ),
             margin = list(   
               b = 80,           
               t = 80,           
               l = 80,            
               r = 80            
             ),
             plot_bgcolor = 'rgba(0, 0, 0, 0.1)',
             paper_bgcolor = 'rgba(0, 0, 0, 0.1)'
      )
  })
  
  #--------------------------------------------------------------
  # WYKRES DYSTANSU (7)
  daneDoWykresuDystansu7 <- reactive({
    df_done <- daneDoWszystkichWykresow()
    typWykresu <- input$typWykresuDystansu
    
    # przetwarzanie danych w zależności od wybranego typu wykresu
    if (typWykresu == "Sumaryczny") {
      df_done <- df_done %>% 
        group_by(kategoria) %>% 
        summarise(dystans = sum(Distance..km.))
    } else {
      df_done <- df_done %>% 
        group_by(Workout.Date, kategoria) %>% 
        summarise(sumDystans = sum(Distance..km.)) %>% 
        group_by(kategoria) %>% 
        summarise(dystans = mean(sumDystans))
    }
    df_done
  })
  
  # Rysowanie wykresu
  output$wykresDystansu <- renderPlotly({
    tytul_osi_Y <- if (input$typWykresuDystansu == "Sumaryczny") {
      'Sumaryczny dystans [km]'
    } else {
      'Średni dystans [km]'
    }
    plot <- plot_ly(
      data = daneDoWykresuDystansu7(),
      x = ~kategoria,
      y = ~dystans,
      type = 'bar',
      color = ~kategoria,
      colors = RColorBrewer::brewer.pal(8, "Blues"),
      marker = list(line = list(color = 'rgba(255,255,255,0.5)', width = 2)) 
    ) %>% 
      layout(
        title = 'Dystans w zależności od aktywności',
        xaxis = list(title = 'Kategoria'),
        yaxis = list(title = tytul_osi_Y),
        showlegend = FALSE, 
        margin = list(b = 100, t = 70, l = 70, r = 70),
        plot_bgcolor = 'rgba(0, 0, 0, 0.1)',  
        paper_bgcolor = 'rgba(0, 0, 0, 0.1)' 
      )
    plot
  })
  
  
  #--------------------------------------------------------------
  # WYKRES MUZYCZNY 
  daneDoWykresuMuzycznego <- reactive({
    df_done <- daneDoWszystkichWykresow()
    czyMuzyka1 <- input$checkboxMuzyka
    if (czyMuzyka1 == "yes"){
      df_done <- df_done %>% 
        filter(kategoria!="Praca") %>% 
        filter(czyMuzyka == "tak") %>%
        group_by(kategoria) %>%
        summarize(srednia = mean(Avg.Speed..km.h., na.rm = TRUE))
    } else if (czyMuzyka1 == "no"){
      df_done <- df_done %>% 
        filter(czyMuzyka == "nie") %>%
        filter(kategoria!="Praca") %>% 
        group_by(kategoria) %>%
        summarize(srednia = mean(Avg.Speed..km.h., na.rm = TRUE))
    } else{
      daneMuzyczne <- df_done %>% 
        filter(kategoria!="Praca") %>% 
        filter(czyMuzyka == "tak") %>% group_by(kategoria) %>%
        summarize(srednia = mean(Avg.Speed..km.h., na.rm = TRUE)) %>% 
        mutate(czyMuzyka = "tak")
      daneNiemuzyczne <- df_done %>% 
        filter(czyMuzyka == "nie") %>% group_by(kategoria) %>%
        summarize(srednia = mean(Avg.Speed..km.h., na.rm = TRUE)) %>% 
        mutate(czyMuzyka = "nie")
      df_done <- bind_rows(
        daneMuzyczne %>% select(kategoria, srednia, czyMuzyka),
        daneNiemuzyczne %>% select(kategoria, srednia, czyMuzyka)
      )
    }
    print(head(df_done))
    df_done
  })
  
  wariantMuzycznego <- reactive({
    czyMuzyka1 <- input$checkboxMuzyka
    if (czyMuzyka1 == "yes" || czyMuzyka1 == "no"){
      wariant = "1"
    }
    else{
      wariant = "2"
    }
  })
  
  # Rysowanie wykresu
  output$wykresMuzyczny <- renderPlotly({
    wariant = wariantMuzycznego()
    if (wariant == "1"){
      plot <- plot_ly(
        data = daneDoWykresuMuzycznego(),
        x = ~kategoria,
        y = ~srednia,
        type = "bar",
        color = ~kategoria,
        colors = RColorBrewer::brewer.pal(8, "Blues"),
        marker = list(line = list(color = 'rgba(255,255,255,0.5)', width = 2))
      ) %>% 
        layout(
          title = 'Średnia prędkość w zależności od aktywności',
          xaxis = list(title = 'Kategoria'),
          yaxis = list(title = 'Prędkość [km/h]'),
          showlegend = FALSE,
          margin = list(b = 100, t = 70, l = 70, r = 70),
          plot_bgcolor = 'rgba(0, 0, 0, 0.1)',
          paper_bgcolor = 'rgba(0, 0, 0, 0.1)'
        )
    }
    else{
      
      plot <- plot_ly(
        data = daneDoWykresuMuzycznego(),
        x = ~kategoria,
        y = ~srednia,
        type = "bar",
        color = ~czyMuzyka,
        colors = RColorBrewer::brewer.pal(8, "Blues"),
        marker = list(line = list(color = 'rgba(255,255,255,0.5)', width = 2))
      ) %>% 
        layout(
          title = 'Średnia prędkość w zależności od aktywności',
          xaxis = list(title = 'Kategoria'),
          yaxis = list(title = 'Prędkość [km/h]'),
          showlegend = FALSE,
          margin = list(b = 100, t = 70, l = 70, r = 70),
          plot_bgcolor = 'rgba(0, 0, 0, 0.1)',
          paper_bgcolor = 'rgba(0, 0, 0, 0.1)'
        )
    }
    plot
  })
  
  #--------------------------------------------------------------
  # WYKRES SPOTIFY
  
  # funkcja służąca zamianie formatu extended streaming history
  # na format standardowy
  extended_to_normal <- function(data) {
    formatted_data <- data %>%
      select(ts, master_metadata_album_artist_name, master_metadata_track_name, ms_played) %>%
      rename("endTime" = "ts", "artistName" = "master_metadata_album_artist_name", 
             "trackName" = "master_metadata_track_name") %>%
      mutate(endTime = ymd_hms(endTime, tz = "UTC")) %>%
      mutate(endTime = format(endTime, "%Y-%m-%d %H:%M"))
    
    return(formatted_data)
  }
  
  daneDoWykresuSpotify <- reactive({
    df_done <- daneDoWszystkichWykresow()
    wybranaOsoba <- input$wybranaOsoba
    if (wybranaOsoba == "Dominika") {
      spotify_unfiltered <- fromJSON("spotify_dominika.json")
      spotify_data <- extended_to_normal(spotify_unfiltered)
    } else if (wybranaOsoba == "Ola") {
      spotify_data <- fromJSON("spotify_Ola.json")
    } else {
      # dane Janka
      spotify_unfiltered <- fromJSON("spotify_Janek.json")
      spotify_data <- extended_to_normal(spotify_unfiltered)
    }
    Sys.setlocale("LC_TIME", "C")
    spotify_data2 <- spotify_data %>% mutate(date = ymd_hm(endTime),  
                                             Workout.Date = as.Date(format(date, "%Y-%m-%d")),
                                             poraDnia = case_when(
                                               hour(date) %in% 7:11 ~ "rano",
                                               hour(date) %in% 12:16 ~ "popoludnie",
                                               hour(date) %in% 17:22 ~ "wieczor",
                                               TRUE ~ "noc"
                                             ))
    df_done <- df_done %>% 
      mutate(Workout.Date = as.Date(Workout.Date))
    df_muzyka <- inner_join(spotify_data2, df_done, by = c("Workout.Date", "poraDnia"))
    df_muzyka <- df_muzyka %>% filter(czyMuzyka == "tak")
    df_muzyka <- df_muzyka %>% group_by(trackName, artistName) %>% 
      summarise(streams = n())
    df_muzyka
  })
  
  wariantSpotify <- reactive({
    checkbox <- input$checkboxSpotify
    if (checkbox == "piosenki"){
      wariant = "1"
    }
    else{
      wariant = "2"
    }
  })
  
  # rysowanie wykresu
  output$wykresSpotify <- renderPlotly({
    df_muzyka <- daneDoWykresuSpotify()
    wariant <- wariantSpotify()
    if (wariant == "1"){
      plot <- plot_ly(
        data = df_muzyka %>% arrange(desc(streams)) %>% head(10),
        y = ~reorder(paste(artistName, "-", trackName), streams),
        
        x = ~streams,
        type = "bar",
        orientation = "h",
        marker = list(color = "rgba(29,185,84,0.7)"),
        text = ~paste("<span style='color:white; font-size:7px;'>", artistName, "-", trackName, "</span>"),
        hoverinfo = "text"
      ) %>% 
        layout(
          title = list(text = "<span style='color:white; font-size:13px;'>Najczęściej odtwarzane piosenki podczas spacerów</span>"),
          xaxis = list(title = "Liczba odtworzeń", color = "white"),
          yaxis = list(title = "", showticklabels = FALSE),
          paper_bgcolor = "black",
          plot_bgcolor = "black"
        ) %>%
        config(displayModeBar = FALSE)
    } else{
      plot <- plot_ly(
        data = df_muzyka %>% group_by(artistName) %>% 
          summarise(streams = sum(streams)) %>% 
          arrange(desc(streams)) %>% head(10),
        y = ~reorder(paste(artistName), streams),
        x = ~streams,
        type = "bar",
        orientation = "h",
        marker = list(color = "rgba(29,185,84,0.7)"),
        text = ~paste("<span style='color:white; font-size:7px;'>", artistName, "</span>"),
        hoverinfo = "text"
      ) %>% 
        layout(
          title = list(text = "<span style='color:white; font-size:13px;'>Najczęściej słuchani artyści podczas spacerów</span>"),
          xaxis = list(title = "Liczba odtworzeń utworów artysty", color = "white"),
          yaxis = list(title = "", showticklabels = FALSE),
          paper_bgcolor = "black",
          plot_bgcolor = "black"  
        ) %>%
        config(displayModeBar = FALSE)
    }
    
    plot
  })
  
  #--------------------------------------------------------------
  # MAPKI DO EKRANU GŁÓWNEGO
  output$mapka <- renderLeaflet({
    if (input$wybranaOsoba == "Dominika"){
      plot <- leaflet() %>%
        addTiles() %>% 
        addMarkers(lng = c(21.007135613409062, 21.003096903854676, 21.025053128348844, 21.02594062281341, 20.96120133352411), 
                   lat = c(52.22217811913538, 52.181335159262005, 52.17000728344316, 52.17251099582791, 52.27728800424692), 
                   popup = c("Wydział MiNI", "Siłownia", "Sklep", "Dom Dominiki", "Praca"))
    } else if (input$wybranaOsoba == "Ola"){
      plot <- leaflet() %>% 
        addTiles() %>% 
        addMarkers(lng = c(21.007135613409062, 21.059845794466803, 21.022735311672808, 21.00276756934589, 21.000396296331246),
                   lat = c(52.22217811913538, 52.13178548832623, 52.22447192663785, 52.230303483341096, 52.269464978674385),
                   popup = c("Wydział MiNI", "Dom Oli", "Praca", "Złote Tarasy", "Korty Tenisowe"))
    } else {
      plot <- leaflet() %>%
        addTiles() %>% 
        addMarkers(lng = c(21.007135613409062, 21.018375, 21.01688170046933, 20.984181897018193, 21.013635409597267), 
                   lat = c(52.22217811913538, 52.187871, 52.239135875014554, 52.2118501421227, 52.183898349045855), 
                   popup = c("Wydział MiNI", "Dom Janka", "Wydział Filozofii UW", "Centrum Nowych Technologii UW", "Biedronka"))
    }
    plot
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
