library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(rlang)
library(plotly)
library(dashboardthemes)
library(tidyr)
library(treemap)
library(shinycssloaders)

################################################################################
# Przygotowywane danych
################################################################################
# java
df1 <- read.csv("data/Sebastian_java.csv")
df2 <- read.csv("data/Mikolaj_java.csv")
df2 <- df2 %>%
  mutate(Imie = ifelse(Imie == "Mikolaj", "Mikołaj", Imie))
df3 <- read.csv("data/Malgosia_java.csv")
df3 <- df3 %>%
  mutate(Imie = ifelse(Imie == "Malgosia", "Małgosia", Imie))
df <- df1
df <- bind_rows(df1, df2, df3)
df[is.na(df)] <- 0
df <- df[,-1]
df <- df[!duplicated(df), ]
df$Data_ostatniej_modefikacji <- as.Date(substr(df$Data_ostatniej_modefikacji,1,10))
colnames(df)[which(names(df) == "break.")] <- "break"
colnames(df)[which(names(df) == "else.")] <- "else"
colnames(df)[which(names(df) == "for.")] <- "for"
colnames(df)[which(names(df) == "if.")] <- "if"
colnames(df)[which(names(df) == "non.sealed")] <- "non-sealed"
colnames(df)[which(names(df) == "while.")] <- "while"
java_keyword_list <- sort(colnames(df)[9:73])
kolory_java <- c('#5382a1', '#f89820', '#fc0703', '#DD4B39', '#1666de', '#03a1fc')

# word
malgosia_word <- read.csv("data/Malgosia-word.csv")
malgosia_word <- malgosia_word %>%
  mutate(Imie = ifelse(Imie == "Malgosia", "Małgosia", Imie))
sebastian_word <- read.csv("data/Sebastian-word.csv")

mikolaj_word <- read.csv("data/Mikolaj-word.csv")
mikolaj_word <- mikolaj_word %>%
  mutate(Imie = ifelse(Imie == "Mikolaj", "Mikołaj", Imie))
word <- rbind(malgosia_word, mikolaj_word, sebastian_word)
zmienne <- c("Sebastian", "Mikołaj", "Małgosia")
kolory_word <- c("#47bbfa", "#78dfd0", "#2929b3")

# matlab
mikolaj_matlab <- read.csv("data/Mikolaj_matlab.csv")
sebastian_matlab <- read.csv("data/Sebastian_matlab.csv")
malgosia_matlab <- read.csv("data/Malgosia_matlab.csv")

mikolaj_matlab <- mikolaj_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................) 

sebastian_matlab <- sebastian_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................)

malgosia_matlab <- malgosia_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................)
matlab_merged <- bind_rows(mikolaj_matlab, sebastian_matlab, malgosia_matlab)
matlab_merged$Data.modyfikacji <- as.Date(substr(matlab_merged$Data.modyfikacji,1,10))
matlab_merged <- matlab_merged %>%
  mutate(Imie = ifelse(Imie == "Malgosia", "Małgosia", Imie)) %>%
  mutate(Imie = ifelse(Imie == "Mikolaj", "Mikołaj", Imie))
# zakładka domowa
podsumowanie_wykres1 <- read.csv("data/ogolny_wykres1.csv")
podsumowanie_wykres1 <- podsumowanie_wykres1 %>%
  mutate(Imie = ifelse(Imie == "Malgosia", "Małgosia", Imie)) %>%
  mutate(Imie = ifelse(Imie == "Mikolaj", "Mikołaj", Imie))
podsumowanie_wykres2 <- read.csv("data/ogolny_wykres2.csv")
podsumowanie_wykres2 <- podsumowanie_wykres2 %>%
  mutate(Imie = ifelse(Imie == "Malgosia", "Małgosia", Imie)) %>%
  mutate(Imie = ifelse(Imie == "Mikolaj", "Mikołaj", Imie))
kolory_ogolny <- c("#0d5630", "#6b3c02", "#1b0952", "#6af1ab", "#f7be79", "#64b5f8")

kolor_przewodni_java <- c('#fc0703', '#03a1fc')
kolor_przewodni_word <- c('#1B5EBE', '#41A5EE')
kolor_przewodni_matlab <- c('#ed9242', '#fcf647')
# styl domyślny ----
kolor_tla <- "#352d2d"
theme_default <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "FuturaMedium"
  ,appFontColor = "white"
    ,primaryFontColor = "#434C5E"
    ,infoFontColor = "#434C5E"
    ,successFontColor = "#434C5E"
    ,warningFontColor = "#434C5E"
    ,dangerFontColor = "#434C5E"
    ,bodyBackColor = kolor_tla 
    
  ### header
  ,logoBackColor = "#151515" 
    
  ,headerButtonBackColor = "#151515"
  ,headerButtonIconColor = "#D8DEE9"
  ,headerButtonBackColorHover = "#48e0ab"
  ,headerButtonIconColorHover = "#151515" 
    
  ,headerBackColor = "#151515"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  # ,sidebarBackColor = "#151515"
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "#151515"
    ,colorMiddle = "#151515"
    ,colorEnd = kolor_tla
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 5
  ,sidebarMenuBorderRadius = 5
  
  ,sidebarShadowRadius = "" 
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#D8DEE9"
    
  ,sidebarSearchBackColor = "#4C566A"
    ,sidebarSearchIconColor = "#151515"
    ,sidebarSearchBorderColor = "#4C566A"
    
  ,sidebarTabTextColor = "#ECEFF4"
    ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "#000000"
    ,sidebarTabBorderWidth = 0
  
  # ,sidebarTabBackColorSelected = "#fc0703"
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "#42c798"
    ,colorMiddle = "#48e0ab"
    ,colorEnd = "#55f2ba"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "#000000" 
    ,sidebarTabRadiusSelected = "20px" 
  
  # ,sidebarTabBackColorHover = "#fc0703"
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "#42c798"
      ,colorMiddle = "#48e0ab"
      ,colorEnd = "#55f2ba"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "#000000"
    ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "20px"
  
  ### boxes
  ,boxBackColor = kolor_tla 
    ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = kolor_tla
    ,boxPrimaryColor = kolor_tla
    ,boxInfoColor = kolor_tla
    ,boxSuccessColor = kolor_tla
    ,boxWarningColor = kolor_tla
    ,boxDangerColor = kolor_tla
    
  ,tabBoxTabColor = "#151515"
    ,tabBoxTabTextSize = 16
  ,tabBoxTabTextColor = "#151515"
    ,tabBoxTabTextColorSelected = "#151515"
    ,tabBoxBackColor = "#BF616A"
    ,tabBoxHighlightColor = "#4C566A"
    ,tabBoxBorderRadius = 5 
  
  ### inputs
  ,buttonBackColor = "#151515"
    ,buttonTextColor = "#2E3440"
    ,buttonBorderColor = "#2E3440"
    ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "#151515"
    ,buttonTextColorHover = kolor_tla
    ,buttonBorderColorHover = "#2E3440"
    
  ,textboxBackColor = "#151515" 
    ,textboxBorderColor = "#48e0ab" 
    ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "#151515"
    ,textboxBorderColorSelect = "#47fcf6"
    
  ### tables
  ,tableBackColor = "#151515"
    ,tableBorderColor = "#2E3440"
    ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)

################################################################################
# Serwer
################################################################################
server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------
  # Wykresy Java
  #-----------------------------------------------------------------------------
  
  # Wykres 1 -------------------------------------------------------------------
  output$JavaWykres1 <- renderPlotly({
    df_date <- df %>% 
      group_by(Data_ostatniej_modefikacji, Imie) %>% 
      summarise(liczba= n()) %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>% 
      mutate(Rok_i_Miesiac = substr(Data_ostatniej_modefikacji, 1, 7)) %>%
      group_by(Rok_i_Miesiac, Imie) %>% 
      mutate(liczba_w_miesiacu = sum(liczba))
    
    plot_ly(data = df_date %>% filter(Imie %in% c("Sebastian", "Mikołaj", "Małgosia")),
            x = ~Data_ostatniej_modefikacji, y = ~liczba,
            type = "bar", color = ~Imie,
            hoverinfo = 'text',
            hovertext = ~paste0("Liczba stworzonych plików \nw ",
                                c("styczniu", "lutym", "marcu", "kwietniu", "maju", "czerwcu",
                                  "lipcu", "sierpniu", "wrześniu", "październiku", "listopadzie", "grudniu"
                                )[as.numeric(substr(Data_ostatniej_modefikacji, 6, 7))],
                                " w ", substr(Data_ostatniej_modefikacji, 1, 4), " roku",
                                "\nu ", c("Sebastiana", "Mikołaja", "Małgosi"
                                )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                ": ", liczba_w_miesiacu,
                                "\nLiczba stworzonych plików\nw ", Data_ostatniej_modefikacji,
                                ": ", liczba),
            textposition = "none",
            colors =  kolory_java,
            xperiod="M1", xperiodalignment="middle"
    ) %>%
      layout(barmode = 'group',
             font = list(family = "FuturaMedium", color = "white", size = 14),
             title = list(text = "Tworzenie plików .java w czasie", font = list(size = 22)),
             xaxis = list(fixedrange = TRUE,
                          title = list(text = "Data", font = list(size = 18))),
             yaxis=list(fixedrange=TRUE,
                        title = list(text = "Liczba utworzonych plików", font = list(size = 18)),
                        gridcolor = "grey"),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)",
             margin = list(t = 40)
             ) %>%
      config(displayModeBar = FALSE,
             locale = 'pl') 
  })
  # Wykres 2 -------------------------------------------------------------------
  output$JavaWykres2 <- renderPlotly({
    df_date <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
      group_by(Imie) %>%
      summarise(liczba_komentarzy_w_linii = mean(liczba_komentarzy_w_linii),
                liczba_komentarzy_w_bloku = mean(liczba_komentarzy_w_bloku)) %>%
      mutate(Imie = c("Sebastian", "Mikołaj", "Małgosia")[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))])
    plot_ly(data = df_date,
            x = ~Imie, y = ~liczba_komentarzy_w_linii,
            type = "bar", name = 'Komentarze\njednoliniowe;\n<i><sup>//komentarz</sup></i>',
            hoverinfo = 'text',
            hovertext = ~paste0("Średnia liczba\nkomentarzy\njednoliniowych na plik\nu ",
                                c("Sebastiana", "Mikołaja", "Małgosi"
                                )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                ":\n", round(liczba_komentarzy_w_linii, 2)),
            textposition = "none",
            marker = list(color = kolory_java[5])
    ) %>%
      add_trace(name = "Komentarze\nwieloliniowych;\n<i><sup>/*komentarz*/</sup></i>",
                y = ~liczba_komentarzy_w_bloku,
                hoverinfo = 'text',
                hovertext = ~paste0("Średnia liczba\nkomentarzy\nwieloliniowych na plik\nu ",
                                    c("Sebastiana", "Mikołaja", "Małgosi"
                                    )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                    ":\n", round(liczba_komentarzy_w_bloku, 2)),
                marker = list(color = kolory_java[4])
      ) %>%
      layout(barmode = 'group',
             font = list(family = "FuturaMedium", color = "white", size = 14),
             title = list(text = "Średnia liczba komentarzy na plik", font = list(size = 22)),
             xaxis = list(fixedrange = TRUE,
                          title = list(text = "Osoba", font = list(size = 18))),
             yaxis=list(fixedrange=TRUE,
                        title = list(text = "Liczba komentarzy", font = list(size = 18)),
                        gridcolor = "grey"
                        ),
             legend = list(
               itemclick = FALSE,
               itemdoubleclick = FALSE,
               groupclick = FALSE
             ),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)",
             margin = list(t = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  # Wykres 3 -------------------------------------------------------------------
  output$JavaWykres3 <- renderPlotly({
    df_date <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
      group_by(Imie) %>%
      summarise(liczba_komentarzy_w_linii = sum(liczba_komentarzy_w_linii),
                liczba_komentarzy_w_bloku = sum(liczba_komentarzy_w_bloku),
                liczba_znakow_w_komantarzu_w_linii = sum(liczba_znakow_w_komantarzu_w_linii),
                liczba_znakow_w_komentarzu_w_bloku = sum(liczba_znakow_w_komentarzu_w_bloku)) %>%
      mutate(srednia_liczba_znakow_w_komantarzu_w_linii = if_else(liczba_komentarzy_w_linii == 0, 0, liczba_znakow_w_komantarzu_w_linii/liczba_komentarzy_w_linii),
             srednia_liczba_znakow_w_komentarzu_w_bloku = if_else(liczba_komentarzy_w_bloku == 0, 0, liczba_znakow_w_komentarzu_w_bloku/liczba_komentarzy_w_bloku)) %>%
      mutate(Imie = c("Sebastian", "Mikołaj", "Małgosia")[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))])
    plot_ly(data = df_date,
            x = ~Imie, y = ~srednia_liczba_znakow_w_komantarzu_w_linii,
            type = "bar", name = "Komentarze\njedniolinowe;\n<i><sup>//komentarz</sup></i>",
            hoverinfo = 'text',
            hovertext = ~paste0("Średnia liczba znaków\nw komentarzach jedniolinowych\nu ",
                                c("Sebastiana", "Mikołaja", "Małgosi"
                                )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                ":\n", round(srednia_liczba_znakow_w_komantarzu_w_linii, 2)),
            textposition = "none",
            marker = list(color = kolory_java[5])
    ) %>%
      add_trace(name = "Komentarze\nwieloliniowe;\n<i><sup>/*komentarz*/</sup></i>",
                y = ~srednia_liczba_znakow_w_komentarzu_w_bloku,
                hoverinfo = 'text',
                hovertext = ~paste0("Średnia liczba znaków\nw komentarzach wieloliniowych\nu ",
                                    c("Sebastiana", "Mikołaja", "Małgosi"
                                    )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                    ":\n", round(srednia_liczba_znakow_w_komentarzu_w_bloku, 2)),
                marker = list(color = kolory_java[4])
      ) %>%
      layout(barmode = 'group',
             font = list(family = "FuturaMedium", color = "white", size = 14),
             title = list(text = "Średnia liczba znaków na komentarz", font = list(size = 22)),
             xaxis = list(fixedrange = TRUE,
                          title = list(text = "Osoba", font = list(size = 18))
                          ),
             yaxis=list(fixedrange=TRUE,
                        title = list(text = "Liczba komentarzy", font = list(size = 18)),
                        gridcolor = "grey"),
             legend = list(
               itemclick = FALSE,
               itemdoubleclick = FALSE,
               groupclick = FALSE
             ),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)",
             margin = list(t = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  # Wykres 6 -------------------------------------------------------------------
  output$JavaWykres6 <- renderUI({
    new_df_Mikolaj <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Mikołaj")
    najdluzszy_wyraz_Mikolaj <- new_df_Mikolaj$Najdluzszy_wyraz[nchar(new_df_Mikolaj$Najdluzszy_wyraz) == max(nchar(new_df_Mikolaj$Najdluzszy_wyraz))] %>%
      head(1)
    new_df_Sebastian <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Sebastian")
    najdluzszy_wyraz_Sebastian <- new_df_Sebastian$Najdluzszy_wyraz[nchar(new_df_Sebastian$Najdluzszy_wyraz) == max(nchar(new_df_Sebastian$Najdluzszy_wyraz))] %>%
      head(1)
    new_df_Malgosia <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Małgosia")
    najdluzszy_wyraz_Malgosia <- new_df_Malgosia$Najdluzszy_wyraz[nchar(new_df_Malgosia$Najdluzszy_wyraz) == max(nchar(new_df_Malgosia$Najdluzszy_wyraz))] %>%
      head(1)
    HTML(paste("<b>Małgosia</b>:<br/>",
               najdluzszy_wyraz_Malgosia, 
               "<br/><small>(", nchar(najdluzszy_wyraz_Malgosia), "znaków)</small>"),
         "<br/><b>Mikołaj</b>:<br/>",
         najdluzszy_wyraz_Mikolaj, 
         "<br/><small>(", nchar(najdluzszy_wyraz_Mikolaj), "znaków)</small>",
         "<br><b>Sebastian</b>:<br/>",
         najdluzszy_wyraz_Sebastian, 
         "<br/><small>(", nchar(najdluzszy_wyraz_Sebastian), "znaków)</small>",
    )
    
  })
  # Wykres 7 -------------------------------------------------------------------
  output$JavaWykres7 <- renderUI({
    validate(
      need(input$txtIn %in% java_keyword_list, "Proszę podać poprawny wraz")
    )
    df_tmp <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2], input$txtIn %in% java_keyword_list)
    Mikolaj_df_tmp <- df_tmp %>% filter(Imie == "Mikołaj")
    Sebastian_df_tmp <- df_tmp %>% filter(Imie == "Sebastian")
    Malgosia_df_tmp <- df_tmp %>% filter(Imie == "Małgosia")
    HTML(paste0("<b>Małgosia</b>: ", round(Malgosia_df_tmp %>% summarise(sum(!!sym(input$txtIn))) %>% pull() / Malgosia_df_tmp %>% summarise(liczba_plikow = n()), 2),
                "<br/><b>Mikołaj</b>: ", round(Mikolaj_df_tmp %>% summarise(sum(!!sym(input$txtIn))) %>% pull() / Mikolaj_df_tmp %>% summarise(liczba_plikow = n()), 2),
                "<br/><b>Sebastian</b>: ", round(Sebastian_df_tmp %>% summarise(sum(!!sym(input$txtIn))) %>% pull() / Sebastian_df_tmp %>% summarise(liczba_plikow = n()), 2)
    ))
  })
  
  # Wykres 8 -------------------------------------------------------------------
  output$JavaWykres8 <- renderValueBox({
    valueBox(tags$p(df_tmp <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
                      mutate(Imie = c("Sebastian", "Mikołaj", "Małgosia")[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))]) %>%
                      filter(Imie %in% input$JavaSelectInput1) %>% summarise(ilosc_znakow = sum(Liczba_znaków)) %>% pull(),
                    style = "font-size: 175%; text-align: center;color: #FFFFFF;"),
             tags$p("znaków napisanych w sumie", style = "font-size: 125%; font-family: 'FuturaMedium'; text-align: center;color: #FFFFFF;"), 
             color = "red")    
  })
  # Wykres 9 -------------------------------------------------------------------
  output$JavaWykres9 <- renderValueBox({
    valueBox(tags$p(df_tmp <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
                      mutate(Imie = c("Sebastian", "Mikołaj", "Małgosia")[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))]) %>%
                      filter(Imie %in% input$JavaSelectInput1) %>% summarise(ilosc_znakow = sum(Liczba_linii)) %>% pull(),
                    style = "font-size: 175%; text-align: center;color: #FFFFFF;"),
             tags$p("linijek napisanych w sumie", style = "font-size: 125%; font-family: 'FuturaMedium'; text-align: center;color: #FFFFFF;"), 
             color = "red")    
  })
  # Wykres 10 ------------------------------------------------------------------
  output$JavaWykres10 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color:", kolor_tla, ";",
      "border: 2px solid ", "#fc0703", ";",
      "padding: 10px;",
      "margin-top: 10px;",
      "text-align: justify;"
    )
    text <- "Wykres przedstawia liczbę utworzonych plików z rozszerzeniem
                     .java w czasie. Klikając na legendę można wybać osoby, do których
                      dane będą się odnosić. Po najechaniu na słupek można
                     zobaczyć dokładną liczbę utworzonych plików w wybranym miesiącu. Dodatkowo przesuwając
                     kursor wzdłuż kolumny w dolnej części opisu wyświetli się informacja
                     o liczbie utworzonych plików z podziałem na dni."
    div(style = text_style, HTML(text))
  })

  #-----------------------------------------------------------------------------
  # Wykresy Word ***************************************************************
  #-----------------------------------------------------------------------------
  
  # word - wykres tworzenia plikow ---------------------------------------------
  output$wykres1_1 <- renderPlotly({
    word$Data.utworzenia.pliku <- as.Date(substr(word$Data.utworzenia.pliku,1,10))
    word1 <- word %>% 
      group_by(Data.utworzenia.pliku, Imie) %>%
      summarise(n = n()) %>% 
      mutate(data = substr(Data.utworzenia.pliku, 1, 7)) %>%
      group_by(data, Imie) %>%
      mutate(liczba_w_miesiacu = sum(n)) %>% 
      filter(Data.utworzenia.pliku >= input$data[1], Data.utworzenia.pliku <= input$data[2])
    
    plot_ly(data = word1 %>% filter(Imie %in% c("Sebastian", "Mikołaj", "Małgosia")),
            x = ~Data.utworzenia.pliku, 
            y = ~n,
            type = "bar",
            color = ~Imie,
            hoverinfo = 'text',
            hovertext = ~paste0("Liczba stworzonych plików \nw ",
                                c("styczniu", "lutym", "marcu", "kwietniu", "maju", "czerwcu",
                                  "lipcu", "sierpniu", "wrześniu", "październiku", "listopadzie", "grudniu"
                                )[as.numeric(substr(Data.utworzenia.pliku, 6, 7))],
                                " w ", substr(Data.utworzenia.pliku, 1, 4), " roku",
                                "\nu ", c("Sebastiana", "Mikołaja", "Małgosi"
                                )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                ": ", liczba_w_miesiacu,
                                "\nLiczba stworzonych plików\nw ", Data.utworzenia.pliku,
                                ": ", n),
            textposition = "none",
            colors =  c(kolory_word[3], kolory_word[2], kolory_word[1]),
            xperiod="M1", xperiodalignment="middle"
    ) %>%
      layout(barmode = 'group',
             font = list(family = "FuturaMedium", color = "white", size = 14),
             title = list(text = "Tworzenie plików .docx w czasie", font = list(size = 22)),
             margin = list(t = 40),
             xaxis = list(fixedrange = TRUE,
                          title = list(text = "Data", font = list(size = 18))),
             yaxis=list(fixedrange=TRUE,
                        title = list(text = "Liczba utworzonych plików", font = list(size = 18)),
                        gridcolor = "grey"),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE,
             locale = 'pl') 
  })
  
  # word - analiza interpunkcji ------------------------------------------------
  output$wykres2 <- renderPlotly({
    kolor <- kolory_word[1]

    if (input$zmienna == "Mikołaj")
    {
      kolor <- kolory_word[2]
    } else if (input$zmienna == "Małgosia") {
      kolor <- kolory_word[3]
    }else if (input$zmienna == "Sebastian") {
      kolor <- kolory_word[1]
    }
    
    
    df_word2 <- word %>% 
      filter(Imie == input$zmienna) %>% 
      filter(Data.utworzenia.pliku >= input$data[1], Data.utworzenia.pliku <= input$data[2]) %>% 
      summarise(Kropka = sum(Ilosc.kropek), Przecinek = sum(Ilosc.przecinkow), Dwukropek = sum(Ilosc.dwukropkow), Pozostałe = sum(Ilosc.pozostalych.znakow), Pytajnik = sum(Ilosc.pytajnikow), Wykrzyknik = sum(Ilosc.wykrzyknikow), Myślnik= sum(Ilosc.myslnikow))
    
    df_word2 <- df_word2 %>% 
      pivot_longer(cols = c("Kropka", "Przecinek", "Dwukropek", "Pozostałe", "Pytajnik", "Wykrzyknik", "Myślnik"), names_to = "Kolumna", values_to = "Wartosc")
    
    plot_ly(
      df_word2,
      x = ~reorder(Kolumna, -Wartosc),
      y = ~Wartosc,
      type = 'bar',
      marker = list(color = kolor)
      
    ) %>% 
      layout(
        font = list(family = "FuturaMedium", color = "white", size = 14),
        title = list(text = "Interpunkcja - word", font = list(size = 22)),
        xaxis = list(title = list(text= "Znaki Interpunkcjne", font = list(size = 18))),
        yaxis = list(title = list(text= "Liczba", font = list(size = 18))),
        showlegend = FALSE,
        margin = list(t = 40),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE,
             locale = 'pl') 
    
  })
  
  
  # word - zaleznosc miedzy kropkami / przecinkami ... -------------------------
  
  output$wykres3 <- renderPlotly({
    
    kolor <- c(kolory_word[3], kolory_word[2], kolory_word[1])
    df_word3 <- word %>% 
      filter(Data.utworzenia.pliku >= input$data[1], Data.utworzenia.pliku <= input$data[2]) %>% 
      select(Ilosc.kropek, Ilosc.słow., Ilosc.przecinkow, Imie)
    plot_ly(data = df_word3 %>% 
            filter(Ilosc.kropek <= 200),
            x = ~Ilosc.kropek,
            y = ~Ilosc.przecinkow,
            type = 'scatter',
            size = ~Ilosc.słow.,
            color = ~Imie,
            colors = kolor,
            mode = 'markers',
            opacity = 1
    )%>%
      layout(
        font = list(family = "FuturaMedium", color = "white", size = 14),
        title = list(text = "Zależność Między Kropkami i Przecinkami", font = list(size = 22)),
        xaxis = list(title = list(text= "Liczba Kropek", font = list(size = 18))),
        yaxis = list(title = list(text= "Liczba Przecinków", font = list(size = 18))),
        showlegend = list(show = TRUE),
        margin = list(t = 40),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>% 
      config(displayModeBar = FALSE,
             locale = 'pl') 
    
  })


  #-----------------------------------------------------------------------------
  # Wykresy MATLAB
  #-----------------------------------------------------------------------------
  
  # Wykres 1 dla zakładki MATLAB ------------------------------------------------
  output$MATLABWykres1 <- renderPlotly({
    p <- plot_ly()
    
    if ("Małgosia" %in% input$matlab1){
      malgosia_matlab_filtered <- malgosia_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])
      
      p <- add_trace(p,
                     data = malgosia_matlab_filtered,
                     margin = list(l = 50, r = 50, b = 50, t = 100),
                     type = "violin",
                     x = ~Imie,
                     y = ~Liczba.operatorow.otoczonych.spacjami / Liczba.operatorow,
                     box = list(visible = FALSE),
                     meanline = list(visible = TRUE),
                     spanmode = "hard",
                     name = "Małgosia"
      )
    }
    
    if ("Mikołaj" %in% input$matlab1) {
      mikolaj_matlab_filtered <- mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.operatorow < 70)
      
      p <- add_trace(p,
                     data = mikolaj_matlab_filtered,
                     type = "violin",
                     x = ~Imie,
                     y = ~Liczba.operatorow.otoczonych.spacjami / Liczba.operatorow,
                     box = list(visible = FALSE),
                     meanline = list(visible = TRUE),
                     spanmode = "hard",
                     name = "Mikołaj"
      )
    }
    
    if ("Sebastian" %in% input$matlab1) {
      sebastian_matlab_filtered <- sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.operatorow < 70)
      
      p <- add_trace(p,
                     data = sebastian_matlab_filtered,
                     type = "violin",
                     x = ~Imie,
                     y = ~Liczba.operatorow.otoczonych.spacjami / Liczba.operatorow,
                     box = list(visible = FALSE),
                     meanline = list(visible = TRUE),
                     spanmode = "hard",
                     name = "Sebastian"
      )
    }
    
    names <- input$matlab1  # Get the selected names
    if (length(names) == 1) {
      names_str <- paste(names, collapse = "")  # Combine names into a single string
      title <- paste("Konwencja zapisu operatorów matematycznych -", names_str)
    } else {
      title <- "Konwencja zapisu operatorów matematycznych"
    }
    
    p %>% 
      layout(font = list(family = "FuturaMedium", color = "white", size = 14),
            title = list(text = title, font = list(size = 22)), 
             xaxis = list(title =list(text = "Autor", font = list(size = 18))), 
             y_axis_values <- seq(0, 1, 0.1),
             yaxis = list(
               title = list(text = "Udział operatorów zapisanych ze spacjami wokół", font = list(size = 18)),
               tickvals = y_axis_values,
               ticktext = sprintf("%.0f%%", y_axis_values * 100),
               tickformat = "%"),  # Dodanie formatu procentowego do osi Y
             plot_bgcolor = kolor_tla,  # Kolor tła wykresu
             paper_bgcolor = kolor_tla,
             margin = list(t = 40),
             aspectratio = list(x = 1, y = 1)  # Ustawienie stosunku osi X do Y
      )
  })
  
  
  
  
  # Wykres 2 dla zakładki MATLAB ------------------------------------------------
  
  
  output$MATLABWykres2 <- renderPlotly({
    p <- plot_ly()
    
    if ("Małgosia" %in% input$matlab2) {
      malgosia_matlab_filtered <- malgosia_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])
      
      
      p <-   add_trace(p,
                       data = malgosia_matlab_filtered,
                       type = "box",
                       x = ~Imie,
                       y = ~round(Liczba.wierszy.zakonczonych.srednikiem / Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem, 2)
                       ,
                       name = "Małgosia",
                       boxmean = TRUE,
                       boxpoints = ""
      )
      
    }
    
    if ("Mikołaj" %in% input$matlab2) {
      mikolaj_matlab_filtered <- mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])
      
          p <- add_trace(p,
                         data = mikolaj_matlab_filtered,
                         type = "box",
                         x = ~Imie,
                         y = ~round(Liczba.wierszy.zakonczonych.srednikiem / Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem, 2),
                         
                         name = "Mikołaj",
                         boxmean = TRUE,
                         boxpoints = ""
              )
      
    }
    
    if ("Sebastian" %in% input$matlab2) {
      sebastian_matlab_filtered <- sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem < 60)
      
    
                p <-   add_trace(p,
                    data = sebastian_matlab_filtered,
                    type = "box",
                    x = ~Imie,
                    y = ~round(Liczba.wierszy.zakonczonych.srednikiem / Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem, 2),
                    
                    name = "Sebastian",
                    boxmean = TRUE,
                    boxpoints = ""
                  )
      
    }
    
    names <- input$matlab2  # Get the selected names
    if (length(names) == 1) {
      names_str <- paste(names, collapse = "")  # Combine names into a single string
      title <- paste("Stawianie średników na końcu linii -", names_str)
    } else {
      title <- "Stawianie średników na końcu linii"
    }
    y_axis_values <- seq(0, 1, 0.1)
    p %>% 
      layout(font = list(family = "FuturaMedium", color = "white", size = 14),
            title = list(text = title, font = list(size = 22)), 
             xaxis = list(title = list(text = "Autor", font = list(size = 18))),
             y_axis_values <- seq(0, 1, 0.1),
             yaxis = list(
               title = list(text = "Udział wierszy zakończonych średnikiem w pliku", font = list(size = 18)),
               tickvals = y_axis_values,
               ticktext = sprintf("%.0f%%", y_axis_values * 100),
               tickformat = "%"),  # Dodanie formatu procentowego do osi Y
             plot_bgcolor = kolor_tla,  # Kolor tła wykresu
             paper_bgcolor = kolor_tla,
             margin = list(t = 40),
             xaxis = list(scaleanchor = "y", scaleratio = 1),  # Ustawienie skali osi X
             grid = list(
               gridwidth = 5,  # Grubość siatki
               gridcolor = "white"  # Kolor siatki
             ),
             legend = list(title = "Autor")) 
  })
  
  
  # Wykres 3 dla zakładki MATLAB ------------------------------------------------
  output$MATLABWykres3 <- renderPlotly({
    p <- plot_ly()
    
    
    malgosia_matlab_filtered <-  malgosia_matlab %>% 
      filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
      summarise(Srednia_liczba_znakow_w_niepustym_wierszu = sum(Liczba.znaków.)
                / (sum(Liczba.wierszy) - sum(Liczba.pustych.linii))) %>% 
      mutate(Imie = "Małgosia")
    
    
    
    p <- add_bars(p,
                  data = malgosia_matlab_filtered,
                  x = ~Imie,
                  y = ~Srednia_liczba_znakow_w_niepustym_wierszu,
                  name = "Małgosia",
                  opacity = 0.85
    )
    
    
    # (analogiczne bloki dla innych autorów)
    

      mikolaj_matlab_filtered <-  mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        summarise(Srednia_liczba_znakow_w_niepustym_wierszu = sum(Liczba.znaków.)
                  / (sum(Liczba.wierszy) - sum(Liczba.pustych.linii))) %>% 
        mutate(Imie = "Mikołaj")
      
      
      p <- add_bars(p,
                    data = mikolaj_matlab_filtered,
                    x = ~Imie,
                    y = ~Srednia_liczba_znakow_w_niepustym_wierszu,
                    marker = list(color = "#FF7F0E"),
                    name = "Mikołaj",
                    opacity = 0.85
      )

    
      sebastian_matlab_filtered <-  sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        summarise(Srednia_liczba_znakow_w_niepustym_wierszu = sum(Liczba.znaków.) 
                  / (sum(Liczba.wierszy) - sum(Liczba.pustych.linii))) %>% 
        mutate(Imie = "Sebastian")
      
      
      p <- add_bars(p,
                    data = sebastian_matlab_filtered,
                    x = ~Imie,
                    y = ~Srednia_liczba_znakow_w_niepustym_wierszu,
                    marker = list(color = "#2C9B2C"),
                    name = "Sebastian",
                    opacity = 0.85
      )
    

    
    p %>% 
      layout(font = list(family = "FuturaMedium", color = "white", size = 14),
             title = list(text = "Liczba znaków w niepustym wierszu", font = list(size = 22)), 
             yaxis = list(title = list(text = "Średnia liczba znaków w niepustym wierszu", font = list(size = 18))), 
             xaxis = list(title = list(text = "Autor", font = list(size = 18))),
             plot_bgcolor = kolor_tla,  # Kolor tła wykresu
             paper_bgcolor = kolor_tla,
             margin = list(t = 40),
             grid = list(
               gridwidth = 5,  # Grubość siatki
               gridcolor = "white"  # Kolor siatki
             )) 
  })
  
  
  # Wykres 4 dla zakładki MATLAB ------------------------------------------------
  
  output$MATLABWykres4 <- renderPlotly({
    p_boxplot <- plot_ly()
    
    if ("Małgosia" %in% input$matlab4) {
      malgosia_matlab_filtered <- malgosia_matlab %>%
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])%>% 
        mutate( przelicznik  = Laczna.dlugosc.komentarzy / Liczba.wierszy) %>% 
        filter(przelicznik < 0.5) %>% 
        mutate(Imie = "Małgosia")
      
      p_boxplot <- add_trace(p_boxplot,
                             data = malgosia_matlab_filtered,
                             type = "box",
                             x = ~Imie,
                             y = ~Laczna.dlugosc.komentarzy / Liczba.wierszy,
                             name = "Małgosia",
                             marker = list(color = "#6E8CB4"),
                             boxmean = TRUE,
                             boxpoints = ""
      )
    }
    
    if ("Mikołaj" %in% input$matlab4) {
      mikolaj_matlab_filtered <- mikolaj_matlab %>%
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        mutate(Imie = "Mikołaj")
      
      p_boxplot <- add_trace(p_boxplot,
                             data = mikolaj_matlab_filtered,
                             type = "box",
                             x = ~Imie,
                             y = ~Laczna.dlugosc.komentarzy / Liczba.wierszy,
                             name = "Mikołaj",
                             boxmean = TRUE,
                             boxpoints = ""
      )
    }
    
    if ("Sebastian" %in% input$matlab4) {
      sebastian_matlab_filtered <- sebastian_matlab %>%
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])%>% 
        mutate(Imie = "Sebastian")
      
      p_boxplot <- add_trace(p_boxplot,
                             data = sebastian_matlab_filtered,
                             type = "box",
                             x = ~Imie,
                             y = ~Laczna.dlugosc.komentarzy / Liczba.wierszy,
                             name = "Sebastian",
                             hovertemplate = "Value: %{y}<extra></extra>",
                             boxmean = TRUE,
                             boxpoints = ""
      )
    }
    
    
    y_axis_values <- seq(0, 1, 0.1)
    p_boxplot %>%
      layout(font = list(family = "FuturaMedium", color = "white", size = 14),
            title = list(text = "Jaką część pliku stanowią komentarze?", font = list(size = 18)),
             xaxis = list(title = list(text = "Autor", font = list(size = 18))),
             yaxis = list(
               title = list(text = "Stosunek długości komentarzy do długości pliku", font = list(size = 18)),
               tickvals = y_axis_values,
               ticktext = sprintf("%.0f%%", y_axis_values * 100),
               tickformat = "%"),  # Dodanie formatu procentowego do osi Y
             plot_bgcolor = kolor_tla,
             paper_bgcolor = kolor_tla,
             margin = list(t = 40),
             aspectratio = list(x = 1, y = 1),
             xaxis2 = list(domain = c(0.8, 1), anchor = "y2"),
             grid = list(gridwidth = 5, gridcolor = "white"),
             legend = list(title = "Autor")
      )
  })
  
  ### Wykres 5 dla zakładki MATLAB-------------
  
  output$MATLABWykres5 <- renderPlotly({
    
    df_date_matlab <- matlab_merged %>%
      group_by(Data.modyfikacji, Imie) %>%
      summarise(liczba = n()) %>%
      filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>%
      mutate(Rok_i_Miesiac = substr(Data.modyfikacji, 1, 7)) %>%
      group_by(Rok_i_Miesiac, Imie) %>%
      mutate(liczba_w_miesiacu = sum(liczba))
    
    selected_names <- input$matlab5
    
    plot_ly(data = df_date_matlab %>% filter(Imie %in% selected_names),
            x = ~Data.modyfikacji, y = ~liczba,
            type = "bar", color = ~Imie,
            hoverinfo = 'text',
            hovertext = ~paste0("Liczba stworzonych plików \nw ",
                                c("styczniu", "lutym", "marcu", "kwietniu", "maju", "czerwcu",
                                  "lipcu", "sierpniu", "wrześniu", "październiku", "listopadzie", "grudniu"
                                )[as.numeric(substr(Data.modyfikacji, 6, 7))],
                                " w ", substr(Data.modyfikacji, 1, 4), " roku",
                                "\nu ", c( "Małgosi", "Mikołaja", "Sebastiana"
                                )[match(Imie, c("Małgosia", "Mikołaj", "Sebastian"))],
                                ": ", liczba_w_miesiacu,
                                "\nLiczba stworzonych plików\nw ", Data.modyfikacji,
                                ": ", liczba),
            textposition = "none",
            colors = c("#1F77B4", "#FF7F0E", "#2C9B2C"),
            xperiod = "M1", xperiodalignment = "middle",
            opacity = 0.85
    ) %>%
      layout(barmode = 'group',
             font = list(family = "FuturaMedium", color = "white", size = 14),
             title = list(text = "Tworzenie plików .m w czasie", font = list(size = 22)),
             xaxis = list(fixedrange = TRUE,
                          title = list(text = "Data", font = list(size = 14))),
             yaxis = list(fixedrange = TRUE,
                          title = list(text = "Liczba utworzonych plików", font = list(size = 18)),
                          gridcolor = "grey"),
             legend = list(
               itemclick = FALSE,
               itemdoubleclick = FALSE,
               groupclick = FALSE
             ),
             margin = list(t = 40),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)"
             ) %>%
      config(displayModeBar = FALSE,
             locale = 'pl') 
  })
  
  # Jaką część pliku stanowią komentarze?
  
  #-----------------------------------------------------------------------------
  # Wykresy Podsumowanie
  #-----------------------------------------------------------------------------

  ### podsumowanie -> wykres numer 1--------------------------------------------
  output$Podsumowanie_wykres1 <- renderPlotly({
    
    df_podsumowanie <- podsumowanie_wykres1
    df_podsumowanie$year <- as.integer(df_podsumowanie$year)
    df_podsumowanie$month <- as.integer(df_podsumowanie$month)
    df_podsumowanie$data <- as.Date(paste(df_podsumowanie$year, "-", df_podsumowanie$month, "-01", sep = ""), format = "%Y-%m-%d")
    df_podsumowanie <- df_podsumowanie %>% 
      arrange(month) %>% 
      arrange(year) %>% 
      filter(data >= input$data[1] & data <= input$data[2]) %>% 
      filter(Imie == input$podsumowanie_11)
    df_podsumowanie <- df_podsumowanie %>% 
      mutate( nn = 
                case_when(
                  Rozszerzenie == "docx" ~ n,
                  Rozszerzenie == "m" & month < 9 & year == 2023 ~ NA,
                  Rozszerzenie == "java" & year < 2023 ~ NA,
                  Rozszerzenie == "m" & year < 2023 ~ NA,
                  TRUE ~ n
                )
      )
    df_podsumowanie <- df_podsumowanie[complete.cases(df_podsumowanie[, "nn"]), ]
    
    df1 <- df_podsumowanie %>% 
      filter(Rozszerzenie == "java")
    
    df2 <- df_podsumowanie %>% 
      filter(Rozszerzenie == "docx")
    
    df3 <- df_podsumowanie %>% 
      filter(Rozszerzenie == "m")
    
    w <- plot_ly()
    w %>% 
    add_trace(x = df1$data, y = df1$nn, type = 'scatter', mode = 'lines', line = list(color = kolory_ogolny[2]), showlegend = FALSE,  hoverinfo = "none") %>% 
      add_trace(x = c(df1$data, rev(df1$data)), y = c(df1$nn, rep(min(df1$nn), length(df1$nn))),
                fill = 'toself', type = 'scatter', mode = 'none', fillcolor = kolory_ogolny[5], name = ".java", hoverinfo = "none") %>% 
      add_trace(x = df2$data, y = df2$nn, type = 'scatter', mode = 'lines', line = list(color = kolory_ogolny[1]), showlegend = FALSE, hoverinfo = "none") %>% 
      add_trace(x = c(df2$data, rev(df2$data)), y = c(df2$nn, rep(min(df2$nn), length(df2$nn))),
                fill = 'toself', type = 'scatter', mode = 'none', fillcolor = kolory_ogolny[4], name = ".docx", hoverinfo = "none") %>% 
      add_trace(x = df3$data, y = df3$nn, type = 'scatter', mode = 'lines', line = list(color = kolory_ogolny[3]), showlegend = FALSE, hoverinfo = "none") %>% 
      add_trace(x = c(df3$data, rev(df3$data)), y = c(df3$nn, rep(min(df3$nn), length(df3$nn))),
                fill = 'toself', type = 'scatter', mode = 'none', fillcolor = kolory_ogolny[6],  name = ".m", hoverinfo = "none") %>% 
      layout(
        font = list(family = "FuturaMedium", color = "white", size = 14),
        title = list(text = "Tworzenie Plików o Danym Rozszerzeniu", font = list(size = 22)),
        xaxis = list(title = list(text= "Czas", font = list(size = 18))),
        yaxis = list(title = list(text= "Liczba Utworzonych Plików", font = list(size = 18))),
        margin = list(t = 40),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>% 
      config(displayModeBar = FALSE,
             locale = 'pl') 
    
  })
  
  ### podsumowanie -> wykres numer 2 -------------------------------------------
  
  
  output$Podsumowanie_wykres2 <- renderPlotly({
    
    df1 <- podsumowanie_wykres2 %>% 
      filter(Imie == input$podsumowanie_2) %>% 
      filter(data >= input$data[1] & data <= input$data[2]) %>% 
      group_by(Rozszerzenie) %>% 
      summarise(n = n())
    
    df1$Procent <- (df1$n / sum(df1$n)) * 100
    
    plot_ly(
      data = df1,
      labels = ~Rozszerzenie,
      parents = ~"",
      values = ~n,
      type = "treemap",
      colors = c(kolory_ogolny[4], kolory_ogolny[5], kolory_ogolny[6]),
      marker = list(
        colors = c(kolory_ogolny[4], kolory_ogolny[5], kolory_ogolny[6]),
        line = list(width = 1, color = "white")
      ),
      hoverinfo = 'text',
      hovertext = ~paste('Rozszerzenie: ', Rozszerzenie, '<br>Procent: ', round(Procent, 2), '%'),
      textposition = "none"
    ) %>%
      layout(
        font = list(family = "FuturaMedium", color = "white", size = 20),
        title = list(text = "Jak Rozkładają Się Pliki na Naszych Komputerach?", font = list(size = 22)),
        margin = list(t = 40),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>% 
      config(displayModeBar = FALSE,
             locale = 'pl') 
  })
  
  
  
  # output$tekst_ogolny----
  output$tekst_ogolny <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 22, "px;",
      "color: ", "#48e0ab", ";",
      "background-color: rgba(0, 0, 0, 0);",
      "border: rgba(0, 0, 0, 0);",
      "padding: 10px;",
      "margin-top: 10px;",
      "text-align: justify;"
    )
    text <- "Cześć, tu Małgosia, Mikołaj i Sebastian. Witamy na stronie domowej! Tutaj znajdziesz szczegółowe informacje dotyczące tworzonych przez nas plików. Aby uzyskać bardziej precyzyjne dane, skorzystaj z suwaka po lewej stronie i wybierz interesujący Cię przedział czasowy."
    div(style = text_style, HTML(text))
  })
  # output$tekst_podsumowanie_1----
  output$tekst_podsumowanie_1 <- renderUI({

    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color: rgba(0, 0, 0, 0);",
      "border: 2px solid ", "#48e0ab", ";",
      "margin-top: 70px;",
      "padding: 10px;"
    )
    text <- "Na wykresie obserwujemy liczbę plików utworzonych przez nas z podziałem na różne rozszerzenia. Po wyborze konkretnej osoby możliwe jest prześledzenie historii tworzenia plików przez daną osobę. Wyraźnie widać, kiedy zaczęliśmy intensywnie tworzyć pliki z rozszerzeniem Matlab i Java.
    W kolejnych zakładkach dostępne są bardziej szczegółowe dane dotyczące tworzenia plików dla każdego z rozszerzeń, co pozwala na dokładniejszą analizę."
    div(style = text_style, HTML(text))
  })
  # output$tekst_podsumowanie_2----
  output$tekst_podsumowanie_2 <- renderUI({

    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color: rgba(0, 0, 0, 0);",
      "border: 2px solid ", "#48e0ab", ";",
      "margin-top: 60px;",
      "padding: 10px;"
    )
    text <- "Na wykresie porównujemy liczbę plików z różnymi rozszerzeniami, umożliwiając identyfikację dominującego formatu plików. Po najechaniu na konkretne pole wykresu uzyskujemy informację o procentowym udziale plików z danym rozszerzeniem. Zauważalne jest, że u Małgosi przeważają pliki Java, podczas gdy u Sebastiana utrzymane są najbardziej zrównoważone proporcje między różnymi formatami plików."
    div(style = text_style, HTML(text))
  })

  
  # output$tekst_word_1----
  output$tekst_word_1 <- renderUI({

    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color: ", kolor_tla, ";",
      "border: 2px solid ", kolory_word[3], ";",
      "padding: 10px;",
      "margin-top: 40px;",
      "text-align: justify;"
    )
    text <- "Pierwszy wykres prezentuje liczbę utworzonych plików przez każdą osobę z naszej grupy. Dane dotyczą liczby plików stworzonych w poszczególnych miesiącach i latach. Przy najeżdżaniu kursorem na konkretną kolumnę możliwe jest uzyskanie szczegółowych informacji na temat liczby utworzonych plików w poszczególnych dniach.
            Zauważalne jest, że Mikołaj i Sebastian regularnie korzystają z programu Word przez wiele lat. Natomiast Małgosia tworzyła pliki głownie w latach 2020 - 2021."
    div(style = text_style, HTML(text))
  })
  # output$tekst_word_2----
  output$tekst_word_2 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color: ", kolor_tla, ";",
      "border: 2px solid ", kolory_word[3], ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Na wykresie po prawej stronie dostępne są informacje dotyczące najczęściej używanych znaków interpunkcyjnych. Dane obejmują okres czasu wybrany za pomocą suwaka umieszczonego w pasku po lewej stronie. Warto zauważyć, że wśród najpopularniejszych znaków interpunkcyjnych dominują przede wszystkim kropki i przecinki. Kategoria 'pozostałe' obejmuje znaki takie jak wielokropki, średniki i cudzysłowia.
            Analizując okres od 2017 do 2023 roku, zauważamy, że u Małgosi i Mikołaja najwięcej używanych jest przecinków, natomiast u Sebastiana przeważają kropki."
    div(style = text_style, HTML(text))
  })
  # output$tekst_word_3----
  output$tekst_word_3 <- renderUI({

    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ",15, "px;",
      "color: ", "white", ";",
      "background-color: ", kolor_tla, ";",
      "border: 2px solid ", kolory_word[3], ";",
      "padding: 10px;",
      "margin-top: 40px;",
      "text-align: justify;"
    )
    text <- "Na ostatnim wykresie dokładniej przeanalizowaliśmy zależności pomiędzy najczęściej używanymi przez nas znakami interpunkcyjnymi. Każda kropka na tym wykresie reprezentuje jeden plik. Dodatkowo, rozmiar kropki odzwierciedla liczbę słów w danym pliku - im większa kropka, tym dłuższy tekst."
    div(style = text_style, HTML(text))
  })
  
  output$tekst_matlab_1 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color: ", kolor_tla, ";",
      "border: 2px solid ", "#ed9242", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Wykres po lewej dotyczy tego, jaką część plików o rozszerzeniu .m poświęcamy komentarzom. Badaliśmy
    to zjawisko, zliczając liczbę zakomentowanych wierszy w danym pliku i dzieląc ją przez łączną liczbę wierszy. Wyniki 
    ukazują znaczne różnice między nami - okazuje się, że u Sebastiana zdarzają się pliki zakomentowane
    w ponad 80 procentach, u Małgosi wartość maksymalna to 40 procent. Co ciekawe, jeżeli chodzi o średnią i medianę, są one największe u Mikołaja
    i wynoszą około 35 procent."
    div(style = text_style, HTML(text))
  })
  
  output$tekst_matlab_2 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color: ", kolor_tla, ";",
      "border: 2px solid ", "#ed9242", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Podczas analizy naszych plików przyjrzeliśmy się również temu, w jaki sposób zapisujemy matematyczne i logiczne operatory.
    Część programistów otacza takie symbole w kodzie dwiema spacjami, to znaczy zamiast 2+4 pisze 2 + 4. Inni piszą je zawsze razem lub też nie zwracają na to żadnej uwagi.
    Postanowiliśmy sprawdzić, do której kategorii zalicza się każdy z nas. W tym celu zliczyliśmy znaki dodawania, odejmowania, dzielenia i mnożenia, znaki równości, wszystkie typy znaków nierówności, a także operatory koniunkcji i alternatywy.
    Następnie wyliczyliśmy, jaka część z nich w poszczególnych plikach jest otoczona spacjami. Okazało się, że w przypadku Mikołaja i Małgosi wartości średnie są bardzo zbliżone i wynoszą 75%, u Sebastiana ta wartość jest trochę mniejsza i wynosi około 62%"
    div(style = text_style, HTML(text))
  })
  
  output$tekst_matlab_3 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color: ", kolor_tla, ";",
      "border: 2px solid ", "#ed9242", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Dobrą praktyką pisania kodu w Matlabie jest również stawianie średników na końcu linii, aby zapobiec wypisywaniu na konsolę niepotrzebnych informacji.
    Napisaliśmy specjalny algorytm pozwalający oszacować liczbę wierszy, które powinny kończyć się średnikiem. Nie zlicza on pustych czy zakomentowanych linii 
    ani tych zaczynających się od słów kluczowych if, else, for, end, function, itp. Nie uwzględniane są również wiersze zakończone trzema kropkami. Dla każdego pliku policzyliśmy, jaki procent wierszy, które powinny sie kończyć średnikiem,
    jest tak rzeczywiście zakończony. Okazuje się, że każdy z nas posiada pliki w 100% procentach zgodne z tymi zasadami, zaś mediana waha się od 75% u Małgosi do 95% u Mikołaja."
    div(style = text_style, HTML(text))
  })
  
  output$tekst_matlab_4 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color: ", kolor_tla, ";",
      "border: 2px solid ", "#ed9242", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Aby móc lepiej przyjrzeć się jakości pisanego przez nas kodu, zadaliśmy sobie również pytanie, jak dużo kodu piszemy w jednej linii,
    co ma niewątpliwy wpływ na jego czytelność. Rekomendowana maksymalna liczba znanym w jednym wierszu w Matlabie wynosi 75 znaków. Okazuje się, że
    nikt z nas nie ma tendencji do jej notorycznego przekraczania. Najwięcej znaków na linię pisze średnio Mikołaj - około 41, zaś najmniej Małgosia - około 34."
    div(style = text_style, HTML(text))
  })
  
  output$tekst_matlab_5 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color: ", kolor_tla, ";",
      "border: 2px solid ", "#ed9242", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Pierwszy wykres pozwala prześledzić liczbę plików .m tworzonych przez poszczególne osoby w danych miesiącach.
    Domyślnie wykres wygenerowany jest dla całej naszej grupy, można jednak, kilkając na legendę, wybrać słupki dotyczące poszczególnych osób.
    Jeżdżąc kursorem wzdłuż kolumny wyświetla się w dolnej części opisu informacja o liczbie utworzonych plików z podziałem na dni.
    Matlab to dość nowe dla nas środowisko obliczeniowe, z którego zaczęliśmy korzystać dopiero w październiku 2023"
    div(style = text_style, HTML(text))
  })

  
  #-----------------------------------------------------------------------------
  # Zmiana stylu
  #-----------------------------------------------------------------------------
  # styl ogólny strony----
  output$style_ogol <- renderUI({
    # Motyw Word ----
    if(input$menu=='Word')
      return(shinyDashboardThemeDIY(
        
        ### general
        appFontFamily = "FuturaMedium"
        ,appFontColor = "white"
          ,primaryFontColor = "#434C5E"
          ,infoFontColor = "#434C5E"
          ,successFontColor = "#434C5E"
          ,warningFontColor = "#434C5E"
          ,dangerFontColor = "#434C5E"
          ,bodyBackColor = kolor_tla
          
        ### header
        ,logoBackColor = "#151515" 
          
        ,headerButtonBackColor = "#151515"
          ,headerButtonIconColor = "#D8DEE9"
          ,headerButtonBackColorHover = kolor_przewodni_word[1]
          ,headerButtonIconColorHover = "#151515" 
          
        ,headerBackColor = "#151515"
          ,headerBoxShadowColor = ""
        ,headerBoxShadowSize = "0px 0px 0px"
        
        ### sidebar
        ,sidebarBackColor = cssGradientThreeColors(
          direction = "down"
          ,colorStart = "#151515"
            ,colorMiddle = "#151515"
            ,colorEnd = kolor_tla
          ,colorStartPos = 0
          ,colorMiddlePos = 50
          ,colorEndPos = 100
        )
          ,sidebarPadding = 0
        
        ,sidebarMenuBackColor = "transparent"
        ,sidebarMenuPadding = 5
        ,sidebarMenuBorderRadius = 5
        
        ,sidebarShadowRadius = "" 
        ,sidebarShadowColor = "0px 0px 0px"
        
        ,sidebarUserTextColor = "#D8DEE9"
          
        ,sidebarSearchBackColor = "#4C566A"
          ,sidebarSearchIconColor = "#151515"
          ,sidebarSearchBorderColor = "#4C566A"
          
        ,sidebarTabTextColor = "#ECEFF4"
          ,sidebarTabTextSize = 14
        ,sidebarTabBorderStyle = "none"
        ,sidebarTabBorderColor = "#000000"
          ,sidebarTabBorderWidth = 0
        
        ,sidebarTabBackColorSelected = kolor_przewodni_word[1]
          ,sidebarTabTextColorSelected = "#000000" 
          ,sidebarTabRadiusSelected = "20px" 
        
        ,sidebarTabBackColorHover = kolor_przewodni_word[1]
          ,sidebarTabTextColorHover = "#000000"
          ,sidebarTabBorderStyleHover = "none"
        ,sidebarTabBorderColorHover = "none"
        ,sidebarTabBorderWidthHover = 0
        ,sidebarTabRadiusHover = "20px"
        
        ### boxes
        ,boxBackColor = kolor_tla
          ,boxBorderRadius = 5
        ,boxShadowSize = "0px 0px 0px"
        ,boxShadowColor = ""
        ,boxTitleSize = 18
        ,boxDefaultColor = kolor_tla
          ,boxPrimaryColor = kolor_tla
          ,boxInfoColor = kolor_tla
          ,boxSuccessColor = kolor_tla
          ,boxWarningColor = kolor_tla
          ,boxDangerColor = kolor_tla
          
        ,tabBoxTabColor = "#151515"
          ,tabBoxTabTextSize = 16
        ,tabBoxTabTextColor = "#151515"
          ,tabBoxTabTextColorSelected = "#151515"
          ,tabBoxBackColor = "#BF616A"
          ,tabBoxHighlightColor = "#4C566A"
          ,tabBoxBorderRadius = 5 
        
        ### inputs
        ,buttonBackColor = "#151515"
          ,buttonTextColor = "#2E3440"
          ,buttonBorderColor = "#2E3440"
          ,buttonBorderRadius = 5
        
        ,buttonBackColorHover = "#151515"
          ,buttonTextColorHover = kolor_tla
          ,buttonBorderColorHover = "#2E3440"
          
        ,textboxBackColor = "#151515" 
          ,textboxBorderColor = kolor_przewodni_word[1]
          ,textboxBorderRadius = 5
        ,textboxBackColorSelect = "#151515"
          ,textboxBorderColorSelect = kolor_przewodni_word[2]
          
        ### tables
        ,tableBackColor = "#151515"
          ,tableBorderColor = "#2E3440"
          ,tableBorderTopSize = 1
        ,tableBorderRowSize = 1
      ))
    # Motyw Java ----
    if(input$menu=='Java')
      return(shinyDashboardThemeDIY(
        
        ### general
        appFontFamily = "FuturaMedium"
        ,appFontColor = "white"
          ,primaryFontColor = "#434C5E"
          ,infoFontColor = "#434C5E"
          ,successFontColor = "#434C5E"
          ,warningFontColor = "#434C5E"
          ,dangerFontColor = "#434C5E"
          ,bodyBackColor = kolor_tla 
          
        ### header
        ,logoBackColor = "#151515" 
          
        ,headerButtonBackColor = "#151515"
          ,headerButtonIconColor = "#D8DEE9"
          ,headerButtonBackColorHover = kolor_przewodni_java[1]
          ,headerButtonIconColorHover = "#151515" 
          
        ,headerBackColor = "#151515"
          ,headerBoxShadowColor = ""
        ,headerBoxShadowSize = "0px 0px 0px"
        
        ### sidebar
        ,sidebarBackColor = cssGradientThreeColors(
          direction = "down"
          ,colorStart = "#151515"
            ,colorMiddle = "#151515"
            ,colorEnd = kolor_tla
          ,colorStartPos = 0
          ,colorMiddlePos = 50
          ,colorEndPos = 100
        )
          ,sidebarPadding = 0
        
        ,sidebarMenuBackColor = "transparent"
        ,sidebarMenuPadding = 5
        ,sidebarMenuBorderRadius = 5
        
        ,sidebarShadowRadius = "" 
        ,sidebarShadowColor = "0px 0px 0px"
        
        ,sidebarUserTextColor = "#D8DEE9"
          
        ,sidebarSearchBackColor = "#4C566A"
          ,sidebarSearchIconColor = "#151515"
          ,sidebarSearchBorderColor = "#4C566A"
          
        ,sidebarTabTextColor = "#ECEFF4"
          ,sidebarTabTextSize = 14
        ,sidebarTabBorderStyle = "none"
        ,sidebarTabBorderColor = "#000000"
          ,sidebarTabBorderWidth = 0
        
        ,sidebarTabBackColorSelected = kolor_przewodni_java[1]
          ,sidebarTabTextColorSelected = "#000000" 
          ,sidebarTabRadiusSelected = "20px" 
        
        ,sidebarTabBackColorHover = kolor_przewodni_java[1]
          ,sidebarTabTextColorHover = "#000000"
          ,sidebarTabBorderStyleHover = "none"
        ,sidebarTabBorderColorHover = "none"
        ,sidebarTabBorderWidthHover = 0
        ,sidebarTabRadiusHover = "20px"
        
        ### boxes
        ,boxBackColor = kolor_tla
        ,boxBorderRadius = 5
        ,boxShadowSize = "0px 0px 0px"
        ,boxShadowColor = ""
        ,boxTitleSize = 18
        ,boxDefaultColor = kolor_tla
        ,boxPrimaryColor = kolor_tla
        ,boxInfoColor = kolor_tla
        ,boxSuccessColor = kolor_tla
        ,boxWarningColor = kolor_tla
        ,boxDangerColor = kolor_tla
          
        ,tabBoxTabColor = "#151515"
          ,tabBoxTabTextSize = 16
        ,tabBoxTabTextColor = "#151515"
          ,tabBoxTabTextColorSelected = "#151515"
          ,tabBoxBackColor = "#BF616A"
          ,tabBoxHighlightColor = "#4C566A"
          ,tabBoxBorderRadius = 5 
        
        ### inputs
        ,buttonBackColor = "#151515"
          ,buttonTextColor = "#2E3440"
          ,buttonBorderColor = "#2E3440"
          ,buttonBorderRadius = 5
        
        ,buttonBackColorHover = "#151515"
          ,buttonTextColorHover = kolor_tla
          ,buttonBorderColorHover = "#2E3440"
          
        ,textboxBackColor = "#151515" 
          ,textboxBorderColor = kolor_przewodni_java[1]
          ,textboxBorderRadius = 5
        ,textboxBackColorSelect = "#151515"
          ,textboxBorderColorSelect = kolor_przewodni_java[2]
          
        ### tables
        ,tableBackColor = "#151515"
          ,tableBorderColor = "#2E3440"
          ,tableBorderTopSize = 1
        ,tableBorderRowSize = 1
      ))
    # Motyw MATLAB ----
    if(input$menu=='MATLAB')
      return(shinyDashboardThemeDIY(
        
        ### general
        appFontFamily = "FuturaMedium"
        ,appFontColor = "white"
          ,primaryFontColor = "#434C5E"
          ,infoFontColor = "#434C5E"
          ,successFontColor = "#434C5E"
          ,warningFontColor = "#434C5E"
          ,dangerFontColor = "#434C5E"
          ,bodyBackColor = kolor_tla
          
        ### header
        ,logoBackColor = "#151515" 
          
        ,headerButtonBackColor = "#151515"
          ,headerButtonIconColor = "#D8DEE9"
          ,headerButtonBackColorHover = kolor_przewodni_matlab[1]
          ,headerButtonIconColorHover = "#151515" 
          
        ,headerBackColor = "#151515"
          ,headerBoxShadowColor = ""
        ,headerBoxShadowSize = "0px 0px 0px"
        
        ### sidebar
        ,sidebarBackColor = cssGradientThreeColors(
          direction = "down"
          ,colorStart = "#151515"
            ,colorMiddle = "#151515"
            ,colorEnd = kolor_tla
          ,colorStartPos = 0
          ,colorMiddlePos = 50
          ,colorEndPos = 100
        )
          ,sidebarPadding = 0
        
        ,sidebarMenuBackColor = "transparent"
        ,sidebarMenuPadding = 5
        ,sidebarMenuBorderRadius = 5
        
        ,sidebarShadowRadius = "" 
        ,sidebarShadowColor = "0px 0px 0px"
        
        ,sidebarUserTextColor = "#D8DEE9"
          
        ,sidebarSearchBackColor = "#4C566A"
          ,sidebarSearchIconColor = "#151515"
          ,sidebarSearchBorderColor = "#4C566A"
          
        ,sidebarTabTextColor = "#ECEFF4"
          ,sidebarTabTextSize = 14
        ,sidebarTabBorderStyle = "none"
        ,sidebarTabBorderColor = "#000000"
          ,sidebarTabBorderWidth = 0
        
        ,sidebarTabBackColorSelected = kolor_przewodni_matlab[1]
          ,sidebarTabTextColorSelected = "#000000" 
          ,sidebarTabRadiusSelected = "20px" 
        
        ,sidebarTabBackColorHover = kolor_przewodni_matlab[1]
          ,sidebarTabTextColorHover = "#000000"
          ,sidebarTabBorderStyleHover = "none"
        ,sidebarTabBorderColorHover = "none"
        ,sidebarTabBorderWidthHover = 0
        ,sidebarTabRadiusHover = "20px"
        
        ### boxes
        ,boxBackColor = kolor_tla
        ,boxBorderRadius = 5
        ,boxShadowSize = "0px 0px 0px"
        ,boxShadowColor = ""
        ,boxTitleSize = 18
        ,boxDefaultColor = kolor_tla
        ,boxPrimaryColor = kolor_tla
        ,boxInfoColor = kolor_tla
        ,boxSuccessColor = kolor_tla
        ,boxWarningColor = kolor_tla
        ,boxDangerColor = kolor_tla
          
        ,tabBoxTabColor = "#151515"
          ,tabBoxTabTextSize = 16
        ,tabBoxTabTextColor = "#151515"
          ,tabBoxTabTextColorSelected = "#151515"
          ,tabBoxBackColor = "#BF616A"
          ,tabBoxHighlightColor = "#4C566A"
          ,tabBoxBorderRadius = 5 
        
        ### inputs
        ,buttonBackColor = "#151515"
          ,buttonTextColor = "#2E3440"
          ,buttonBorderColor = "#2E3440"
          ,buttonBorderRadius = 5
        
        ,buttonBackColorHover = "#151515"
          ,buttonTextColorHover = kolor_tla
          ,buttonBorderColorHover = "#2E3440"
          
        ,textboxBackColor = "#151515" 
          ,textboxBorderColor = kolor_przewodni_matlab[1] 
          ,textboxBorderRadius = 5
        ,textboxBackColorSelect = "#151515"
          ,textboxBorderColorSelect = kolor_przewodni_matlab[2]
          
        ### tables
        ,tableBackColor = "#151515"
          ,tableBorderColor = "#2E3440"
          ,tableBorderTopSize = 1
        ,tableBorderRowSize = 1
      ))
    # Motyw domyślny ----
    return(theme_default)
  })
  
  # styl wybranych komponentów----
  output$style_css <- renderUI({
    # word page style----
    if(input$menu=='Word')
      return(tags$style(HTML("
      
    /* sliders */
    .js-irs-0 {
    max-width: 215px;
    } 
    .irs--shiny .irs-bar {
    border-top-color: #1B5EBE;
    border-bottom-color: #1B5EBE;
    } 
    .irs--shiny .irs-bar-edge {
    border-color: #1B5EBE;
    }
    .irs--shiny .irs-single, .irs--shiny .irs-bar-edge, .irs--shiny .irs-bar {
    background: #41A5EE;
    }
    .irs--shiny .irs-handle {
    border: 1px solid #1B5EBE;
    background-color: #1B5EBE;
    }
    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
    background: #41A5EE;
    }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #000000;
    background-color: #1B5EBE;
    }
    
    /* dropdown menus */
    .selectize-dropdown .selected {
    background-color: #1B5EBE;
    color: #000000;
    }
    .selectize-dropdown .active:not(.selected) {
    background: #41A5EE;
    color: #000000;
    }
    .selectize-input, .selectize-control.single .selectize-input.input-active {
    background: #000000;
    color: #ffffff
    }
    .selectize-dropdown [data-selectable] .highlight {
    background: rgba(235, 64, 52, 0.4);
    border-radius: 1px;
    }
    .selectize-control.multi .selectize-input>div {
    cursor: pointer;
    background: #1B5EBE;
    color: #ffffff;
    }
    selectize-dropdown .create {
    color: #ffffff;
    }
    .form-control, .selectize-input, .selectize-control.single .selectize-input {
    background: #000000;
    color: #ffffff;
    border-color: #1B5EBE;
    }
    .nwm-czemu-to-dziala .selectize-input, .selectize-control.single .selectize-input.input-active {
    border-color: #41A5EE;
    background: #151515;
    }
    .selectize-control.multi .selectize-input>div.active {
    background: #41A5EE;
    }
    .selectize-dropdown, .selectize-input, .selectize-input input {
    color: #ffffff;
    }
    
    /* fixed sidebar and header */
    .sidebar {
    position: fixed;
    width: 250px;
    white-space: nowrap;
    overflow: visible;
    }
    .main-header {
    position: fixed;
    width:100%;
    }
    .shiny-output-error { visibility: hidden; },
    .shiny-output-error:before { visibility: hidden; }
                           ")))
    # java page style----
    if(input$menu=='Java')
      return(tags$style(HTML("
      
    /* sliders */
    .js-irs-0 {
    max-width: 215px;
    } 
    .irs--shiny .irs-bar {
    border-top-color: #fc0703;
    border-bottom-color: #fc0703;
    } 
    .irs--shiny .irs-bar-edge {
    border-color: #fc0703;
    }
    .irs--shiny .irs-single, .irs--shiny .irs-bar-edge, .irs--shiny .irs-bar {
    background: #DD4B39;
    }
    .irs--shiny .irs-handle {
    border: 1px solid #fc0703;
    background-color: #fc0703;
    }
    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
    background: #DD4B39;
    }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #000000;
    background-color: #fc0703;
    }
    
    /* dropdown menus */
    .selectize-dropdown .selected {
    background-color: #fc0703;
    color: #000000;
    }
    .selectize-dropdown .active:not(.selected) {
    background: #03a1fc;
    color: #000000;
    }
    .selectize-input, .selectize-control.single .selectize-input.input-active {
    background: #000000;
    color: #ffffff
    }
    .selectize-dropdown [data-selectable] .highlight {
    background: rgba(235, 64, 52, 0.4);
    border-radius: 1px;
    }
    .selectize-control.multi .selectize-input>div {
    cursor: pointer;
    background: #DD4B39;
    color: #ffffff;
    }
    selectize-dropdown .create {
    color: #ffffff;
    }
    .form-control, .selectize-input, .selectize-control.single .selectize-input {
    background: #000000;
    color: #ffffff;
    border-color: #fc0703;
    }
    .nwm-czemu-to-dziala .selectize-input, .selectize-control.single .selectize-input.input-active {
    border-color: #03a1fc;
    background: #151515;
    }
    .selectize-control.multi .selectize-input>div.active {
    color: #000000;
    background: #03a1fc;
    }
    
    /* fixed sidebar and header */
    .sidebar {
    position: fixed;
    width: 250px;
    white-space: nowrap;
    overflow: visible;
    }
    .main-header {
    position: fixed;
    width:100%;
    }
    .shiny-output-error { visibility: hidden; },
    .shiny-output-error:before { visibility: hidden; }
                           ")))
    # matlab page style----
    if(input$menu=='MATLAB')
      return(tags$style(HTML("
      
    /* sliders */
    .js-irs-0 {
    max-width: 215px;
    } 
    .irs--shiny .irs-bar {
    border-top-color: #ed9242;
    border-bottom-color: #ed9242;
    } 
    .irs--shiny .irs-bar-edge {
    border-color: #ed9242;
    }
    .irs--shiny .irs-single, .irs--shiny .irs-bar-edge, .irs--shiny .irs-bar {
    background: #fcf647;
    }
    .irs--shiny .irs-handle {
    border: 1px solid #ed9242;
    background-color: #ed9242;
    }
    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
    background: #fcf647;
    }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #000000;
    background-color: #ed9242;
    }
    
    /* dropdown menus */
    .selectize-dropdown .selected {
    background-color: #ed9242;
    color: #000000;
    }
    .selectize-dropdown .active:not(.selected) {
    background: #fcf647;
    color: #000000;
    }
    .selectize-input, .selectize-control.single .selectize-input.input-active {
    background: #000000;
    color: #ffffff
    }
    .selectize-dropdown [data-selectable] .highlight {
    background: rgba(235, 64, 52, 0.4);
    border-radius: 1px;
    }
    .selectize-control.multi .selectize-input>div {
    cursor: pointer;
    background: #ed9242;
    color: #ffffff;
    }
    selectize-dropdown .create {
    color: #ffffff;
    }
    .form-control, .selectize-input, .selectize-control.single .selectize-input {
    background: #000000;
    color: #ffffff;
    border-color: #ed9242;
    }
    .nwm-czemu-to-dziala .selectize-input, .selectize-control.single .selectize-input.input-active {
    border-color: #fcf647;
    background: #151515;
    }
    .selectize-control.multi .selectize-input>div.active {
    color: #000000;
    background: #fcf647;
    }
    .selectize-dropdown, .selectize-input, .selectize-input input {
    color: #ffffff;
    }
    
    /* fixed sidebar and header */
    .sidebar {
    position: fixed;
    width: 250px;
    white-space: nowrap;
    overflow: visible;
    }
    .main-header {
    position: fixed;
    width:100%;
    }
    .shiny-output-error { visibility: hidden; },
    .shiny-output-error:before { visibility: hidden; }
                           ")))
    # home page style----
    return(tags$style(HTML("

    /* sliders */
    .js-irs-0 {
    max-width: 215px;
    }
    .irs--shiny .irs-bar {
    border-top-color: #48e0ab;
    border-bottom-color: #48e0ab;
    }
    .irs--shiny .irs-bar-edge {
    border-color: #48e0ab;
    }
    .irs--shiny .irs-single, .irs--shiny .irs-bar-edge, .irs--shiny .irs-bar {
    background: #47fcf6;
    }
    .irs--shiny .irs-handle {
    border: 1px solid #48e0ab;
    background-color: #48e0ab;
    }
    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
    background: #47fcf6;
    }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #000000;
    background-color: #48e0ab;
    }

    /* dropdown menus */
    .selectize-dropdown .selected {
    background-color: #48e0ab;
    color: #000000;
    }
    .selectize-dropdown .active:not(.selected) {
    background: #47fcf6;
    color: #000000;
    }
    .selectize-input, .selectize-control.single .selectize-input.input-active {
    background: #000000;
    color: #ffffff
    }
    .selectize-dropdown [data-selectable] .highlight {
    background: rgba(235, 64, 52, 0.4);
    border-radius: 1px;
    }
    .selectize-control.multi .selectize-input>div {
    cursor: pointer;
    background: #48e0ab;
    color: #ffffff;
    }
    selectize-dropdown .create {
    color: #ffffff;
    }
    .form-control, .selectize-input, .selectize-control.single .selectize-input {
    background: #000000;
    color: #ffffff;
    border-color: #48e0ab;
    }
    .nwm-czemu-to-dziala .selectize-input, .selectize-control.single .selectize-input.input-active {
    border-color: #47fcf6;
    background: #151515;
    }
    .selectize-control.multi .selectize-input>div.active {
    background: #47fcf6;
    }
    .selectize-dropdown, .selectize-input, .selectize-input input {
    color: #ffffff;
    }

    /* fixed sidebar and header */
    .sidebar {
    position: fixed;
    width: 250px;
    white-space: nowrap;
    overflow: visible;
    }
    .main-header {
    position: fixed;
    width:100%;
    }
    .shiny-output-error { visibility: hidden; },
    .shiny-output-error:before { visibility: hidden; }
                           ")))
  })
}

################################################################################
# Tworzenie UI
################################################################################
app_ui <- dashboardPage(
  
  #-----------------------------------------------------------------------------
  # Panel zarządzania
  #-----------------------------------------------------------------------------
  dashboardHeader(
    title = "MyFiles",
    titleWidth = 250
    ),
  dashboardSidebar(
    sidebarMenu(uiOutput('style_ogol'),
                uiOutput('style_css'),
                id = "menu", sidebarMenuOutput("menu"),
                menuItem("Strona domowa", tabName = "Ogólny",
                         icon = icon("home")),
                menuItem("Java", tabName = "Java",
                         icon = icon("java")),
                menuItem("Word", tabName = "Word",
                         icon = icon("file-word")),
                menuItem("MATLAB", tabName = "MATLAB",
                         icon = icon("calculator"))
    ),
    sliderInput(
      inputId = "data",
      label = "Ustaw przedział czasu",
      min = min(as.Date(word$Data.utworzenia.pliku)),
      max = as.Date("2024-01-31"),
      value = c(as.Date(min(as.Date("2021-01-01"))), as.Date("2024-01-31"))
    ),
    width = 250
  ),
  #-----------------------------------------------------------------------------
  # Koniec panelu zarządzania
  #-----------------------------------------------------------------------------
  #style = "margin-bottom: 80px; margin-left: 20px; margin-top: 80x;"
  dashboardBody(
    theme_default,
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabItems(
      
      #-------------------------------------------------------------------------
      # Panel ogólny
      #-------------------------------------------------------------------------
      tabItem(
        tabName = "Ogólny",
        fluidRow(
          box(
            title = tags$p(icon("home"),"MyFiles",
                           style = "font-size: 45px; margin-top: 60px; margin-left: 500px; text-align: center;"),
            width = 12,  # Dostosuj szerokość pudełka według potrzeb
            solidHeader = TRUE,
          )
        ),
        fluidRow(
          column(width = 12,
                 wellPanel(
                   style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
                   withSpinner(uiOutput("tekst_ogolny"))
                 ))
        ),
        fluidRow(column(width = 12, style = "margin-left: 400px; margin-top: 20px; text-align: center",
                        selectInput(
                          inputId = "podsumowanie_11",
                          label = "Wybierz osobę",
                          choices = c("Mikołaj", "Małgosia", "Sebastian"),
                          selected = zmienne,
                          multiple = FALSE
                        ))),
        fluidRow(style = "margin-left: 10px; margin-top: 40px",
          column(width = 8,style="text-align: center",
                 
                 withSpinner(plotlyOutput("Podsumowanie_wykres1"))
                 ),
          column(width = 4,
                 wellPanel(
                   style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
                   withSpinner(uiOutput("tekst_podsumowanie_1"))
                 ))
        ),
        fluidRow(column(width = 12, style = "margin-left: 400px; margin-top: 20px; text-align: center",
                        selectInput(
                          inputId = "podsumowanie_2",
                          label = "Wybierz osobę",
                          choices = c("Mikołaj", "Małgosia", "Sebastian"),
                          selected = zmienne,
                          multiple = FALSE
                        ))),
        fluidRow(style = "margin-left: -10px",
          column(width = 8, style="text-align: center",
                 withSpinner(plotlyOutput("Podsumowanie_wykres2"))
                 ),
          column(width = 4,
                 wellPanel(
                   style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
                   withSpinner(uiOutput("tekst_podsumowanie_2"))
                 ))
          )
      ),
      #-------------------------------------------------------------------------
      # Koniec panelu ogólnego
      #-------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------
      # Panel Java
      #-------------------------------------------------------------------------
      tabItem(
        tabName = "Java",
        fluidRow(
          box(
            title = tags$p(icon("java"),"Java",
                           style = "font-size: 45px; margin-top: 60px; margin-left: 500px; text-align: center;"),
            width = 12,  # Dostosuj szerokość pudełka według potrzeb
            solidHeader = TRUE,
          )
        ),
        fluidRow(
          style = "margin-bottom: 80px;",
          column(width = 9,
                 withSpinner(plotlyOutput("JavaWykres1"))
          ),
          column(width = 3,
                 wellPanel(
                   style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
                   withSpinner(uiOutput("JavaWykres10"))
                   )
                 )
        ),
        fluidRow(
          style = "margin-bottom: 80px;",
          column(width = 6,
                 withSpinner(plotlyOutput("JavaWykres2"))
          ),
          column(width = 6,
                 withSpinner(plotlyOutput("JavaWykres3"))
          )
        ),
        fluidRow(
          style = "margin-bottom: 80px;",
          column(width = 6,
                 box(
                   title = tags$p("Najdłuższy wyraz:",
                                  style = "font-size: 125%; text-align: left;color: #FFFFFF;"),
                   withSpinner(htmlOutput("JavaWykres6")),
                   width = 12
                 )
          ),
          column(width = 6,
                 box(
                   title = tags$p("Ile wyrazów charakterystycznych dla Javy średnio zostało napisanych na plik:",
                                  style = "font-size: 125%; text-align: left;color: #FFFFFF;"),
                   selectizeInput("txtIn", "Wpisz wyraz charakterystyczny dla Javy",
                                  choices = java_keyword_list,
                                  selected = "protected",
                                  multiple = F, 
                                  options = list(create=F,
                                                 placeholder = 'Wpisz wyraz',
                                                 plugins = list('restore_on_backspace'))
                   ),
                   withSpinner(htmlOutput("JavaWykres7")),
                   width = 12
                 )
          )
        ),
        fluidRow(
          style = "margin-bottom: 80px;",
          column(width = 3,
                 selectizeInput(
                   inputId = "JavaSelectInput1",
                   label = "Wybierz osoby, do których dane będą się odnosiły",
                   choices = c("Mikołaj", "Małgosia", "Sebastian"),
                   selected = c("Mikołaj", "Małgosia", "Sebastian"),
                   multiple = TRUE,
                   options = list(create=F,
                                  placeholder = 'Wybierz imiona',
                                  plugins= list('remove_button')
                   )
                 )
          ),
          column(width = 4.5,
                 withSpinner(valueBoxOutput('JavaWykres8'))
          ),
          column(width = 4.5,
                 withSpinner(valueBoxOutput('JavaWykres9'))
          )
        )
      ),
      #-------------------------------------------------------------------------
      # Koniec panelu Java
      #-------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------
      # Panel Word
      #-------------------------------------------------------------------------
      tabItem(
        tabName = "Word",
        fluidRow(
          box(
            title = tags$p(icon("file-word"),"Word",
                           style = "font-size: 45px; margin-top: 60px; margin-left: 500px; text-align: center;"),
            width = 12,  # Dostosuj szerokość pudełka według potrzeb
            solidHeader = TRUE,
          )
        ),
        fluidRow(
          column(style = "margin-bottom: 80px",
            width = 8,
            withSpinner(plotlyOutput("wykres1_1"))
          ),
          column(
            width = 4,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              withSpinner(uiOutput("tekst_word_1"))
            )
          )
          
        ),
        fluidRow(
          style = "margin-bottom: 40px; margin-top: 40px; margin-left: 10px",
          column(
            width = 4,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              withSpinner(uiOutput("tekst_word_2"))
            )
          ),
          column(
            width = 8,  style="text-align: center",
            selectInput("zmienna",
                        "Wybierz osobę",
                        zmienne),
            withSpinner(plotlyOutput("wykres2", height = "500px"))
          )
          
        ),
        fluidRow(
          style = "margin-bottom: 40px; margin-top: 40px; margin-left: 20px",
          column(
            width = 8,
            withSpinner(plotlyOutput("wykres3"))
          ),
          column(
            width = 4,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              withSpinner(uiOutput("tekst_word_3"))
            )
          )
          
        )
      ),
      #-------------------------------------------------------------------------
      # Koniec panelu Word
      #-------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------
      # Panel MATLAB
      #-------------------------------------------------------------------------
      tabItem(
      tabName = "MATLAB",
      fluidRow(
        box(
          title = tags$p(icon("calculator"),"Matlab",
                         style = "font-size: 45px; margin-top: 60px; margin-left: 480px; text-align: center;"),
          width = 12,  # Dostosuj szerokość pudełka według potrzeb
          solidHeader = TRUE,
        )
      ),
      fluidRow(column(width = 12, style = "margin-left: 400px; text-align: center",
                      selectInput(
                        
                        inputId = "matlab5",
                        label = "Wybierz osobę",
                        choices = c("Mikołaj", "Małgosia", "Sebastian"),
                        selected = c("Mikołaj", "Małgosia", "Sebastian"),
                        multiple = TRUE
                      ))),
      fluidRow( 
        column(
          width = 5,
          wellPanel(
            style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
            withSpinner(uiOutput("tekst_matlab_5"))
          )
        ),
            column(width = 7,
                    style = "text-align: center; align: center;",
            withSpinner(plotlyOutput("MATLABWykres5"))
        ))
      ,
      fluidRow(column(width = 12, style = "margin-left: 400px; margin-top: 20px; text-align: center",
                      selectInput(
                        inputId = "matlab4",
                        label = "Wybierz osobę",
                        choices = c("Mikołaj", "Małgosia", "Sebastian"),
                        selected = c("Mikołaj", "Małgosia", "Sebastian"),
                        multiple = TRUE
                      ))),
      fluidRow(style = "margin-left: 10px",
        column(
          width = 7,
          withSpinner(plotlyOutput("MATLABWykres4"))
        ),
        column(
          width = 5,
          wellPanel(
            style = " background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
            style = "margin-top: -50px;",
            withSpinner(uiOutput("tekst_matlab_1"))
          )
        )),
      fluidRow(column(width = 12, style = "margin-left: 400px; margin-top: 20px; text-align: center",
                      selectInput(
                        inputId = "matlab1",
                        label = "Wybierz osobę",
                        choices = c("Mikołaj", "Małgosia", "Sebastian"),
                        selected = c("Mikołaj", "Małgosia", "Sebastian"),
                        multiple = TRUE
                      ))),
        fluidRow( 
          column(
            width = 5,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              style = "margin-top: -70px",
              withSpinner(uiOutput("tekst_matlab_2"))
            )
          ),
          column(
              width = 7,
              style = "text-align: center; align: center;",
                 withSpinner(plotlyOutput("MATLABWykres1"))
          )),
      fluidRow(column(width = 12, style = "margin-left: 400px; margin-top: 20px; text-align: center",
                      selectInput(
                        inputId = "matlab2",
                        label = "Wybierz osobę",
                        choices = c("Mikołaj", "Małgosia", "Sebastian"),
                        selected = c("Mikołaj", "Małgosia", "Sebastian"),
                        multiple = TRUE
                      ))),
      fluidRow(
        style = "margin-bottom: 80px; margin-left: 10px",
        column(width = 7,
               style = "text-align: center; align: center;",
               withSpinner(plotlyOutput("MATLABWykres2")),
        ),
        column(
          width = 5,
          wellPanel(
            style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
            style = "margin-top: -70px",
            withSpinner(uiOutput("tekst_matlab_3"))
          )
        )
      ),
        fluidRow(
          column(
            width = 5,
            style = "margin-top: -50px",
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              withSpinner(uiOutput("tekst_matlab_4"))
            )
          ),
          column(width = 7,
              withSpinner(plotlyOutput("MATLABWykres3"))
          )
        )

      )
      #-------------------------------------------------------------------------
      # Koniec panelu MATLAB
      #-------------------------------------------------------------------------
      
    )
  )
)

shinyApp(app_ui, server)

