# setwd("C:\\Users\\basiu\\OneDrive\\Pulpit\\sem 3\\techniki wizualizacji danych\\projekt 2")

setwd('~/Desktop/pw/3 sem/techniki_wizualizacji_danych/projekt_2/')

df <- readxl::read_xlsx("dane.xlsx")
library(dplyr)
library(plotly)
library(stringr)
library(shiny)
library(shinyjqui)
library(ggplot2)

library(tidyverse)
# install.packages("shinyWidgets")
# install.packages("shinydashboard")
library(shinydashboard)
library(lubridate)

library(shinyWidgets)

tlo <- "#8EC7DD"
  fillB <- "#653AB0"
    fillM <- "orange3"
      
    df %>% mutate(Godzina_basia_spanie = as.numeric(word(`spanie BASIA`, 1)),
                  Godzina_michal_spanie = as.numeric(word(`spanie MICHAL`, 1)),
                  Minuta_basia_spanie = as.numeric(word(`spanie BASIA`,2)),
                  Minuta_michal_spanie = as.numeric(word(`spanie MICHAL`,2)),
                  Godzina_basia_wstanie = as.numeric(word(`wstanie BASIA`, 1)),
                  Godzina_michal_wstanie = as.numeric(word(`wstanie MICHAL`, 1)),
                  Minuta_basia_wstanie = as.numeric(word(`wstanie BASIA`,2)),
                  Minuta_michal_wstanie = as.numeric(word(`wstanie MICHAL`,2)),
                  dzien = as.numeric(word(data, 1)),
                  miesiac = word(data,2),
                  miesiac = ifelse(miesiac == "grudnia", 12,1),
                  rok = ifelse(miesiac == 12, 2023, 2024))%>% 
      select(-c(2:5)) -> df1
    
    df1 %>% mutate(miesiac = ifelse(miesiac >= 10, miesiac, paste("0", miesiac, sep = "")),
                   dzien = ifelse(dzien >= 10, dzien, paste("0", dzien, sep="")),
                   d = as.Date(paste(dzien,miesiac, rok, sep = "-"),"%d-%m-%Y")) -> df1
    
    df1 %>% select(d, Godzina_basia_spanie, Godzina_michal_spanie)%>%
      pivot_longer(-d, names_to = "osoba", values_to = "godzina_spanie")  %>%
      mutate(osoba = substr(osoba, start = 9, stop = 9))-> a
    df1 %>% select(d, Minuta_basia_spanie, Minuta_michal_spanie) %>%
      pivot_longer(-d, names_to = "osoba", values_to = "minuta_spanie") %>%
      mutate(osoba = substr(osoba, start = 8, stop = 8))-> b
    df1 %>% select(d, Godzina_basia_wstanie, Godzina_michal_wstanie) %>%
      pivot_longer(-d, names_to = "osoba", values_to = "godzina_wstanie") %>%
      mutate(osoba = substr(osoba, start = 9, stop = 9))-> c
    df1 %>% select(d, Minuta_basia_wstanie, Minuta_michal_wstanie) %>%
      pivot_longer(-d, names_to = "osoba", values_to = "minuta_wstanie") %>%
      mutate(osoba = substr(osoba, start = 8, stop = 8))-> d
    inner_join(a,b) -> df_merged
    inner_join(df_merged,c) -> df_merged
    inner_join(df_merged,d) -> df_merged
    
    # df_m <- df_merged %>%
    #   filter(osoba == "m")
    # df_b <- df_merged %>%
    #   filter(osoba == "b")
    
    df_spanko <- df_merged
    
    
    df_spanko$minuta_wstanie <- c(tail(df_merged$minuta_wstanie, -2), NA, NA)
    df_spanko$godzina_wstanie <- c(tail(df_merged$godzina_wstanie, -2), NA, NA)
    
    df_spanko <- head(df_spanko, -2)
    
    
    oblicz_roznice_czasu <- function(godzina1, minuta1, godzina2, minuta2) {
      czas1_w_minutach <- godzina1 * 60 + minuta1 
      czas2_w_minutach <- godzina2 * 60 + minuta2 
      roznica_w_minutach <- czas2_w_minutach - czas1_w_minutach  
      return(roznica_w_minutach)
    }
    
    df_merged %>% 
      mutate(czas = oblicz_roznice_czasu(godzina_wstanie, minuta_wstanie,
                                         godzina_spanie, minuta_spanie)) %>% 
      mutate(czas = ifelse(czas <= 0, 1440 + czas, czas),
             pocz = godzina_wstanie*60 + minuta_wstanie) -> df_merged
    
    df_spanko %>% 
      mutate(czas = oblicz_roznice_czasu(godzina_spanie, minuta_spanie, 
                                         godzina_wstanie, minuta_wstanie)) %>% 
      mutate(czas = ifelse(czas <= 0, 1440 + czas, czas),
             pocz = ifelse((godzina_spanie*60 + minuta_spanie) < 360,
                           (godzina_spanie*60 + minuta_spanie) + 1440,
                           (godzina_spanie*60 + minuta_spanie))) -> df_spanko
    
    df1 %>% 
      select(d, `kroki BASIA`, `kroki MICHAL`, `instagram BASIA`, `instagram MICHAL`) -> df3
    
    
    df1 %>% 
      select(d, `kroki BASIA`, `kroki MICHAL`) %>% 
      pivot_longer(-d, names_to = "osoba", values_to = "kroki") %>% 
      mutate(osoba = case_when(substr(osoba, start = 7, stop = 7) == "B" ~ "b",
                               substr(osoba, start = 7, stop = 7) == "M" ~ "m")) -> aa
    
    df1 %>% 
      select(d, `instagram BASIA`, `instagram MICHAL`) %>% 
      pivot_longer(-d, names_to = "osoba", values_to = "instagram") %>% 
      mutate(osoba = case_when(substr(osoba, start = 11, stop = 11) == "B" ~ "b",
                               substr(osoba, start = 11, stop = 11) == "M" ~ "m")) -> bb
    
    df_merged %>% 
      select(d, osoba, czas, pocz) -> cc
    
    df3 <- inner_join(aa, bb)
    
    df3 <- inner_join(df3, cc)
    
    zmienne_3 <- c(
      "Liczba kroków",
      "Minuty na instagramie",
      "Godzina wstania",
      "Czas na nogach")
    
    breaks_czas <- seq(480, 1200, by = 120)
    labels_czas <- c("8", "10", "12", "14", "16", "18", "20")
    limits_czas <- c(480, 1200)
    
    breaks_kroki <- seq(0, 30000, by = 2500)
    labels_kroki <- seq(0, 30000, by = 2500)
    limits_kroki <- c(0, 30000)
    
    breaks_instagram <- seq(0, 300, by = 25)
    labels_instagram <- seq(0, 300, by = 25)
    limits_instagram <- c(0, 300)
    
    breaks_pocz <- seq(360, 840, by = 60)
    labels_pocz <- c("06:00", "07:00","08:00", "09:00", "10:00", "11:00", "12:00", "13:00",
                     "14:00")
    limits_pocz <- c(360, 840)
    
    
    
    server <- function(input, output, session) {
      
      output$barPlot <- renderPlotly({
        
        if(length(input$kiedy) == 2){
          beg <- as.Date(input$kiedy[1],"%d-%m-%Y")
          end <- as.Date(input$kiedy[2],"%d-%m-%Y")
        } else{
          beg <- as.Date("01-12-2023","%d-%m-%Y")
          end <- as.Date("24-01-2024","%d-%m-%Y")
        }
        if(input$wykres == "kroki"){
          
          if(length(input$osoba) %in% c(0,2)){
            if(length(input$osoba) == 2){
              df1 %>% filter(d >= beg, d <= end) %>% select(d, `kroki BASIA`, `kroki MICHAL`)%>%
                pivot_longer(-d, names_to = "osoba", values_to = "kroki") %>% 
                ggplot(aes(x = d, y = kroki, fill = osoba)) + 
                geom_bar(stat = "identity", position = "dodge", width = 1) +
                theme(panel.background = element_rect(fill = tlo),
                      plot.background = element_rect(fill = tlo)) +
                theme(legend.position = "none") +
                labs(
                  x = "Dzień",
                  y = "Liczba kroków",
                ) +
                ylim(0,30000) ->plot
              
              plot + scale_fill_manual(values = c(fillB,fillM)) ->plot
              plot
            }
            else if(length(input$osoba) == 0){
              plot <- ggplot() +                
                annotate("text",
                         x = 1,
                         y = 1,
                         size = 8,
                         label = "Wybierz osobę") + 
                theme_void() + 
                theme(panel.background = element_rect(fill = tlo),
                      plot.background = element_rect(fill = tlo))
              
            }
            
          }
          else if(input$osoba == "michal"){
            ggplot(df1 %>% filter(d >= beg, d <= end), aes(x = d, y = `kroki MICHAL`)) +
              geom_bar(stat = "identity", fill = fillM, width = 0.5) +
              theme(panel.background = element_rect(fill = tlo),
                    plot.background = element_rect(fill = tlo)) +
              labs(
                x = "Dzień",
                y = "Liczba kroków"
              )+ylim(0,30000) -> plot
          }
          else if(input$osoba == "basia"){
            ggplot(df1%>% filter(d >= beg, d <= end), aes(x = d, y = `kroki BASIA`)) +
              geom_bar(stat = "identity", fill = fillB, width = 0.5) +
              theme(panel.background = element_rect(fill = tlo),
                    plot.background = element_rect(fill = tlo))+
              labs(
                x = "Dzień",
                y = "Liczba kroków"
              )+ylim(0,30000) -> plot
          } 
        }
        
        else if(input$wykres == "instagram"){
          
          if(length(input$osoba) %in% c(0,2)){
            if(length(input$osoba) == 2){
              df1 %>% filter(d >= beg, d <= end)%>% select(d, `instagram BASIA`, `instagram MICHAL`)%>%
                pivot_longer(-d, names_to = "osoba", values_to = "instagram") %>% 
                ggplot(aes(x = d, y = instagram, fill = osoba)) + 
                geom_bar(stat = "identity", position = "dodge", width = 1) +
                theme(panel.background = element_rect(fill = tlo),
                      plot.background = element_rect(fill = tlo)) +
                theme(legend.position = "none")+
                labs(
                  x = "Dzień",
                  y = "Liczba minut na Instagramie"
                ) +
                ylim(0,300)->plot
              
              plot + scale_fill_manual(values = c(fillB,fillM)) ->plot
            }
            else if(length(input$osoba) == 0){
              plot <- ggplot() +                
                annotate("text",
                         x = 1,
                         y = 1,
                         size = 8,
                         label = "Wybierz osobę") + 
                theme_void() + 
                theme(panel.background = element_rect(fill = tlo),
                      plot.background = element_rect(fill = tlo))
              
            }
            
          }
          else if(input$osoba == "michal"){
            ggplot(df1%>% filter(d >= beg, d <= end), aes(x = d, y = `instagram MICHAL`)) +
              geom_bar(stat = "identity", fill = fillM, width = 0.5) +
              theme(panel.background = element_rect(fill = tlo),
                    plot.background = element_rect(fill = tlo)) +
              labs(
                x = "Dzień",
                y = "Liczba minut na Instagramie"
              )+ylim(0,300) -> plot
          }
          else if(input$osoba == "basia"){
            ggplot(df1%>% filter(d >= beg, d <= end), aes(x = d, y = `instagram BASIA`)) +
              geom_bar(stat = "identity", fill = fillB, width = 0.5) +
              theme(panel.background = element_rect(fill = tlo),
                    plot.background = element_rect(fill = tlo))+
              labs(
                x = "Dzień",
                y = "Liczba minut na Instagramie"
              )+ylim(0,300) -> plot
          } 
        }
        
        else if(input$wykres == "stosunek"){
          
          if(length(input$osoba) == 2){
            df1%>% filter(d >= beg, d <= end) %>% select(d, `instagram BASIA`, `instagram MICHAL`)%>%
              pivot_longer(-d, names_to = "osoba", values_to = "instagram")  %>%
              mutate (osoba = word(osoba,2))-> a
            df1 %>% select(d, `kroki BASIA`, `kroki MICHAL`) %>%
              pivot_longer(-d, names_to = "osoba", values_to = "kroki") %>%
              mutate(osoba = word(osoba,2))-> b
            inner_join(a,b) -> df_merged
            
            ggplot(df_merged,aes(x = d, y = ifelse(instagram !=0, kroki/instagram, 0), fill = osoba)) + 
              geom_bar(stat = "identity", position = "dodge", width = 1) +
              theme(panel.background = element_rect(fill = tlo),
                    plot.background = element_rect(fill = tlo)) +
              theme(legend.position = "none")+
              labs(
                x = "Dzień",
                y = "Stosunek liczby kroków do minut na instagramie"
              ) +
              ylim(0,5000)->plot
            
            plot + scale_fill_manual(values = c(fillB,fillM)) ->plot
          }
          else if(length(input$osoba) == 0){
            plot <- ggplot() +                
              annotate("text",
                       x = 1,
                       y = 1,
                       size = 8,
                       label = "Wybierz osobę") + 
              theme_void() + 
              theme(panel.background = element_rect(fill = tlo),
                    plot.background = element_rect(fill = tlo))
            
          }
          else if(input$osoba == "michal"){
            ggplot(df1%>% filter(d >= beg, d <= end), aes(x = d, y = ifelse(`instagram MICHAL` != 0,`kroki MICHAL`/`instagram MICHAL`, 0))) +
              geom_bar(stat = "identity", fill = fillM, width = 0.5) +
              theme(panel.background = element_rect(fill = tlo),
                    plot.background = element_rect(fill = tlo)) +
              labs(
                x = "Dzień",
                y = "Stosunek liczby kroków do minut na instagramie"
              )+ylim(0,5000) -> plot
          }
          else if(input$osoba == "basia"){
            ggplot(df1%>% filter(d >= beg, d <= end), aes(x = d, y = ifelse(`instagram BASIA` != 0,`kroki BASIA`/`instagram BASIA`,0))) +
              geom_bar(stat = "identity", fill = fillB, width = 0.5) +
              theme(panel.background = element_rect(fill = tlo),
                    plot.background = element_rect(fill = tlo))+
              labs(
                x = "Dzień",
                y = "Stosunek liczby kroków do minut na instagramie"
              )+ylim(0,5000) -> plot
          }
        }
        
        ggplotly(plot)
        
      })
      
      output$BBox <- renderValueBox({
        if(length(input$kiedy) == 2){
          beg <- as.Date(input$kiedy[1],"%d-%m-%Y")
          end <- as.Date(input$kiedy[2],"%d-%m-%Y")
        } else{
          beg <- as.Date("01-12-2023","%d-%m-%Y")
          end <- as.Date("24-01-2024","%d-%m-%Y")
        }
        
        if(input$wykres == "kroki"){
          
          valueBox(
            floor(mean((df1 %>% filter(d >= beg, d <= end))$`kroki BASIA`, na.rm = T)),
            subtitle = "BASIA - Średnia liczba kroków",
            if(floor(mean((df1 %>% filter(d >= beg, d <= end))$`kroki BASIA`, na.rm = T))>= 5000){
              icon = icon("thumbs-up")
            }else{
              icon =  icon("thumbs-down")
            },
            color = "purple"
          )
        }else if(input$wykres =="instagram"){
          valueBox(
            floor(mean((df1 %>% filter(d >= beg, d <= end))$`instagram BASIA`, na.rm = T)),
            subtitle = "BASIA - średnia liczba minut na instagramie ",
            if(floor(mean((df1 %>% filter(d >= beg, d <= end))$`instagram BASIA`, na.rm = T))<= 60){
              icon = icon("thumbs-up")
            }else{
              icon =  icon("thumbs-down")
            },
            color = "purple"
          )
        }
        else{
          valueBox(
            floor(mean(((df1 %>% filter(d >= beg, d <= end, `instagram BASIA`!=0))$`kroki BASIA`)/(((df1 %>% filter(d >= beg, d <= end,`instagram BASIA`!=0)))$`instagram BASIA`), na.rm = T)),
            subtitle = "BASIA - średni stosunek kroków do minut na instagramie",
            if(floor(mean(((df1 %>% filter(d >= beg, d <= end, `instagram BASIA`!=0))$`kroki BASIA`)/(((df1 %>% filter(d >= beg, d <= end,`instagram BASIA`!=0)))$`instagram BASIA`), na.rm = T))>= 200){
              icon = icon("thumbs-up")
            }else{
              icon =  icon("thumbs-down")
            },
            color = "purple"
          )
        }
      })
      output$MBox <- renderValueBox({
        if(length(input$kiedy) == 2){
          beg <- as.Date(input$kiedy[1],"%d-%m-%Y")
          end <- as.Date(input$kiedy[2],"%d-%m-%Y")
        } else{
          beg <- as.Date("01-12-2023","%d-%m-%Y")
          end <- as.Date("24-01-2024","%d-%m-%Y")
        }
        if(input$wykres == "kroki"){
          
          valueBox(
            floor(mean((df1 %>% filter(d >= beg, d <= end))$`kroki MICHAL`, na.rm = T)),
            subtitle = "MICHAŁ - średnia liczba kroków",
            if(floor(mean((df1 %>% filter(d >= beg, d <= end))$`kroki MICHAL`, na.rm = T))>= 5000){
              icon = icon("thumbs-up")
            }else{
              icon =  icon("thumbs-down")
            },
            color = "yellow"
          )}
        else if(input$wykres =="instagram"){
          valueBox(
            floor(mean((df1 %>% filter(d >= beg, d <= end))$`instagram MICHAL`, na.rm = T)),
            subtitle = "MICHAŁ - średnia liczba minut na instagramie",
            if(floor(mean((df1 %>% filter(d >= beg, d <= end))$`instagram MICHAL`, na.rm = T))<= 60){
              icon = icon("thumbs-up")
            }else{
              icon =  icon("thumbs-down")
            },
            color = "yellow"
          )
        }
        else{
          valueBox(
            floor(mean(((df1 %>% filter(d >= beg, d <= end, `instagram MICHAL`!=0))$`kroki MICHAL`)/(((df1 %>% filter(d >= beg, d <= end,`instagram MICHAL`!=0)))$`instagram MICHAL`), na.rm = T)),
            subtitle = "MICHAŁ - średni stosunek kroków do minut na instagramie",
            if(floor(mean(((df1 %>% filter(d >= beg, d <= end, `instagram MICHAL`!=0))$`kroki MICHAL`)/(((df1 %>% filter(d >= beg, d <= end,`instagram MICHAL`!=0)))$`instagram MICHAL`), na.rm = T))>= 200){
              icon = icon("thumbs-up")
            }else{
              icon =  icon("thumbs-down")
            },
            color = "yellow"
          )
        }
      })
      
      
      output$ile_na_nogach <- renderPlotly({
        
        color_b <- "#653AB0"
          color_m <- "orange3"
            
          
          if(length(input$kiedy2) == 2){
            beg2 <- as.Date(input$kiedy2[1],"%d-%m-%Y")
            end2 <- as.Date(input$kiedy2[2],"%d-%m-%Y")
            if (input$wykres2 == "noc" && end2 == as.Date("24-01-2024","%d-%m-%Y")){
              end2 <- as.Date("23-01-2024","%d-%m-%Y")
            }
          } else{
            beg2 <- as.Date("01-12-2023","%d-%m-%Y")
            end2 <- as.Date("24-01-2024","%d-%m-%Y")
            if (input$wykres2 == "noc"){
              end2 <- as.Date("23-01-2024","%d-%m-%Y")
            }
          }
          
          
          
          
          if (input$wykres2 == "dzien"){
            if(length(input$osoba2) == 0){
              plot <- ggplot() +                
                annotate("text",
                         x = 1,
                         y = 1,
                         size = 8,
                         label = "Wybierz osobę") + 
                theme_void() + 
                theme(panel.background = element_rect(fill = tlo),
                      plot.background = element_rect(fill = tlo))
              
            }else{
              ggplot(df_merged %>%
                       filter(osoba %in% input$osoba2, d >= beg2, d <= end2)) +
                geom_segment(aes(x = d, xend = d, y = pocz, yend = czas + pocz,
                                 color = osoba),
                             linewidth = 2,alpha = 0.6) +
                scale_color_manual(values = c("b" = color_b, "m" = color_m)) +
                theme(panel.background = element_rect(fill = tlo),
                      plot.background = element_rect(fill = tlo)) +
                scale_y_continuous(
                  breaks = seq(360, 1800, by = 120),
                  labels = c("06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00",
                             "20:00", "22:00", "+00:00", "+02:00", "+04:00", "+06:00"),
                  limits = c(360, 1800),
                  expand = c(0, 0)) +
                labs(x = "Dzień",
                     y = "Godziny") +
                theme(legend.position = "none") -> plot
            }}
          else {
            if(length(input$osoba2) == 0){
              plot <- ggplot() +                
                annotate("text",
                         x = 1,
                         y = 1,
                         size = 8,
                         label = "Wybierz osobę") + 
                theme_void() + 
                theme(panel.background = element_rect(fill = tlo),
                      plot.background = element_rect(fill = tlo))
              
            }else{
              ggplot(df_spanko %>%
                       filter(osoba %in% input$osoba2, d >= beg2, d <= end2)) +
                geom_segment(aes(x = d, xend = d, y = pocz, yend = czas + pocz,
                                 color = osoba),
                             linewidth = 2,alpha = 0.6) +
                scale_color_manual(values = c("b" = color_b, "m" = color_m)) +
                theme(panel.background = element_rect(fill = tlo),
                      plot.background = element_rect(fill = tlo)) +
                scale_y_continuous(
                  breaks = seq(1200, 2400, by = 120),
                  labels = c("-20:00", "-22:00", "00:00", "02:00", "04:00", "06:00",
                             "08:00", "10:00", "12:00", "14:00", "16:00"),
                  limits = c(1200, 2400),
                  expand = c(0, 0)) +
                labs(x = "Dzień",
                     y = "Godziny") +
                theme(legend.position = "none") -> plot
            }}
          ggplotly(plot)
          
          
      })
      
      output$BBox2 <- renderValueBox({
        color_b <- "#0007FF"
          
        if(length(input$kiedy2) == 2){
          beg2 <- as.Date(input$kiedy2[1],"%d-%m-%Y")
          end2 <- as.Date(input$kiedy2[2],"%d-%m-%Y")
          if (input$wykres2 == "noc" && end2 == as.Date("24-01-2024","%d-%m-%Y")){
            end2 <- as.Date("23-01-2024","%d-%m-%Y")
          }
        } else{
          beg2 <- as.Date("01-12-2023","%d-%m-%Y")
          end2 <- as.Date("24-01-2024","%d-%m-%Y")
          if (input$wykres2 == "noc"){
            end2 <- as.Date("23-01-2024","%d-%m-%Y")
          }
        }
        
        if(length(input$kiedy2) == 2){
          beg2 <- as.Date(input$kiedy2[1],"%d-%m-%Y")
          end2 <- as.Date(input$kiedy2[2],"%d-%m-%Y")
          if (input$wykres2 == "noc" && end2 == as.Date("24-01-2024","%d-%m-%Y")){
            end2 <- as.Date("23-01-2024","%d-%m-%Y")
          }
        } else{
          beg2 <- as.Date("01-12-2023","%d-%m-%Y")
          end2 <- as.Date("24-01-2024","%d-%m-%Y")
          if (input$wykres2 == "noc"){
            end2 <- as.Date("23-01-2024","%d-%m-%Y")
          }
        }
        
        
        if(input$wykres2 == "dzien"){
          max((df_merged %>% filter(d >= beg2, d <= end2, osoba == "b"))$czas,
              na.rm = T) -> max_b
          valueBox(
            paste(floor(max_b/60), " h ", max_b %% 60, "min"),
            subtitle = "BASIA - najdłuższy czas na nogach",
            if(max_b <= 1080){
              icon = icon("face-smile")
            }else{
              icon =  icon("face-frown")
            },
            color = "purple"
          )
        }else if(input$wykres2 =="noc"){
          min((df_spanko %>% filter(d >= beg2, d <= end2, osoba == "b"))$czas,
              na.rm = T) -> min_b
          valueBox(
            paste(floor(min_b/60), " h ", min_b %% 60, "min"),
            subtitle = "BASIA - najkrótszy czas w łóżku",
            if(min_b >= 360){
              icon = icon("face-smile")
            }else{
              icon =  icon("face-frown")
            },
            color = "purple"
          )
        }
      })
      
      output$MBox2 <- renderValueBox({
        color_m <- "#FFF800"
          
        if(length(input$kiedy2) == 2){
          beg2 <- as.Date(input$kiedy2[1],"%d-%m-%Y")
          end2 <- as.Date(input$kiedy2[2],"%d-%m-%Y")
          if (input$wykres2 == "noc" && end2 == as.Date("24-01-2024","%d-%m-%Y")){
            end2 <- as.Date("23-01-2024","%d-%m-%Y")
          }
        } else{
          beg2 <- as.Date("01-12-2023","%d-%m-%Y")
          end2 <- as.Date("24-01-2024","%d-%m-%Y")
          if (input$wykres2 == "noc"){
            end2 <- as.Date("23-01-2024","%d-%m-%Y")
          }
        }
        
        
        if(input$wykres2 == "dzien"){
          max((df_merged %>% filter(d >= beg2, d <= end2, osoba == "m"))$czas,
              na.rm = T) -> max_m
          valueBox(
            paste(floor(max_m/60), " h ", max_m %% 60, "min"),
            subtitle = "MICHAŁ - najdłuższy czas na nogach",
            if(max_m <= 960){
              icon = icon("face-smile")
            }else{
              icon =  icon("face-frown")
            },
            color = "yellow"
          )
        }else if(input$wykres2 =="noc"){
          min((df_spanko %>% filter(d >= beg2, d <= end2, osoba == "m"))$czas,
              na.rm = T) -> min_m
          valueBox(
            paste(floor(min_m/60), " h ", min_m %% 60, "min"),
            subtitle = "MICHAŁ - najkrótszy czas w łóżku",
            if(min_m >= 360){
              icon = icon("face-smile")
            }else{
              icon =  icon("face-frown")
            },
            color = "yellow"
          )
        }
      })
      
      output$select_Var_2 <- renderUI({
        
        if (!is.null(input$zmienna2)){
          temp <- input$zmienna2
        } else {
          temp <- "Liczba kroków"
        }
        
        available_choices <- if (input$osoba31 == input$osoba32) {
          setdiff(zmienne_3, input$zmienna1)
        } else {
          zmienne_3
        }
        
        selectInput(
          inputId = "zmienna2",
          "Wybierz zmienną na oś OY:",
          choices = available_choices,
          selected = ifelse(input$osoba31 == input$osoba32 &&
                              input$zmienna1 == temp, 
                            available_choices[1], 
                            ifelse(is.null(temp),available_choices[1], temp))
        )})
      
      output$plot3 <- renderPlotly({
        
        
        
        var_1_label <- input$zmienna1
        var_2_label <- input$zmienna2
        osoba_1 <- input$osoba31
        osoba_2 <- input$osoba32
        
        if (input$zmienna1 == "Liczba kroków"){
          var_1 <- "kroki"
        } else if (input$zmienna1 == "Minuty na instagramie"){
          var_1 <- "instagram"
        } else if (input$zmienna1 == "Godzina wstania"){
          var_1 <- "pocz"
        }else {
          var_1 <- "czas"
        }
        
        if (input$zmienna2 == "Liczba kroków"){
          var_2 <- "kroki"
        } else if (input$zmienna2 == "Minuty na instagramie"){
          var_2 <- "instagram"
        } else if (input$zmienna2 == "Godzina wstania"){
          var_2 <- "pocz"
        }else {
          var_2 <- "czas"
        }
        
        if (osoba_1 == "b"){
          osoba_1_label <- "Basi"
        } else {
          osoba_1_label <- "Michała"
        }
        
        if (osoba_2 == "b"){
          osoba_2_label <-  "Basi"
        } else {
          osoba_2_label <- "Michała"
        }
        
        if (input$zmienna1 == "Liczba kroków"){
          x_breaks <- breaks_kroki
          x_labels <- labels_kroki
          x_limits <- limits_kroki
        } else if (input$zmienna1 == "Minuty na instagramie"){
          x_breaks <- breaks_instagram
          x_labels <- labels_instagram
          x_limits <- limits_instagram
        } else if (input$zmienna1 == "Godzina wstania"){
          x_breaks <- breaks_pocz
          x_labels <- labels_pocz
          x_limits <- limits_pocz
        } else {
          x_breaks <- breaks_czas
          x_labels <- labels_czas
          x_limits <- limits_czas
        }
        
        if (input$zmienna2 == "Liczba kroków"){
          y_breaks <- breaks_kroki
          y_labels <- labels_kroki
          y_limits <- limits_kroki
        } else if (input$zmienna2 == "Minuty na instagramie"){
          y_breaks <- breaks_instagram
          y_labels <- labels_instagram
          y_limits <- limits_instagram
        } else if (input$zmienna2 == "Godzina wstania"){
          y_breaks <- breaks_pocz
          y_labels <- labels_pocz
          y_limits <- limits_pocz
        } else {
          y_breaks <- breaks_czas
          y_labels <- labels_czas
          y_limits <- limits_czas
        }
        
        
        data_1 <- df3 %>% 
          filter(osoba == osoba_1) %>% 
          select(d,var_1)
        names(data_1) <- c("d", "col1")
        
        data_2 <- df3 %>% 
          filter(osoba == osoba_2) %>% 
          select(d,var_2)
        names(data_2) <- c("d", "col2")
        
        
        data_combined <- full_join(data_1, data_2)
        
        plot <- ggplot(data_combined, aes(x = col1, y = col2)) +
          geom_point() +
          theme(panel.background = element_rect(fill = tlo),
                plot.background = element_rect(fill = tlo))+
          labs(x = paste(sep = " ", var_1_label, osoba_1_label),
               y = paste(sep = " ", var_2_label, osoba_2_label)) +
          scale_x_continuous(
            breaks = x_breaks,
            labels = x_labels,
            limits = x_limits,
            expand = c(0,0)
          ) +
          scale_y_continuous(
            breaks = y_breaks,
            labels = y_labels,
            limits = y_limits,
            expand = c(0,0)
          ) +
          theme(legend.position = "none")
        
        ggplotly(plot)
        
      })
    }
    
    ui <- dashboardPage(
      dashboardHeader(title = "Nasza aktywność", titleWidth = 300),
    dashboardSidebar( width = 400,
                      prettyCheckboxGroup(
                        inputId = "osoba",
                        label = "Wybierz osobę:",
                        choices = c(
                          "Basia" = "basia",
                          "Michał" = "michal"
                        ),
                        selected = "basia",
                        icon = icon("user"),
                        inline = TRUE
                      ),
                      selectInput(
                        "wykres",
                        "Co chcesz zobaczyć?",
                        choices = c(
                          "Kroki" = "kroki", 
                          "Instagram" = "instagram", 
                          "Stosunek kroków do instagrama" = "stosunek")
                      ),
                      airDatepickerInput(
                        inputId = "kiedy",
                        label = "Wybierz okres (data początkowa, data końcowa):",
                        multiple = 2, 
                        clearButton = TRUE,
                        language = "pl",
                        minDate = "2023-12-01",
                        maxDate = "2024-01-24",
                        todayButton = T,
                        toggleSelected = T,
                        inline = T
                      )),dashboardBody(
                        tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #8EC7DD;
      }
    '))),
    fluidRow(
      column(width = 7,
             box(
               title = "Wykres", width = NULL, status = "primary", background = "light-blue",
               shinycssloaders::withSpinner(color = "darkblue",
                                            type = 4,plotlyOutput("barPlot", height = 400))
             )
      ),
      
      column(width = 5,
             box(
               width = 15, background = "light-blue",
               HTML(
                 "<span style='font-size: 13px;'>
               Panele powyżej sumaryzują wyniki na wykresie,
                pokazując średnią wartość danej wielkości podczas wybranego okresu.
                <p>Również <strong>ocenia tą wartość</strong> łapką w górę lub w dół według kryteriów:</p>
                <p>1) łapka w górę dla średniej liczby kroków <strong>>= 5000</strong>,</p>
                <p>2) łapka w górę dla średniej liczby minut spędzonych na instagramie <strong><= 60</strong>,</p>   
                <p>3) łapka w górę dla średniego stosunku kroków do minut na instagramie <strong>>= 200</strong>,</p>
                a w przeciwnym wypadku łapka w dół."
               )
               ,
               valueBoxOutput("BBox", width = 15),
               valueBoxOutput("MBox", width = 15)
               
             ))
    )
                      ))
    
    
    ui2 <- dashboardPage(
      dashboardHeader(title = "Godziny wstania i położenia się", titleWidth = 300),
    dashboardSidebar( width = 400,
                      prettyCheckboxGroup(
                        inputId = "osoba2",
                        label = "Wybierz osobę",
                        choices = c(
                          "Basia" = "b",
                          "Michał" = "m"
                        ),
                        selected = "b",
                        icon = icon("user"),
                        inline = TRUE
                      ),
                      radioButtons(
                        "wykres2",
                        "Co chcesz zobaczyć?",
                        choices = c(
                          "Czas na nogach" = "dzien", 
                          "Czas w łóżku" = "noc")
                      ),
                      airDatepickerInput(
                        inputId = "kiedy2",
                        label = "Wybierz okres (data początkowa, data końcowa):",
                        multiple = 2, 
                        clearButton = TRUE,
                        language = "pl",
                        minDate = "2023-12-01",
                        maxDate = "2024-01-24",
                        todayButton = T,
                        toggleSelected = T,
                        inline = T
                      )),dashboardBody(
                        tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #8EC7DD;
      }
    '))),
    fluidRow(
      column(width = 7,
             box(
               title = "Wykres", width = NULL, status = "primary", background = "light-blue",
               shinycssloaders::withSpinner(color = "darkblue",
                                            type = 4,plotlyOutput("ile_na_nogach", height = 400))
             )
      )
      ,
      column(width = 5,
             box(
               width = 15, background = "light-blue",
               
               HTML("<span style='font-size: 13px;'>
                     Panele powyżej sumaryzują wyniki na wykresie,
                pokazując najdłuższy czas na nogach lub najkrótszy czas w łóżku
                podczas wybranego okresu.
                <p>Również <strong>ocenia tą wartość</strong> łapką w górę lub w dół według kryteriów:</p>
                <p>1) uśmiechnięta buźka dla najdłuższego czasu na nogach <strong><= 960</strong>,</p>
                <p>2) uśmiechnięta buźka dla najkrószego czasu w łóżku <strong>>= 360</strong>,</p>
                a w przeciwnym wypadku smutna buźka."
               ),
               valueBoxOutput("BBox2", width = 15),
               valueBoxOutput("MBox2", width = 15)
               
             )
             
      )
    )))
    
    ui3 <- dashboardPage(
      dashboardHeader(title = "Szczegółowa analiza", titleWidth = 300),
    dashboardSidebar(
      radioButtons(
        inputId = "osoba31",
        label = "Wybierz osobe do zmiennej z osi OX:",
        choices = c(
          "Basia" = "b",
          "Michał" = "m"
        ),
        selected = "b",
        inline = TRUE
      ),
      
      selectInput(
        inputId = "zmienna1",
        "Wybierz zmienną na oś OX:",
        zmienne_3,
        selected = "Godzina wstania"
      ),
      radioButtons(
        inputId = "osoba32",
        label = "Wybierz osobe do zmiennej z osi OY:",
        choices = c(
          "Basia" = "b",
          "Michał" = "m"
        ),
        selected = "m",
        inline = TRUE
      ),
      uiOutput("select_Var_2")
    )
    
    ,dashboardBody(
      tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #8EC7DD;
      }
    '))),
    fluidRow(
      column(width = 12,
             box(
               width = 100,
               plotlyOutput("plot3"),
               background = "light-blue"
             )
      )
      
    )
    ))
    
    navbarPage("Jak spędzamy dzień?",
               tabPanel(title = "Aktywność i instagram", ui, icon = icon("database")),
               tabPanel(title = "Godzina wstania i położenia się", ui2,
                        icon = icon("database")),
               tabPanel(title = "Szczegółowa analiza", ui3,
                        icon = icon("database")),
               theme = bslib::bs_theme(version = 4, bootswatch = "superhero")
               
    ) -> navbar
    
    
    
    
    runApp(shinyApp(navbar, server))
    
    