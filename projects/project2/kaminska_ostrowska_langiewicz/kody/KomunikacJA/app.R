#### wczytanie bibliotek ####

library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(wordcloud2)

#### wczytanie bibliotek koniec####




##### wczytanie funkcji pomocniczych #####
filter_outliers <- function(data) {
  boxplot_stats <- boxplot.stats(data$MessageLength)
  outliers <- boxplot_stats$out
  return(data[!data$MessageLength %in% outliers, ])
}
##### wczytanie funkcji pomocniczych koniec #####





###### wczytanie danych #####


##### wczytanie danych heatmapa #####
heatMap_data <- read.csv("./appData/heatMap/heatMapData.csv",
                         colClasses = c(date = "Date"))
##### wczytanie danych heatmapa koniec #####


##### wczytanie danych linePlot Ani #####
linePlot_data <- read.csv("./appData/wyslaneOdebrane/wyslaneOdebrane_all.csv",
                          colClasses = c(date = "Date"))
##### wczytanie danych linePlot Ani koniec #####


##### wczytanie danych emojiPlot Zosi #####
emojiPlot_data <- read.csv("./appData/emojiData/cloud_data.csv")
##### wczytanie danych emojiPlot Zosi koniec #####


##### wczytanie danych barPlot Zosi #####
barPlot_data <- read.csv("./appData/emojiData/bar_data.csv")
##### wczytanie danych barPlot Zosi koniec #####


##### wczytanie danych dlugosci wiadomosci Zosi #####
dlugosciWiadomosciPlot_data <- read.csv("appData/dlugosciWiadomosciPlot/dlugosciWiadomosci_data.csv")
##### wczytanie danych dlugosci wiadomosci Zosi koniec #####


##### wczytanie danych friendsPlot #####
friendsPlot_data <- read.csv("./appData/friendsPlot/friendsData.csv",
                             colClasses = c(date = "Date"))
##### wczytanie danych friendsPlot koniec #####


###### wczytanie danych koniec #####



#### obsluga UI


############################# ui z logo #####################

ui0 <- tags$div(
  class = "logo",
  img(src = "logo.png", 
      style = "height:76vh;"),
  tags$footer(
    HTML("<a href = 'https://github.com/FilipLangiewicz/Projekt_TWD_02' target='_blank'>Link do repozytorium na GitHubie </a>"),
    HTML("<p class = 'copyright'>Copyright © 2023 <i>Kamińska, Ostrowska, Langiewicz</i></p>"))
  
)



############################# ui z logo koniec #####################


############################# ui do heatmapy #####################
ui1 <- tags$div(
  
  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(  
          class = "person_button",
          tags$button(
            id = "a",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_a.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Ania</p>"),
          )
        ),
        tags$div(  
          class = "person_button",
          tags$button(
            id = "z",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_z.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Zosia</p>"),
          )
        ),
        tags$div(  
          class = "person_button",
          tags$button(
            id = "f",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_f.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Filip</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button",
          tags$button(
            id = "mg",
            class = c("btn btn-default action-button shiny-bound-input", "mg_button"),
            ""
          )
        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "ig",
            class = c("btn btn-default action-button shiny-bound-input", "ig_button"),
            ""
          )
        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "sp",
            class = c("btn btn-default action-button shiny-bound-input", "sp_button"),
            ""
          )
        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "all",
            class = c("btn btn-default action-button shiny-bound-input", "all_button"),
            ""
          )
        )
      ),
    ),
    
    tags$div(
      tags$div(
        class = "tytul_konwersacji_convo",
        imageOutput("person_title_im",
                    height = "auto",
                    width = "auto"),
        textOutput("person_main"),
        # HTML('<h1 class = "tytul_konwersacji"><b>Którego dnia roku najwięcej się komunikujemy?</b></h1>')
      ),
      class = "convo_div",

      tags$div(
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            textOutput("heatmapa_text1")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im",
                        height = "auto",
                        width = "auto"),
                   ),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("heatMapa_plot"),
            selectInput("input_year",
                        choices = unique(year(heatMap_data$date)) %>% sort(),
                        label = "Wybierz rok",
                        selected = 2023,
                        width = "7%")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im2",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Powyższa mapka pokazuje ile wiadomości danego dnia zostało przeze mnie odebranych i wysłanych w sumie. Wystarczy, że najedziesz na odpowiedni kwadracik i wszystkie ważne informacje powinny Ci się pokazać! Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało mi się pobrać, więc te wyniki mogą być zaniżone"
          )
        )
      )
      
      
    )
  )
)

############################# ui do heatmapy koniec #####################


############################# ui liczba wiadomosci Ani #####################
ui2 <- tags$div(
  
  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "a2",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_a.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Ania</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "z2",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_z.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Zosia</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "f2",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_f.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Filip</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button",
          tags$button(
            id = "mg2",
            class = c("btn btn-default action-button shiny-bound-input", "mg_button"),
            ""
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "ig2",
            class = c("btn btn-default action-button shiny-bound-input", "ig_button"),
            ""
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "sp2",
            class = c("btn btn-default action-button shiny-bound-input", "sp_button"),
            ""
          )
        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "all2",
            class = c("btn btn-default action-button shiny-bound-input", "all_button"),
            ""
          )
        )
      ),
    ),
    
    tags$div(
      tags$div(
        class = "tytul_konwersacji_convo",
        imageOutput("person_title_im2",
                    height = "auto",
                    width = "auto"),
        textOutput("person_main2"),
        
        # HTML('<h1 class = "tytul_konwersacji"><b>Z jakich aplikacji najwięcej korzystamy?</b></h1>')
      ),
      class = "convo_div",
      tags$div(
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            textOutput("linePlot_text1")
          ))),
      class = "convo_div",
      tags$div(
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im3",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("linePlot_plot")
            
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im4",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Powyższy wykres pokazuje ile wiadomości odebrała i wysłała dana osoba w sumie do danego dnia. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać. Wybierając poszczególne aplikacje oddzielnie możemy zobaczyć podział wymienionych wiadomości na wysłane i odebrane. Przy wyborze przycisku wszystkich aplikacji wiadomości są podzielone na aplikacje, na których były wymieniane. Ze względu na to, że Messenger jest przeze mnie używany o wiele częściej i intensywniej, niż pozostałe komunikatory, zastosowana tu została skala logarytmiczna, aby łatwiej było odczytać wartości na osi Y."
          )
        )),
      tags$div(
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            textOutput("linePlot_text2")
          )
        )
      ),
      tags$div(
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im21",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("linePlot_text2_answer")
          )
        )
      )
    )
  )
)





############################# ui liczba wiadomosci Ani koniec #####################


############################# ui emoji plot Zosi #####################
ui3 <- tags$div(
  
  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "a3",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_a.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Ania</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "z3",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_z.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Zosia</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "f3",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_f.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Filip</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button",
          tags$button(
            id = "mg3",
            class = c("btn btn-default action-button shiny-bound-input", "mg_button"),
            ""
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "ig3",
            class = c("btn btn-default action-button shiny-bound-input", "ig_button"),
            ""
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "all3",
            class = c("btn btn-default action-button shiny-bound-input", "all_button3"),
            ""
          )
        )
      ),
    ),
    
    tags$div(
      tags$div(
        class = "tytul_konwersacji_convo",
        imageOutput("person_title_im3",
                    height = "auto",
                    width = "auto"),
        textOutput("person_main3"),
        # HTML('<h1 class = "tytul_konwersacji"><b>Jakich emotek używamy najczęściej?</b></h1>')
      ),
      class = "convo_div",

      tags$div(
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            textOutput("emojiPlot_text1")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im5",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = "wiadomosc",
            textOutput("emoji_plot_title"),
            htmlOutput("emoji_plot"),
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im6",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Powyżej pokazane są najczęściej wysyłane przeze mnie emotki, podczas całego okresu używania wybranej aplikacji (dokładnie jaki to okres można sobie wyczytać z heatmapy w zakładce Wiadomości). Warto dodać że liczba emotek jest proporcjonalna do liczby wysłanych wiadomości, stąd duże różnice w liczbie wysłanych emotek między wybranymi osobami. Wykresy zawierają dane zarówno z konwersacji prywatnych jak i grupowych."
          ),
        ),
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            textOutput("emojiPlot_text2")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im7",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("animated_plot")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im8",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Tu pokazane jest 10 najczęściej używanych przeze mnie emotek. Są one wybrane spośród emotek używanych przeze mnie najczęściej w całym okresie posiadania wybranej aplikacji. Wykres pokazuje jak użycie danej emotki rośnie w czasie. Przedstawiona jest kumulatywna liczba użytych emotek, od momentu pierwszej wiadomości, do wybranej daty."
          )
        )
      )
      
      
    )
  )
)




############################# ui emoji plot Zosi koniec #####################


############################# ui dlugosci wiadomosci Zosi #####################
ui4 <- tags$div(

  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "a4",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_a.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Ania</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "z4",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_z.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Zosia</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "f4",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_f.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Filip</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "azf4",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_all.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Razem</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button",
          tags$button(
            id = "mg4",
            class = c("btn btn-default action-button shiny-bound-input", "mg_button"),
            ""
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "ig4",
            class = c("btn btn-default action-button shiny-bound-input", "ig_button"),
            ""
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "all4",
            class = c("btn btn-default action-button shiny-bound-input", "all_button3"),
            ""
          )
        )
      ),
     ),

    tags$div(
      tags$div(
        class = "tytul_konwersacji_convo",
        imageOutput("person_title_im4",
                    height = "auto",
                    width = "auto"),
        textOutput("person_main4"),
        # HTML('<h1 class = "tytul_konwersacji"><b>Jak długie są nasze wiadomości?</b></h1>')
      ),
      class = "convo_div",

      tags$div(
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            "Ciekawi mnie, czy wszystkie wiadomości mają podobną długość itd? Ciekawe też czy ma znaczenie, czy te wiadomości były z czatów grupowych, czy prywatnych, w końcu na priv pewnie piszemy krótsze wiadomości. Albo czy wszyscy piszą wiadomości o podobnej długości. Gdyby tylko dało się to jakoś sprawdzić..."
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im9",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("dlugosciWiadomosci_plot"),
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im19",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Wszystko możesz wyczytać z powyższego przygotowanego przeze mnie wykresu :))"
          ),
        ),
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            "Super! A tak w zasadzie to ile było tych wysłanych przez wiadomości?"
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im10",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text2")
          ),
        ),
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            "A ile z tego było na grupach?"
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im11",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text3")
          ),
        ),
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            "A na priv?"
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im12",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text4")
          ),
        ),
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            "Nieźle, to średnia długość wiadomości to ile w takim razie? Nigdy nie szło mi odczytywanie takich ładnych wykresów!"
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im13",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text5")
          ),
        ),
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            "A to w sumie dużo czy mało? Wyobraźnia też nie działa u mnie najlepiej... Może chcesz podesłać jakąś swoją przykładową wiadomość o takiej długości?"
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im14",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text6")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im15",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text7")
          ),
        ),
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            "Ostatnia rzecz, która mnie ciekawi, ile znaków miała najdłuższa wysłana wiadomość?"
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im16",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text8")
          )
        ),
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            "Dzięki!"
          ),
        ),
      )
    )
  )
)

############################# ui dlugosci wiadomosci Zosi koniec #####################

  
############################# ui friendsPlot #####################
ui5 <- tags$div(
  
  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(  
          class = "person_button_focused", 
          tags$button(
            id = "azf5",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "cat_all.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Razem</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button_focused",
          tags$button(
            id = "fb",
            class = c("btn btn-default action-button shiny-bound-input", "fb_button"),
            ""
          )
        )
      )
    ),
    
    tags$div(
      tags$div(
        class = "tytul_konwersacji_convo",
        imageOutput("person_title_im5",
                    height = "auto",
                    width = "auto"),
        textOutput("person_main5"),
        # HTML('<h1 class = "tytul_konwersacji"><b>Kiedy przybywa nam najwięcej znajomych?</b></h1>')
      ),
      class = "convo_div",
      
      tags$div(
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            "Siemka, słyszałem, że na wydziale MiNI w ogóle nie ma czasu na życie towarzyskie i że ludzie wcale nie mają tam znajomych, czy to prawda?"
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im17",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("friends_plot"),
          ),
        ),
        tags$div(
          class = "person_message",
          tags$div(
            class = "person_image_convo",
            imageOutput("person_message_im18",
                        height = "auto",
                        width = "auto"),
          ),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Hmm, wystarczy spojrzeć na tek wykres, który pokazuje jak zmieniała się nasza liczba znajomych na Facebooku w czasie. Widać na nim duże przyrosty w momencie pójścia na studia u nas wszystkich i w sumie liczba ta cały czas trochę rośnie, więc to chyba nie jest prawda... Ciekawa sytuacja jest również w przypadku pójścia do liceum, widać wzrost u Filipa i u Ani, ale u Zosi jest on rok wcześniej, bo była wtedy za granicą, gdzie jest inny system edukacji i rok wcześniej poszła do szkoły średniej"
          )
        )
      )
      
      
    )
  )
)
  
############################# ui friendsPlot koniec #####################
  

############################# ui główne #####################
ui_main <- tags$div(includeCSS("./css/styles.css"),
                    style = "background-color: red; display:block;",
                    tags$div(
                      style = "background-color: white;",
                      navbarPage("",
                                 tabPanel(HTML("<b class = 'JA'>JA</b>"), ui0),
                                 tabPanel(HTML("<b class = 'menu_text'>Wiadomości</b>"), ui1),
                                 tabPanel(HTML("<b class = 'menu_text'>Aplikacje</b>"), ui2),
                                 tabPanel(HTML("<b class = 'menu_text'>Emocje</b>"), ui3),
                                 tabPanel(HTML("<b class = 'menu_text'>Forma</b>"), ui4),
                                 tabPanel(HTML("<b class = 'menu_text'>Znajomi</b>"), ui5)
                      )
                    )
)

############################# ui główne koniec #####################




### oblsuga server

server <- function(input, output) {
  
  
  ### początkowe wybrane osoby i apki
  person_main <- reactiveVal("a")
  app_main <- reactiveVal("mg")
  typ_main <- reactiveVal(c('wyslane','odebrane','wszystkie'))
  
  
  #### wczytywanie początkowych danych na wykresy ####
  heatMap <- reactiveValues(data = heatMap_data %>%
                              filter(person == "a",
                                     app == "mg")
  )
  
  
  linePlot <- reactiveValues(data = linePlot_data %>%
                               filter(person == "a",
                                      app == "mg")
  )
  
  
  emojiPlot <- reactiveValues(data = emojiPlot_data %>%
                               filter(person == "a",
                                      app == "mg")
  )
  
  barPlot <- reactiveValues(data = barPlot_data %>%
                              filter(person == "a",
                                     app == "mg")
  )
  
  dlugosciWiadomosciPlot <- reactiveValues(data = dlugosciWiadomosciPlot_data %>%
                                filter(person == "z",
                                       app == "ig"
                                       )
  )
  #### wczytywanie początkowych danych na wykresy koniec ####
  
  
  
  #### aktualizacja danych po naciśnięciu push buttonow ####
  updateData <- function(){
    if (all(person_main() == c("a", "z", "f"))) {
      person_main("a")
    }
    
    heatMap$data <- heatMap_data %>%
      filter(person == person_main(),
             app %in% app_main())
    updateOptions()
  }
  
  updateData2 <- function() {
    if (all(person_main() == c("a", "z", "f"))) {
      person_main("a")
    }
    
    if (identical(app_main(), c("mg", "ig"))) {
      app_main("mg")
    }
    
    linePlot$data <- linePlot_data %>%
      filter(person == person_main(),
             app %in% app_main(),
             typ %in% typ_main())
  }
  
  updateData3 <- function() {
    if (all(person_main() == c("a", "z", "f"))) {
      person_main("a")
    }

    if (identical(app_main(), c("mg", "sp", "ig"))) {
      app_main("mg")
    }
    
    if (identical(app_main(), "sp")) {
      app_main("mg")
    }
    
    emojiPlot$data <- emojiPlot_data %>%
      filter(person == person_main(),
             app %in% app_main()) %>% 
      group_by(all_emojis) %>% 
      summarise(Freq = sum(Freq, na.rm = TRUE))
    
    updateData5()
  }
  
  updateData4 <- function() {
    if (identical(app_main(), c("mg", "sp", "ig"))) {
      app_main("ig")
    }
    
    if (identical(app_main(), "sp")) {
      app_main("ig")
    }
    
    dlugosciWiadomosciPlot$data <- dlugosciWiadomosciPlot_data %>%
      filter(person %in% person_main(),
             app %in% app_main()
             )
  }
  
  updateData5 <- function() {
    if (all(person_main() == c("a", "z", "f"))) {
      person_main("a")
    }

    if (identical(app_main(), c("mg", "sp", "ig"))) {
      app_main("mg")
    }
    
    if (identical(app_main(), "sp")) {
      app_main("mg")
    }

    if (all(app_main() == c("mg", "ig"))) {
      barPlot$data <- barPlot_data %>% 
        filter(person == person_main(),
               app == "both")
    } else {
    barPlot$data <- barPlot_data %>%
      filter(person == person_main(),
             app %in% app_main())
    }
  }
  #### aktualizacja danych po naciśnięciu push buttonow koniec ####
  
  
  ### aktualizacja mozliwych do wyboru opcji po nacisnieciu pushbuttonow na stronie Heatmapy
  updateOptions <- function() {
    updateSelectInput(inputId = "input_year",
                      choices = unique(year(heatMap$data$date)) %>% sort,
                      selected = ifelse(input$input_year %in% unique(year(heatMap$data$date)),
                                        input$input_year,
                                        2023))
  }
  
  
  
  ##### nasluchiwanie z mojej strony Heatmapy #####
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
  ##### nasluchiwanie z mojej strony Heatmapy koniec #####
  
  
  ##### nasluchiwanie ze strony linePlot Ani #####
  observeEvent(input$a2, {
    person_main("a")
    updateData2()
  })
  
  observeEvent(input$z2, {
    person_main("z")
    updateData2()
  })
  
  observeEvent(input$f2, {
    person_main("f")
    updateData2()
  })
  
  observeEvent(input$mg2, {
    typ_main(c('wyslane','odebrane','wszystkie'))
    app_main("mg")
    updateData2()
  })
  
  observeEvent(input$ig2, {
    typ_main(c('wyslane','odebrane','wszystkie'))
    app_main("ig")
    updateData2()
  })
  
  observeEvent(input$sp2, {
    typ_main(c('wyslane','odebrane','wszystkie'))
    app_main("sp")
    updateData2()
  })
  
  observeEvent(input$all2, {
    typ_main('wszystkie')
    app_main(c("mg","sp","ig"))
    updateData2()
  })
  ##### nasluchiwanie ze strony linePlot Ani koniec #####
  
  
  ##### nasluchiwanie ze strony emojiPlot Zosi #####
  observeEvent(input$a3, {
    person_main("a")
    updateData3()
  })
  
  observeEvent(input$z3, {
    person_main("z")
    updateData3()
  })
  
  observeEvent(input$f3, {
    person_main("f")
    updateData3()
  })
  
  observeEvent(input$mg3, {
    app_main("mg")
    updateData3()
  })
  
  observeEvent(input$ig3, {
    app_main("ig")
    updateData3()
  })
  
  observeEvent(input$all3, {
    app_main(c("mg", "ig"))
    updateData3()
  })
  ##### nasluchiwanie ze strony emojiPlot Zosi koniec #####
  
  
  ##### nasluchiwanie ze strony dlugosciWiadomosciPlot Zosi #####
  observeEvent(input$a4, {
    person_main("a")
    updateData4()
  })
  
  observeEvent(input$z4, {
    person_main("z")
    updateData4()
  })
  
  observeEvent(input$f4, {
    person_main("f")
    updateData4()
  })
  
  observeEvent(input$azf4, {
    person_main(c("a", "z", "f"))
    updateData4()
  })
  
  observeEvent(input$mg4, {
    app_main("mg")
    updateData4()
  })
  
  observeEvent(input$ig4, {
    app_main("ig")
    updateData4()
  })
  
  observeEvent(input$all4, {
    app_main(c("mg", "ig"))
    updateData4()
  })
  ##### nasluchiwanie ze strony dlugosciWiadomosciPlot Zosi koniec #####  
  
  
  ##### nasluchiwanie ze strony friendsPlot #####

  
  ##### nasluchiwanie ze strony friendsPlot koniec #####
  
  
  
  ################# tworzenie wykresów ################
  
  
  
  ### tworzenie wykresu heatmapy
  output$heatMapa_plot <- renderPlotly({
    updateData()
    
    chosen_app <- case_when(identical(app_main(),"mg") ~ " na Messengerze",
                            identical(app_main(),"ig") ~ " na Instagramie",
                            identical(app_main(),"sp") ~ " na Snapchacie",
                            TRUE ~ " we wszystkich aplikacjach")
    
    chosen_person <- case_when(person_main() == "a" ~ "Anię",
                               person_main() == "z" ~ "Zosię",
                               person_main() == "f" ~ "Filipa")
    plot_title <- paste0("<b>",
                         "Liczba wiadomości",
                         " wysłanych i odebranych przez ",
                         chosen_person,
                         chosen_app,
                         " w ",
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
                                         max(heatMap_data %>%
                                               filter(person == person_main(),
                                                      app %in% app_main()) %>%
                                               .$date),
                                         by = "day")),
                   by = "date") %>%
        filter(year(date) == input$input_year) %>%
        group_by(date) %>%
        summarise(liczba_wiadomosci = sum(liczba,
                                          na.rm = TRUE)) %>%
        ggplot(aes(x = day(date), y = month(date), fill = liczba_wiadomosci, text = paste0(format(date, "%d %B %Y"),
                                                                                           "<br>W tym dniu wysłano i odebrano w sumie<br><b> ",
                                                                                           liczba_wiadomosci,
                                                                                           " wiadomości</b>"))) +
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
                     y = 0.97, 
                     x = 0.51, 
                     xanchor = 'center', 
                     yanchor =  'top')) %>% 
      plotly::config(displayModeBar = FALSE
      ) -> p

    p[["x"]][["data"]][[2]][["hoverinfo"]] = 'skip'
    p[["x"]][["data"]][[3]][["hoverinfo"]] = 'skip'
    
    p[["x"]][["data"]][[4]][["marker"]][["colorbar"]][["title"]] = HTML("<br>ㅤ<br>ㅤ<br>Sumaryczna <br>liczba <br>wiadomości<br>ㅤ")
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
  
  
  ### tworzenie lineplot Ani
  output$linePlot_plot <- renderPlotly({
    updateData2()
    
    chosen_app <- case_when(identical(app_main(),"mg") ~ " na Messengerze",
                            identical(app_main(),"ig") ~ " na Instagramie",
                            identical(app_main(),"sp") ~ " na Snapchacie",
                            TRUE ~ (" we wszystkich aplikacjach"))
    
    chosen_person <- case_when(person_main() == "a" ~ "Anię",
                               person_main() == "z" ~ "Zosię",
                               person_main() == "f" ~ "Filipa")
    
    plot_title <- paste0("<b>Liczba wymienionych wiadomości przez ",
                         chosen_person,
                         chosen_app,
                         " do danego dnia</b>")
    
    legend_title <- case_when(
      identical(app_main(), "mg") ~ "<b>Typ</b>",
      identical(app_main(), "ig") ~ "<b>Typ</b>",
      identical(app_main(), "sp") ~ "<b>Typ</b>",
      TRUE ~ "<b>Aplikacja</b>")
    
    podpis_y <- case_when(
      identical(app_main(), "mg") ~ "<b>Liczba wiadomości</b>",
      identical(app_main(), "ig") ~ "<b>Liczba wiadomości</b>",
      identical(app_main(), "sp") ~ "<b>Liczba wiadomości</b>",
      TRUE ~ "<b>Liczba wiadomości (skala log)</b>")
    
    linePlot$data %>% 
      mutate(color_plot = case_when(
        identical(app_main(), "mg") ~ typ,
        identical(app_main(), "ig") ~ typ,
        identical(app_main(), "sp") ~ typ,
        TRUE ~ app)) %>%
      mutate(data = date) %>% 
      mutate(suma_wiadomości = suma_kumulacyjna) %>% 
      mutate(color_plot = ifelse(color_plot == "wyslane", "wysłane", ifelse(color_plot == "ig", "Instagram", ifelse(color_plot == "sp", "Snapchat", ifelse(color_plot == "mg", "Messenger", color_plot))))) %>%
      mutate(color_plot = case_when(
        identical(app_main(), "mg") ~ factor(color_plot, levels = c("wszystkie", "odebrane", "wysłane")),
        identical(app_main(), "ig") ~ factor(color_plot, levels = c("wszystkie", "odebrane", "wysłane")),
        identical(app_main(), "sp") ~ factor(color_plot, levels = c("wszystkie", "odebrane", "wysłane")),
        TRUE ~ factor(color_plot, levels = c("Messenger", "Instagram", "Snapchat")))) %>% 
      mutate(tekst = ifelse(color_plot == "wysłane", "wysłano", ifelse(color_plot == "odebrane", "odebrano", "odebrano i wysłano"))) %>% 
      plot_ly(
        x = ~data,
        y = ~suma_wiadomości,
        type = 'scatter',
        mode = 'lines',
        line = list(width = 3),
        color = ~color_plot,
        colors = c(
          "Messenger" = "#0695FF",
          "Instagram" = "#C13584",
          "Snapchat" = "#ECD504",
          "wszystkie" = "#0066CC",
          "wysłane" = "#00CC66",
          "odebrane" = "#99004C"
        ),
        hoverinfo = "text",
        hovertext = ~paste0(
          format(data, "%d %B %Y"),
          "<br>Do tego dnia ",
          tekst,
          "<br>w sumie <b>", suma_wiadomości, " </b>wiadomości", ifelse(app=="mg", " na Messengerze",
                                                                        ifelse(app=="ig", " na Instagramie",
                                                                               ifelse(app=="sp", " na Snapchacie",
                                                                                      "")))
        )
      )  -> p

    p <- if (identical(app_main(), "mg") || identical(app_main(), "ig") || identical(app_main(), "sp")) {
      p %>%
        layout(
          title = list(
            text = plot_title,
            font = list(size = 19, face = "bold"),
            y = 0.99,
            x = 0.51,
            xanchor = 'center',
            yanchor = 'top'
          ),
          legend = list(title = list(text = legend_title)),
          showlegend = TRUE,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(
            title = "<b>Zakres dat</b>",
            rangeslider = list(type = "date"),
            title = list(standoff = 15),
            showgrid = TRUE,
            gridcolor = "lightgrey",
            range = c(min(linePlot$data$date) - 60, max(linePlot$data$date) + 60)
          ),
          yaxis = list(
            title = list(text = podpis_y, 
                         standoff = 15),
            zeroline = TRUE,
            showgrid = TRUE,
            gridcolor = "lightgrey", 
            tickformat = " "
          ),
          margin = list(l = 50, r = 50, b = 50, t = 30, pad = 10)
        )
    } else {
      p  %>% 
        layout(
          title = list(
            text = plot_title,
            font = list(size = 19, face = "bold"),
            y = 0.99,
            x = 0.51,
            xanchor = 'center',
            yanchor = 'top'
          ),
          legend = list(title = list(text = legend_title)),
          showlegend = TRUE,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(
            title = "<b>Zakres dat</b>",
            rangeslider = list(type = "date"),
            title = list(standoff = 15), 
            showgrid = TRUE,
            gridcolor = "lightgrey",
            range = c(min(linePlot$data$date) - 60, max(linePlot$data$date) + 60)
          ),
          yaxis = list(
            range = c(-1.1, 1.2 * max(log10(linePlot$data$suma_kumulacyjna))),
            tickvals = c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000),
            ticktext = c("0", "1", "10", "100", "1000", "10000", "100000", "1000000"),
            title = list(text = podpis_y,
                         standoff = 15),
            type = 'log',
            showgrid = TRUE,
            zeroline = TRUE, 
            gridcolor = "lightgrey", 
            tickformat = ' '
          ),
          margin = list(l = 50, r = 50, b = 50, t = 30, pad = 10)
          
        )
    }
  }) 
  
  
  ### tworzenie emojiPlot Zosi
  output$emoji_plot <- renderUI({
    updateData3()
    
    wordcloud2(
      data = emojiPlot$data %>% 
        filter(emojiPlot$data$Freq >= (1 / 50)* max(emojiPlot$data$Freq)),
      color = "red",
      size = 1.5,
      minRotation = 0,
      maxRotation = 0,
      rotateRatio = 0,
      gridSize = 5,
      shape = "circle",
      shuffle = FALSE,
      backgroundColor = rgb(0,0,0,0)
    )
    
  })
  
  
  ### tworzenie animowanego barplot Zosi
  output$animated_plot <- renderPlotly({
    updateData3()
    
    data <- barPlot$data
    
    plot_ly(data,
            x = ~cumulative_count, y = ~emojis, 
            type = "bar", frame = ~month_year, 
            marker = list(color = "blue")) %>%
      layout(title = list(text = "<b>Top 10 najczęściej wysyłanych emotek w czasie</b>", font = list(size = 20),
                          y = 0.99, 
                          x = 0.51, 
                          xanchor = 'center', 
                          yanchor =  'top'),
             xaxis = list(title = list(text = "<b>Łączna liczba wysłanych emotek", standoff = 15, font = list(size = 15)),showgrid = TRUE,
                          gridcolor = "lightgrey"),
             yaxis = list(title = list(text = "<b>Emotki</b>", standoff = 15, font = list(size = 15)), tickfont = list(size = 13)),
             showlegend = FALSE,
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)") %>%
      animation_opts(150, redraw = TRUE) %>%
      animation_slider(currentvalue = 
                         list(prefix = "Miesiąc: ", font = list(color="red")))
    
  })
  
  
  ### tworzenie dlugosciWiadomosci Zosi
  output$dlugosciWiadomosci_plot <- renderPlotly({
    updateData4()
    
    chosen_app <- case_when(identical(app_main(),"mg") ~ " na Messengerze",
                            identical(app_main(),"ig") ~ " na Instagramie",
                            TRUE ~ " w obu aplikacjach")
    
    chosen_person <- case_when(identical(person_main(),c("a","z","f")) ~ "dkdsdsmklkmlkmldskmldskmldskmlads",
                               person_main() == "a" ~ "Anię",
                               person_main() == "z" ~ "Zosię",
                               person_main() == "f" ~ "Filipa")
    
    if (all(person_main() == c("a", "z", "f"))) {
      chosen_color <- c("orange","darkgreen", "#FF007F")
    } else {
      chosen_color <- case_when(
        person_main() == "a" ~ c("orange", "orange"),
        person_main() == "z" ~ c("#FF007F", "#FF007F"),
        person_main() == "f" ~ c("darkgreen", "darkgreen"))
    }
    
    plot_title <- paste0("<b>",
                         "Rozkład długości wiadomości",
                         " wysłanych przez ",
                         chosen_person,
                         chosen_app,
                         "</b>")
    
    title_all <- paste0("<b>",
                        "Porównanie długości wiadomości wysłanych przez nas",
                        chosen_app,
                        "</b>")
    
    
    box_data <- filter_outliers(dlugosciWiadomosciPlot$data)
    if (length(person_main()) > 1) {
      basic_plot <- plot_ly(box_data, y = ~MessageLength, type = "violin", color = ~person, colors = chosen_color,showlegend = FALSE) %>%
        layout(title = list(text = title_all, font = list(size = 20),
                            y = 0.97, 
                            x = 0.51, 
                            xanchor = 'center', 
                            yanchor =  'top'),
               xaxis = list(
                 tickvals = c("a", "f", "z"),
                 ticktext = c("<br><b>Ania</b>", "<br><b>Filip</b>","<br><b>Zosia</b>"),
                 tickfont = list(size = 15,
                                 color = "black",
                                 thickness = 3)),
               yaxis = list(title = list(text = "<b>Długość wiadomości (liczba znaków)</b>", standoff = 15, font = list(size = 13.5)),
                            range = c(0, max(box_data$MessageLength)+10),
                            showgrid = TRUE,
                            gridcolor = "lightgrey"
               ),
               plot_bgcolor = "rgba(0,0,0,0)",
               paper_bgcolor = "rgba(0,0,0,0)",
               hoverlabel = list(
                 bgcolor = "white",  
                 font = list(size = 14, 
                             color = "black"))
        )
      
    } else {
      basic_plot <- plot_ly(box_data, y = ~MessageLength, type = "violin", color = ~GroupOrPriv,colors = chosen_color, showlegend = FALSE) %>%
        layout(title = list(text = plot_title, font = list(size = 20),
                            y = 0.97, 
                            x = 0.51, 
                            xanchor = 'center', 
                            yanchor =  'top'),
               xaxis = list(
                 tickvals = c("priv", "group"),
                 ticktext = c("<br><b>konwersacje prywatne</b>", "<br><b>konwersacje grupowe</b>"),
                 tickfont = list(size = 15,
                                 color = "black",
                                 thickness = 3)),
               yaxis = list(title = list(text = "<b>Długość wiadomości (liczba znaków)</b>", standoff = 15, font = list(size = 13.5)),
                            range = c(0, max(box_data$MessageLength) + 10),
                            showgrid = TRUE,
                            gridcolor = "lightgrey"
               ),
               plot_bgcolor = "rgba(0,0,0,0)",
               paper_bgcolor = "rgba(0,0,0,0)",
               hoverlabel = list(
                 bgcolor = "white",  
                 font = list(size = 14, 
                             color = "black")  
               ))
      
    }

    basic_plot

  })


  ### tworzenie friendsPlot 
  output$friends_plot <- renderPlotly({
    friendsPlot_data %>%
      group_by(person, date) %>%
      summarise(liczba_znajomych = n()) %>%
      mutate(sumaryczna_liczba_znajomych = cumsum(liczba_znajomych),
             names = case_when(person == "a" ~ "Ania",
                               person == "f" ~ "Filip",
                               person == "z" ~ "Zosia"),
             odmienione = case_when(person == "a" ~ "Ani",
                                    person == "f" ~ "Filipa",
                                    person == "z" ~ "Zosi")
             ) %>%
      plot_ly(x = ~date, 
              y = ~sumaryczna_liczba_znajomych, 
              color = ~person, 
              type = "scatter", 
              mode = "lines",
              colors = c("orange","darkgreen", "#FF007F"),
              name = ~names, 
              hoverinfo = "text",
              hovertext = ~paste0(format(date, "%d.%m.%Y"), 
                                  " roku liczba znajomych ",
                                   odmienione,
                                   " wynosiła <b>",
                                 sumaryczna_liczba_znajomych,
                                  "</b>"),
              line = list(width = 3)
              ) %>%
      layout(
        showlegend = TRUE,
        legend = list(title = list(text = "<b>Osoby</b>")),  # Tytuł legendy
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        title = list(text = "<b>Liczba znajomych na Facebooku w czasie</b>",
                     font = list(size = 20),
                     y = 0.99, 
                     x = 0.51, 
                     xanchor = 'center', 
                     yanchor =  'top'),
        xaxis = list(rangeslider = list(type = "date"), 
                     title = list(text = "<b>Data</b>",
                                  standoff = 15),
                     showgrid = TRUE,
                     gridcolor = "lightgrey"
                     ),
        yaxis = list(title = list(text = "<b>Liczba znajomych</b>", 
                                  standoff = 15, 
                                  font = list(size = 13.5)),
                     showgrid = TRUE,
                     gridcolor = "lightgrey",
                     fixedrange = TRUE
                     )
      )
  })
  
    
  ################# tworzenie wykresów koniec ################
  
  
  ################# tworzenie tytulu do emojiPlot ################
  output$emoji_plot_title <- renderText({
    chosen_app <- case_when(identical(app_main(),"mg") ~ " na Messengerze",
                            identical(app_main(),"ig") ~ " na Instagramie",
                            TRUE ~ " w obu aplikacjach")
    
    chosen_person <- case_when(person_main() == "a" ~ "Anię",
                               person_main() == "z" ~ "Zosię",
                               person_main() == "f" ~ "Filipa")
    
    paste0("Najczęściej wysyłane emotki przez ",
      chosen_person,
      " ",
      chosen_app
    )
    
  })
  ################# tworzenie tytulu do heatmapy koniec ################
  
  
  
  ################# tworzenie tytulow konwersacji ################
  
  observe({
    if (all(person_main() == c("a", "z", "f"))) {
      person <- "Razem"
    } else {
      person <- case_when(person_main() == "a" ~ "Ania",
                          person_main() == "z" ~ "Zosia",
                          person_main() == "f" ~ "Filip",
                          TRUE ~ "Razem")
    }
    output$person_main <- renderText(person)
    output$person_main2 <-renderText(person)
    output$person_main3 <- renderText(person)
    output$person_main4 <- renderText(person)
    output$person_main5 <- renderText("Razem")
  })
  
  ################# tworzenie tytulow konwersacji koniec ################
  
  
  ################# tworzenie zdjec do tytulow konwersacji ################
  
  observe({
    
    if (all(person_main() == c("a", "z", "f"))) {
      link <- "cat_all.jpg"
    } else {
      link <- case_when(person_main() == "a" ~ "cat_a.jpg",
                        person_main() == "z" ~ "cat_z.jpg",
                        person_main() == "f" ~ "cat_f.jpg")
    }
    image <- list(src = file.path(".", "www", link), 
         alt = "im")
    
    output$person_title_im <- renderImage({image}, 
    deleteFile = FALSE)
    
    output$person_title_im2 <- renderImage({image}, 
    deleteFile = FALSE)
    
    output$person_title_im3 <- renderImage({image}, 
    deleteFile = FALSE)
    
    output$person_title_im4 <- renderImage({image}, 
    deleteFile = FALSE)
    
    output$person_title_im5 <- renderImage({list(src = file.path(".", "www", "cat_all.jpg"), 
                                                 alt = "im")}, 
    deleteFile = FALSE)
    
  })
  
  ################# tworzenie zdjec do tytulow konwersacji koniec ################
  
  
  ################# tworzenie zdjec do wiadomosci ################
  observe({
    
    if (all(person_main() == c("a", "z", "f"))) {
      link <- "cat_all.jpg"
    } else {
      link <- case_when(person_main() == "a" ~ "cat_a.jpg",
                        person_main() == "z" ~ "cat_z.jpg",
                        person_main() == "f" ~ "cat_f.jpg")
    }
    
    image <- list(src = file.path(".", "www", link), 
                  alt = "im")
    
    output$person_message_im <- renderImage({image}, 
                                          deleteFile = FALSE)
    
    output$person_message_im2 <- renderImage({image}, 
                                           deleteFile = FALSE)
    
    output$person_message_im3 <- renderImage({image}, 
                                           deleteFile = FALSE)
    
    output$person_message_im4 <- renderImage({image}, 
                                           deleteFile = FALSE)
    
    output$person_message_im5 <- renderImage({image}, 
                                           deleteFile = FALSE)
    
    output$person_message_im6 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im7 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im8 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im9 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im10 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im11 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im12 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im13 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im14 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im15 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im16 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im17 <- renderImage({list(src = file.path(".", "www", "cat_all.jpg"), 
                                                    alt = "im")}, 
                                             deleteFile = FALSE)
    
    output$person_message_im18 <- renderImage({list(src = file.path(".", "www", "cat_all.jpg"), 
                                                    alt = "im")}, 
                                             deleteFile = FALSE)
    
    output$person_message_im19 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im20 <- renderImage({image}, 
                                             deleteFile = FALSE)
    
    output$person_message_im21 <- renderImage({image}, 
                                              deleteFile = FALSE)
    
  })
  ################# tworzenie zdjec do wiadomosci koniec ################
  
  
  ################# tworzenie tekstow ################
  ### tworzenie tekstu do heatmapy
  output$heatmapa_text1 <- renderText({
    
    person <- case_when(person_main() == "a" ~ "Ania",
                        person_main() == "z" ~ "Zosia",
                        person_main() == "f" ~ "Filip")
    sex <- case_when(person_main() == "f" ~ "e",
                     TRUE ~ "a")
    
    chosen_app <- case_when(identical(app_main(),"mg") ~ " na Messengerze",
                            identical(app_main(),"ig") ~ " na Instagramie",
                            identical(app_main(),"sp") ~ " na Snapchacie",
                            TRUE ~ " na Messengerze, Instagramie i Snapchacie łącznie")
    
    paste0("Hej ",
           person,
           ", ciekawi mnie, którego dnia w ",
           input$input_year,
           " roku wysłał", sex, "ś i odebrał", sex, "ś najwięcej wiadomości",
           chosen_app)
  })
  
  
  ### tworzenie tekstów do strony emojiPlot
  output$emojiPlot_text1 <- renderText({
    
    person <- case_when(person_main() == "a" ~ "Ania",
                        person_main() == "z" ~ "Zosia",
                        person_main() == "f" ~ "Filip")
    
    paste0("Ej ",
           person,
           ", jakich emotek najczęściej używasz? 🤔")
  })
  
  output$emojiPlot_text2 <- renderText({"Cały czas używasz tych samych emotek? Jak to się rozkłada w czasie? 📅"})
  
  
  ### tworzenie tekstow do strony linePlotu
  output$linePlot_text1 <- renderText({
    
    person <- case_when(person_main() == "a" ~ "Ania",
                        person_main() == "z" ~ "Zosia",
                        person_main() == "f" ~ "Filip")
    sex <- case_when(person_main() == "f" ~ "e",
                     TRUE ~ "a")
    
    chosen_app <- case_when(identical(app_main(),"mg") ~ " na Messengerze",
                            identical(app_main(),"ig") ~ " na Instagramie",
                            identical(app_main(),"sp") ~ " na Snapchacie",
                            TRUE ~ " na Messengerze, Instagramie i Snapchacie")
    
    paste0("Hej ",
           person,
           ", ciekawi mnie w jakich okresach czasu wysyłał", sex, "ś i odbierał", sex, "ś najwięcej wiadomości",
           chosen_app)
  })
  
  output$linePlot_text2 <- renderText({
    case_when((identical(app_main(),"ig") && identical(person_main(),"a")) ~ "Dlaczego w sierpniu 2021r. zaczęłaś wymieniać tak dużo wiadomości na Instagramie?",
              (identical(app_main(),"sp") && identical(person_main(),"a")) ~ "Dlaczego w sierpniu 2021r. zaczęłaś wymieniać tak dużo wiadomości na Snapchacie?",
              (identical(app_main(),c("mg","sp","ig")) && identical(person_main(),"a")) ~ "Dlaczego dane dla Instagrama i Snapchata nie zaczynają się od początku wykresu?",
              (identical(app_main(),"mg") && identical(person_main(),"z")) ~ "Dlaczego pod koniec 2022 r. zaczęłaś wymieniać znacznie więcej wiadomości na Messengerze niż wcześniej?",
              (identical(app_main(),"ig") && identical(person_main(),"z")) ~ "Dlaczego w 2019r. zaczęłaś wymieniać więcej wiadomości na Instagramie?",
              (identical(app_main(),"sp") && identical(person_main(),"z")) ~ "Dlaczego w 2019r. zaczęłaś wymieniać więcej wiadomości na Snapchacie?",
              (identical(app_main(),c("mg","sp","ig")) && identical(person_main(),"z")) ~ "Widać, że do 2018r. i po czerwcu 2022r. najwięcej korzystałaś w Messengera, a pomiędzy 2018r. a 2020r. dużo wiadomości wymieniałaś na Snapchacie i Instagramie. Od 2020r. przez jakiś czas mniej pisałaś z ludźmi. Czy masz pomysł, dlaczego tak było?",
              (identical(app_main(),"mg") && identical(person_main(),"f")) ~ "Na początku 2019r. i 2022r. liczba wymienianych przez Ciebie wiadomości wzrosła. Czy domyślasz się, dlaczego tak mogło się zdarzyć?",
              (identical(app_main(),"sp") && identical(person_main(),"f")) ~ "Na początku sierpnia wymieniłeś na Snapchacie prawie 2 tysiace wiadomości w 3 dni? Jak to możliwe?",
              (identical(app_main(),"ig") && identical(person_main(),"f")) ~ "W kwietniu 2020r. i  w wakacje w 2021r. widać nagły wzrost w liczbie wymienianych przez Ciebie wiadomości na Instagramie. Coś szczególnego wtedy się stało?",
              TRUE ~"Dziękuję:)")
    
  })
  
  output$linePlot_text2_answer <- renderText({
    case_when((identical(app_main(),"ig") && identical(person_main(),"a")) ~ "Wyjechałam wtedy na wymianę do Niemiec, gdzie poznałam dużo osób z państw, w których młode osoby używają głównie Instagrama i Snapchata do komunikacji. Dlatego ja tez zaczęłam z nich korzystać, pisząc z tymi osobami 🤸🏻‍♀️",
              (identical(app_main(),"sp") && identical(person_main(),"a")) ~ "Wyjechałam wtedy na wymianę do Niemiec, gdzie poznałam dużo osób z państw, w których młode osoby używają głównie Instagrama i Snapchata do komunikacji. Dlatego ja tez zaczęłam z nich korzystać, pisząc z tymi osobami 🤸🏻‍♀️",
              (identical(app_main(),c("mg","sp","ig")) && identical(person_main(),"a")) ~ "Na tych aplikacjach założyłam konto później niż na Facebooku, dopiero w 2017 roku, dlatego nie ma danych dla tych aplikacji z wcześniejszych lat 😇",
              (identical(app_main(),"mg") && identical(person_main(),"z")) ~ "Wtedy poszłam na studia, poznałam dużo fajnych nowych ludzi i z niektórymi cały czas utrzymuje bliski kontakt, plus pewnie sporo wiadomości odebranych jest ze studenckich groupchatów, samouczki z algebry same się przecież nie zrobią",
              (identical(app_main(),"ig") && identical(person_main(),"z")) ~ "W połowie 2018 poszłam do międzynarodowego liceum, gdzie większość osób korzystała właśnie ze Snapchata lub Instagrama, messenger nie był tam za bardzo popularny",
              (identical(app_main(),"sp") && identical(person_main(),"z")) ~ "W połowie 2018 poszłam do międzynarodowego liceum, gdzie większość osób korzystała właśnie ze Snapchata lub Instagrama, messenger nie był tam za bardzo popularny",
              (identical(app_main(),c("mg","sp","ig")) && identical(person_main(),"z")) ~ "Od końca 2017 roku byłam w Belgii, chociaż na początku większość czasu spędzałam nadal wśród Polaków, z czasem coraz więcej moich znajomych była spoza Polski i najwięcej korzystali właśnie ze Snapchata i Instagrama, Messenger nie był tam zbyt popularną formą komunikacji. W 2020 wiadomo, COVID, ograniczone kontakty, pisało się mniej",
              (identical(app_main(),"mg") && identical(person_main(),"f")) ~ "Wiem, ale nie powiem 😶",
              (identical(app_main(),"sp") && identical(person_main(),"f")) ~ "Byłem wtedy na wakacjach, może dlatego ¯\\_(ツ)_/¯",
              (identical(app_main(),"ig") && identical(person_main(),"f")) ~ "Kwiecień to pewnie kwestia Covida i siedzenia w domu",
              identical(person_main(),"a") ~ "Nie ma sprawy, miłego dnia ❤️",
              TRUE ~"Nie ma sprawy, miłego dnia")
    
  })
  
  ################# tworzenie tekstow koniec ################
  
  
  
  ################# tworzenie odpowiedzi ################
  
  ### tworzenie odpowiedzi do dlugosciWiadomosci Zosi
  observe({
    
    if (all(person_main() == c("a", "z", "f"))) {
      example_message <- case_when(identical(app_main(),"mg") ~ "Każdy grzyb ma swój dom",
                                   identical(app_main(),"ig") ~ "co tu ma być? nie ogarniam",
                                   TRUE ~ "bardzo piękny projekt!!")
      
    } else {
      example_message <- case_when(
        person_main() == "a" ~ case_when(identical(app_main(),"mg") ~ "Zrobiłeś już projekt??",
                                         identical(app_main(),"ig") ~ "Umiesz coś na kolosa??",
                                         TRUE ~ "Wyślesz notatki z twd?"),
        person_main() == "z" ~ case_when(identical(app_main(),"mg") ~ "ughhh dlaczego ja muszę to robić",
                                         identical(app_main(),"ig") ~ "ale słodziutki kotek miau miau",
                                         TRUE ~ "ej znalazłam świniaka latającego"),
        person_main() == "f" ~ case_when(identical(app_main(),"mg") ~ "koty potrzebują piwnic",
                                         identical(app_main(),"ig") ~ "yyyy eeee array lista",
                                         TRUE ~ "Bardzo lubię małe koty"))
    }
    
    stats_data <- dlugosciWiadomosciPlot$data
    average_length <- mean(stats_data$MessageLength)
    shortest_message <- stats_data[which.min(stats_data$MessageLength), c("MessageLength", "app")]
    longest_message <- stats_data[which.max( stats_data$MessageLength), c("MessageLength", "app")]
    total_mg <- sum(stats_data$app == "mg")
    total_in <- sum(stats_data$app == "ig")
    total_group <- sum(stats_data$GroupOrPriv == "group")
    total_priv <- sum(stats_data$GroupOrPriv == "priv")

    #"Ile wynosi twoja liczba wysłanych wiadomości?"
    output$dlugosciWiadomosci_text2 <- renderText({
      paste("Ogólnie liczba wysłanych wiadomości wynosi ", (total_mg + total_in))
    })
    
    #"Jak to się rozkłada na rodzaje konwersacji?"
    output$dlugosciWiadomosci_text3 <- renderText({
      paste("Na konwersacjach grupowych w sumie mam wysłane ", total_group, " wiadomości")
    })
    
    #"A na konwersacjach prywatnych?"
    output$dlugosciWiadomosci_text4 <- renderText({
      paste("Na konwersacjach prywatnych ", total_priv)
    })
    
    #"Jaka jest średnia długość twojej wiadomości?"
    output$dlugosciWiadomosci_text5 <- renderText({
      paste("Średnia wiadomość ma u mnie ", round(average_length, 2), " znaków")
    })
    
    #"Nie wiem za bardzo co tu napisać ale coś może by się przydało"
    output$dlugosciWiadomosci_text6 <- renderText({
      paste("Jasne, oto przykładowa wiadomość: ")
    }) 
    
    output$dlugosciWiadomosci_text7 <- renderText({
      paste(example_message)
    })
    
    #Jaka jest najdłuższa wysłana przez ciebie wiadomość?
    output$dlugosciWiadomosci_text8 <- renderText({
      HTML(paste("Najdłuższa wysłana przeze mnie wiadomość miała ", longest_message$MessageLength, " znaków"))
      
    })
    
    
  })
  
  ################# tworzenie odpowiedzi koniec ################
  
  
  
}


# Zapinamy pasy i lecimy 
shinyApp(ui = ui_main, server = server)
