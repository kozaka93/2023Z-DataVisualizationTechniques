library(shiny)
library(ggplot2)
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(patchwork)
library(shinyjs)
library(bslib)
library(DT)
library(htmltools)
library(waffle)
library(shinydashboard)
library(shinydashboardPlus)
library(ggimage)
library(ggwaffle)
library(emojifont)
library(fontawesome)
library(showtext)

font_add("Bellota", "C:\\Users\\natal\\Desktop\\SEM_3\\TWD\\PRO2\\Bellota.otf")
showtext_auto()


df_wszyscy<- read.csv("Wszyscy.csv")

df_Ula<- read.csv("Ula.csv")
df_Kasia <- read.csv("Kasia.csv")
df_Natalka<- read.csv("Natalka.csv")

df_wszyscy$czas.snu<- na.omit(as.numeric(gsub(",",".",df_wszyscy$czas.snu)))
df_Ula$czas.snu<- na.omit(as.numeric(gsub(",",".",df_Ula$czas.snu)))
df_Kasia$czas.snu<- na.omit(as.numeric(gsub(",",".",df_Kasia$czas.snu)))
df_Natalka$czas.snu<- na.omit(as.numeric(gsub(",",".",df_Natalka$czas.snu)))

df_Natalka %>% 
  mutate(czy.dużo.czasu.na.powietrzu = case_when(
    czy.dużo.czasu.na.powietrzu == 'FALSE'~'FAŁSZ',
    czy.dużo.czasu.na.powietrzu == 'TRUE'~'PRAWDA'
  )) %>% 
  mutate(czy.telefon.przed.snem = case_when(
    czy.telefon.przed.snem == 'FALSE'~'FAŁSZ',
    czy.telefon.przed.snem == 'TRUE'~'PRAWDA'
  ))
df_Kasia%>% 
  mutate(czy.dużo.czasu.na.powietrzu = case_when(
    czy.dużo.czasu.na.powietrzu == 'FALSE'~'FAŁSZ',
    czy.dużo.czasu.na.powietrzu == 'TRUE'~'PRAWDA'
  )) %>% 
  mutate(czy.telefon.przed.snem = case_when(
    czy.telefon.przed.snem == 'FALSE'~'FAŁSZ',
    czy.telefon.przed.snem == 'TRUE'~'PRAWDA'
  ))
df_Ula%>% 
  mutate(czy.dużo.czasu.na.powietrzu = case_when(
    czy.dużo.czasu.na.powietrzu == 'FALSE'~'FAŁSZ',
    czy.dużo.czasu.na.powietrzu == 'TRUE'~'PRAWDA'
  )) %>% 
  mutate(czy.telefon.przed.snem = case_when(
    czy.telefon.przed.snem == 'FALSE'~'FAŁSZ',
    czy.telefon.przed.snem == 'TRUE'~'PRAWDA'
  ))
df_wszyscy%>% 
  mutate(czy.dużo.czasu.na.powietrzu = case_when(
    czy.dużo.czasu.na.powietrzu == 'FALSE'~'FAŁSZ',
    czy.dużo.czasu.na.powietrzu == 'TRUE'~'PRAWDA'
  )) %>% 
  mutate(czy.telefon.przed.snem = case_when(
    czy.telefon.przed.snem == 'FALSE'~'FAŁSZ',
    czy.telefon.przed.snem == 'TRUE'~'PRAWDA'
  ))



ui <- fluidPage(
  tags$head(
    tags$style(
      
      HTML("body { background-color: #000526; }"),
      HTML(".shiny-input-container input, .shiny-input-container select, .shiny-input-container textarea { color: white !important; }"),
      HTML(".shiny-input-container label { color: white !important; }"),
      HTML(".shiny-text-output { color: white !important; }"),
      HTML("
      .custom-column-1 {
        width: 27%;
        margin-right: 1%; 
        margin-left: 1%; 
        
      }
      .custom-column-2 {
        width: 60%;
        margin-right: 0%; 
        margin-left: 0%; 
      }
      .custom-column-3 {
        width: 8%;
        margin-right: 1%; 
        margin-left: 1%; 
      }"),
      
      HTML("
           .custom-column-4{
           width: 22%;}
           .custom-column-5{
           width: 78%;}"),
      HTML("
           .custom-column-6{
           width: 20.5%;}
           .custom-column-7{
           width: 79.5%;}
           .custom-column-8{
           width: 23%;
           margin-top: 0%;
           margin-right: 0%;
           align-items: center;}
           .custom-column-10{
           width: 20%;
           align-items: center;
           }
           .custom-column-9{
           width: 50%;
           margin-left: 2%;
           margin-top: 3%;}"),
      
      HTML("
      .custom-column-3 label {
        font-size: 10px;}"),
      HTML("
      .custom-column-1.custom-text {
        font-family: 'Bellota', sans-serif; 
        font-size: 15px;
        color: 'white'!important;
      }"),
      HTML("
           .custom-column-obraz{
           width: 20%;
           margin-left: 0%;
           margin-top: 1%;}"),
      HTML("
    .lower-select {
      margin-top: 63px; 
    }
  "),
  HTML(".text1 {color: red; font-family: Bellota; font-size: 30px;}"),
  HTML(".text2 {color: red; font-family: Bellota; font-size: 17px;}")
  
  
    )),
  
  
  theme = bs_theme(
    bg = "#000526",
    fg = "white",
    primary = "#c6aedc",
    base_color = "#000526",
    lightness = 0
  ),
  
  
  navbarPage(
    title="Atlas snu:",
    theme = "bootstrap.css",
    tags$head(
      tags$style(HTML("
        .navbar-default
        .navbar-inner
        {
        background-color: #000526;
      }
        .navbar-default .navbar-brand,
        .navbar-default .navbar-nav > li > a {
        font-family: 'Bellota';
        font-size: 24px;
        color: #c6aedc;
        }
        "))),
    
    
    tabPanel("Czas snu",
             fluidRow(
               
               div(class="custom-column-4", selectInput("spioch",
                                                        label = HTML("<span style='font-family: Bellota; font-size: 17px;'> Dla której osoby pokazać wykresy?:</span>"),
                                                        c("Wszyscy", "Śpioch 1","Śpioch 2","Śpioch 3"))),
              
               
             ),
             fluidRow(
               div(img(src="o6.png",width="100%",deleteFile =FALSE))  
             ),
             
             fluidRow(div(style="height: 30px")),
             fluidRow(
               
               div(class = "custom-column-1 custom-text", htmlOutput("podsumowanie")),
               div(class = "custom-column-2", plotlyOutput("density")),
               div(class = "custom-column-3",checkboxGroupInput("dzien", label = HTML("<span style='font-family: Bellota; font-size: 14px;'> Dzień:</span>"),c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela"),selected = c("sobota","niedziela")))
             ),
             fluidRow(div(style="height: 70px")),
             
             fluidRow(
               div(class="custom-column-8",HTML('<img src="k.png" height="169px" width="240px">'),
                   htmlOutput("godzinySpania")),
               div(class="custom-column-10",HTML('<img src="s.png" height="169px" width="198px">'),htmlOutput("godzinyWstania")),
               div(class="custom-column-9", htmlOutput("ciekawostki"))),
             
             fluidRow(div(style="height: 50px")),
             fluidRow(
               div(class="custom-column-4 lower-select",selectInput("stopien", label = HTML("<span style='font-family: Bellota; font-size: 17px;'> Dla którego stopnia wyspania pokazać wykresy?:</span>"),
                                                                    c("Niewyspany","Średnio","Wyspany", "Wszystkie"))),
               div(class="custom-column-5",plotlyOutput("histogram_spania",height = "250px",width="1146px"))
               
             ),
             fluidRow(
               div(class="custom-column-6",plotlyOutput("histogram_wstania"), style="margin-right: 0%;"),
               div(class="custom-column-7",plotlyOutput("punktowy"))
             ),
             fluidRow(div(style="height: 30px"))
    ),
    
    tabPanel("Czynniki zewnętrzne",
             fluidRow(column( 3,
                              selectInput("selectedFrame",
                                          label = HTML("<span style='font-family: Bellota; font-size: 17px;'> Dla której osoby pokazać wykresy?</span>"),
                                          choices = c("Wszyscy", "Śpioch 1", "Śpioch 2", "Śpioch 3"))
             )),
             fluidRow(
               div(style="height: 15px"),
               HTML('<img src="tytuł_czynniki2.png" height="280px" style="float:right;"/>'),
               
               column(3,
                      selectInput("selectedFactor",
                                  label = HTML("<span style='font-family: Bellota; font-size: 17px;'> Wybierz czynnik do analizy </span>"),
                                  choices = c("Męczący dzień", "Telefon przed snem",
                                              "Czas na powietrzu"))
               ),
               column(3,
                      selectInput("selectedPlot",
                                  label = HTML("<span style='font-family: Bellota; font-size: 17px;'> Wybierz typ wykresu </span>"),
                                  choices = c("Wykres punktowy", "Wykres wiolinowy"))),
             ),
             fluidRow(div(style="height: 20px")),
             
             fluidRow(
               column(8,
                      div(style="height: 15px"),
                      plotlyOutput("pointPlot")
               ),
               
               
               
               column(4,
                      dataTableOutput("meanTable")
                      
               )),
             fluidRow(
               HTML('<img src="tło_czynniki2.png" height="280px" style="float:right;"/>'),
               column(8,
                      plotlyOutput("barPlot") 
                      
               ), 
               
               column(4,
                      div(style="height: 50px"),
                      div(class = "text1", textOutput("text1")),
                      div(class = "text2", textOutput("text2")),
                      div(style="height: 5px"),
                      div(class = "text2", textOutput("text3")))),
             
             
             fluidRow(div(style="height: 50px"))
    ),
    tabPanel("Sny",
             fluidRow(
               column(3, 
                      selectInput("dataset",
                                  label = HTML("<span style='font-family: Bellota; font-size: 17px;'> Dla której osoby pokazać wykresy?:</span>"),
                                  choices = c("Wszyscy", "Śpioch 1", "Śpioch 2", "Śpioch 3")))
             ),
             
             #PASEK GÓRNY
             fluidRow(
               column(
                 width = 4,
                 img(src = "gwiazdeczki.png", width = "100%")
               ),
               column(
                 width = 4,
                 align = "center",
                 p(
                   HTML("O czym śnimy?"),
                   style = "font-size: 48px; font-family: 'Bellota'; color: white;"
                 ),
                 fluidRow(div(valueBoxOutput(width = NULL,"infoBox1"))),
                 fluidRow(div(valueBoxOutput(width = NULL,"infoBox2")))
               ),
               column(
                 width = 4,
                 img(src = "gwiazdeczki2.png", width = "100%")
               )),
             
             
             fluidRow(div(style="height: 30px")),
             
             
             #PASEK DOLNY
             fluidRow(
               column(5,
                      #STRONA LEWA
                      fluidRow(
                        align = "center",
                        p(
                          HTML("Kolorowe sny"),
                          style = "font-size: 30px; font-family: 'Bellota'; color: white;"
                        )
                      ),
                      fluidRow(
                        align = "center",
                        p(
                          HTML("Nie wszyscy śnią w kolorze. Niewielki procent ludzkości ma czarno-białe sny. <br> Co ciekawe, przed wynalezieniem telewizji w kolorze,<br> ich odsetek był znacznie większy."),
                          style = "font-size: 16px; font-family: 'Bellota'; color: white;"
                        )
                      ),
                      fluidRow(div(style="height: 30px")),
                      
                      align = "center",
                      h2(
                        HTML("Jakie mamy sny?"),
                        style = "font-size: 25px; font-family: 'Bellota'; color: white;"
                      ),
                      plotOutput("waffleChart")
               ),
               
               #STRONA LEWA
               column(1,
                      fluidRow(div(style="height: 250px")),
                      img(src = "legenda.png", width = "120px",alt = "eee")
                      
               ),
               
               #STRONA PRAWA
               column(6,
                      align = "center",
                      h2(
                        HTML("Stopień wyspania, a rodzaj snu"),
                        style = "font-size: 25px; font-family: 'Bellota'; color: white;"
                      ),
                      plotlyOutput("bubble_chart"),
                      fluidRow(div(style="height: 30px")),
                      fluidRow(
                        align = "center",
                        p(
                          HTML("Czy wiesz że?"),
                          style = "font-size: 30px; font-family: 'Bellota'; color: white;"
                        )
                      ),
                      fluidRow(
                        align = "center",
                        p(
                          HTML("Nigdy nie śnimy o osobach, których nie znamy. Mózg nie wymyśla nowych twarzy. Ci, którzy pojawiają się w snach są ludźmi, których kiedyś zobaczyliśmy, nawet jeśli tylko przelotnie."),
                          style = "font-size: 16px; font-family: 'Bellota'; color: white;"
                        )
                      )
               )))))






server <- function(input, output) {
  # wybór osoby
  selected_person<- reactive({
    switch (input$spioch,
            "Wszyscy" = df_wszyscy,
            "Śpioch 1" = df_Ula,
            "Śpioch 2" = df_Kasia,
            "Śpioch 3" = df_Natalka
    )
  })
  
  # wybór czynnika
  selected_stopien <- reactive({
    switch(input$stopien,
           "Niewyspany" = selected_person() %>% filter(selected_person()$stopień.wyspania==1) %>%
             group_by(godzina.pójścia.spać,godzina.wstania) %>% 
             summarise(count=n()),
           "Średnio" = selected_person() %>% filter(selected_person()$stopień.wyspania==2) %>%
             group_by(godzina.pójścia.spać,godzina.wstania) %>% 
             summarise(count=n()),
           "Wyspany" = selected_person() %>% filter(selected_person()$stopień.wyspania==3) %>%
             group_by(godzina.pójścia.spać,godzina.wstania) %>% 
             summarise(count=n()),
           "Wszystkie" = selected_person() %>%
             group_by(godzina.pójścia.spać,godzina.wstania) %>% 
             summarise(count=n()))
  })
  
  #Text pod księżycem strona czas
  output$godzinySpania<-renderText({
    
    ramka<- selected_person()
    osoba<- input$spioch
    table_g2<- table(na.omit(ramka$godzina.pójścia.spać))
    pojscia<- names(table_g2)[which.max(table_g2)]
    
    paste( "<div style='font-family: Bellota;'>",
           "<span style='color: white;font-size: 18px;'>",osoba, " najczęściej chodził spać o","<br>",
           "<span style='margin-left: 73px;'></span>",
           "<span style='color: white;font-size: 37px;'>",pojscia,"</span>",
           "</div>")->tx
    
    paste( "<div style='font-family: Bellota;'>",
           "<span style='color: white;font-size: 18px;'>", " Najczęściej chodziłyśmy spać o","<br>",
           "<span style='margin-left: 73px;'></span>",
           "<span style='color: white;font-size: 37px;'>",pojscia,"</span>",
           "</div>")->tx_all
    if(input$spioch=="Wszyscy"){
      HTML(paste("<span style='color: white;'>", tx_all, "</span>"))
    }else{
      HTML(paste("<span style='color: white;'>", tx, "</span>"))}
  })
  
  #Tekst pod słońcem strona czas
  output$godzinyWstania<-renderText({
    
    ramka<- selected_person()
    osoba<- input$spioch
    table_g2<- table(na.omit(ramka$godzina.wstania))
    wstania<- names(table_g2)[which.max(table_g2)]
    
    paste( "<div style='font-family: Bellota;'>",
           "<span style='color: white;font-size: 18px;'>",osoba, " najczęściej wstawał o","<br>",
           "<span style='margin-left: 61px;'></span>",
           "<span style='color: white;font-size: 37px;'>",wstania,"</span>",
           
           "</div>")->tx
    
    paste( "<div style='font-family: Bellota;'>",
           "<span style='color: white;font-size: 18px;'>", " Najczęściej wstawałyśmy o","<br>",
           "<span style='margin-left: 61px;'></span>",
           "<span style='color: white;font-size: 37px;'>",wstania,"</span>",
           
           "</div>")->tx_all
    
    if(input$spioch=="Wszyscy"){
      HTML(paste("<span style='color: white;'>", tx_all, "</span>"))
    }else{
      HTML(paste("<span style='color: white;'>", tx, "</span>"))}
  })
  
  #text pod gęstością strona czas
  output$ciekawostki<-renderText({
    ramka<- selected_person()
    osoba<- input$spioch
    procent<- round(sum(ramka$czas.snu>=7 & ramka$czas.snu<=9)/ nrow(ramka),2)*100
    kolor<- case_when(procent<=50 ~ "#e8635b",
                      procent<75 ~ "#f5d75c",
                      procent>=75 ~ "#88cc6c")
    
    
    paste("<div style='font-family: Bellota;'>",
          "<br>",
          "<div style='text-align: center;'>",
          "<span style='color: white;font-size: 23px;'>","Wiele badań pokazuje, że osoby w naszym wieku powinny spać od 7 do 9 godzin na dobę","</span>","<br>","<br>",
          
          paste("<span style='color:",kolor,";font-size: 44px;'>"), procent,"%","</span>","<br>",
          "<span style='color: white;font-size: 16px;'>", "Tyle procent nocy",osoba,"przespał zgodnie z zaleceniami","</span>","<br>","<br>",
          "</div>",
          "</div>")->tx
    
    paste("<div style='font-family: Bellota;'>",
          "<div style='text-align: center;'>",
          "<span style='color: white;font-size: 23px;'>","Według badań osoby w naszym wieku powinny spać od 7 do 9 godzin na dobę","</span>","<br>",
          
          paste("<span style='color:", kolor,";font-size: 40px;'>"), procent,"%","</span>","<br>",
          "<span style='color: white;font-size: 16px;'>", "Tyle procent nocy przespałyśmy zgodnie z zaleceniami","</span>","<br>","<br>",
          "</div>",
          "</div>")->tx_wszyscy
    if(input$spioch=="Wszyscy"){
      HTML(paste("<span style='color: white;'>", tx_wszyscy, "</span>"))
    }else{
      HTML(paste("<span style='color: white;'>", tx, "</span>"))}
  })
  
  
  # tekst obok gęstości strona czas  
  output$podsumowanie<- renderText({
    ramka<- selected_person()
    osoba<- input$spioch
    stopien<- selected_stopien()
    round(mean(ramka$czas.snu))->srednia
    min(ramka$czas.snu)->minim
    max(ramka$czas.snu)-> maxim
    
    paste("<div style='text-align: center;'>",
          "<span style='color: white;font-size: 23px;'>","Jak wygląda czas snu wybranego śpiocha?","</span>","<br>", "<br>",
          "<span style='color: white;font-size: 37px;'>",srednia,"h","</span>","<br>",
          "<span style='color: white;'>","Tyle wynosi średnia długość snu","</span>","<br>",
          "<span style='color: white;font-size: 37px;'>", minim,"h","</span>","<br>",
          "<span style='color: white;'>","Tyle trwał najkrótszy sen","</span>","<br>",
          "<span style='color: white;font-size: 37px;'>",maxim,"h","</span>","<br>",
          "<span style='color: white;'>","Tyle trwał najdłuższy sen","</span>","<br>",
          
          "</div>")->tx
    paste("<div style='text-align: center;'>",
          "<span style='color: white;font-size: 23px;'>","Jak wygląda czas snu wszystkich śpiochów?","</span>","<br>", "<br>",
          "<span style='color: white;font-size: 37px;'>",srednia,"h","</span>","<br>",
          "<span style='color: white;'>","Tyle wynosi średnia długość snu","</span>","<br>",
          "<span style='color: white;font-size: 37px;'>", minim,"h","</span>","<br>",
          "<span style='color: white;'>","Tyle trwał najkrótszy sen","</span>","<br>",
          "<span style='color: white;font-size: 37px;'>",maxim,"h","</span>","<br>",
          "<span style='color: white;'>","Tyle trwał najdłuższy sen","</span>","<br>",
          "</div>")->tx_wszyscy
    
    if(input$spioch=="Wszyscy"){
      HTML(paste("<span style='color: white;'>", tx_wszyscy, "</span>"))
    }else{
      HTML(paste("<span style='color: white;'>", tx, "</span>"))
    }
  })
  
  #GRESTOSC strona czas
  output$density <- renderPlotly({
    selected_person() %>% 
      filter(dzień.tygodnia %in% input$dzien)%>% 
      ggplot(aes(x=czas.snu, fill = dzień.tygodnia))+
      geom_density(alpha =0.63)+
      theme_minimal()+
      labs(x = "Czas snu", y = "gęstość", title = "Wykres gęstości czasu snu w wybranych dniach tygodnia", fill= "dzień tygodnia")+
      theme(axis.title.x = element_text(size=11, color = "white"),
            axis.title.y = element_text(size=11, color="white"),
            axis.text.x = element_text(size=7.5, color="white"),
            axis.text.y = element_text(size=7.5, color="white"),
            legend.text = element_text(color="white"),
            title = element_text(size=12, color="white"),
            plot.background = element_rect(fill="#000526"),
            panel.background = element_rect(fill="#000526"),
            panel.grid = element_line(color="grey", size=0.01),
            panel.grid.major.x = element_line(color=alpha("#3B4173",0.5)),
            panel.grid.major.y = element_line(color=alpha("#3B4173",0.5)))+
      scale_fill_manual(values = c("poniedziałek" = "#a0f0e8", "wtorek" = "#fe0606", "środa" = "#e7fe06", "czwartek" = "#04adf6", "piątek" = "#3bf604", "niedziela" = "#ffd966", "sobota" = "#f604f2"))->p1
    ggplotly(p1) %>% layout(font=list(family='Bellota'),
                            title=list(x=0.5,yanchor='top'),
                            margin=list(l=40,r=0,b=0,t=85))
  })
  
  #HISTOGRAM SPANIA strona czas
  output$histogram_spania<- renderPlotly({
    ggplot(selected_stopien())+
      geom_bar(aes(x=na.omit(godzina.pójścia.spać), text=paste("Godzina pójścia spać:",na.omit(godzina.pójścia.spać))),width = 0.5, fill = "#c6aedc")+
      theme_minimal()+
      xlim(c("22:00","22:15","22:30","22:45","23:00","23:15","23:30","23:45","00:00","00:15","00:30","00:45","01:00","01:15","01:30","01:45","02:00","02:15","02:30","02:45","03:00","03:15","03:30","03:45","04:00"))+
      scale_y_continuous(breaks = c(0,1,2,3, 4, 5, 6, 7))+
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=7.5,color = "#f9f8ef"),
        axis.text.y = element_text(size=7.5,color = "#f9f8ef"),
        axis.title.y = element_text(size=11,color="white"),
        title = element_text(size=13,color= "white"),
        plot.background = element_rect(fill="#000526"),
        panel.background = element_rect(fill="#000526"),
        panel.grid = element_line(color="grey", size=0.01),
        panel.grid.major.x = element_line(color=alpha("#1c3a68",0.3)),
        panel.grid.major.y = element_line(color=alpha("#1c3a68",0.3)))+
      labs(y="Liczba zaśnięć",title = "Wykresy zależności stopnia wyspania od godzin")->hist_pojscia
    ggplotly(hist_pojscia,tooltip="text") %>%  layout(font=list(family='Bellota'),
                                                      title=list(x=0.5,yanchor='top'),
                                                      margin = list( r = 0, t = 90, b = 0))
    
    
  })
  
  #PUNKTOWY strona czas
  output$punktowy <- renderPlotly({
    ggplot(selected_stopien())+
      geom_point(aes(x=na.omit(godzina.pójścia.spać), y=godzina.wstania,size=count, text=paste("Godzina pójścia spać:",na.omit(godzina.pójścia.spać),"\n Godzina wstania:",godzina.wstania,"\n ilość wystąpień:",count)),  color = "#FCC214",shape=11)+
      xlim(c("22:00","22:15","22:30","22:45","23:00","23:15","23:30","23:45","00:00","00:15","00:30","00:45","01:00","01:15","01:30","01:45","02:00","02:15","02:30","02:45","03:00","03:15","03:30","03:45","04:00"))+
      ylim(c("05:30","05:45","06:00","06:15","06:30","06:45","07:00","07:15","07:30","07:45","08:00","08:15","08:30","08:45","09:00","09:15","09:30","09:45","10:00","10:15","10:30","10:45","11:00","11:15","11:30","11:45","12:00"))+
      theme(axis.title.y = element_blank(),
            axis.text.x = element_text(size=7.5, color = "#f9f8ef"),
            axis.text.y = element_text(size=7.5, color = "#f9f8ef"),
            axis.title.x = element_text(size=11, color = "white"),
            legend.position = "none",
            plot.background = element_rect(fill="#000526"),
            panel.background = element_rect(fill="#000526"),
            panel.grid = element_line(color="grey", size=0.01),
            panel.grid.major.x = element_line(color=alpha("#1c3a68",0.3)),
            panel.grid.major.y = element_line(color=alpha("#1c3a68",0.3)))+
      labs(x="Godzina pójścia spać")->punktowy
    
    ggplotly(punktowy,tooltip = "text") %>%  layout(font=list(family='Bellota'), margin = list(l = 0, r = 0, t = 0, b = 40)
    )
  })
  
  #HISTOGRAM WSTANIA strona czas
  output$histogram_wstania <- renderPlotly({
    ggplot(selected_stopien())+
      geom_bar(aes(x=godzina.wstania, text=paste("Godzina wstania:",godzina.wstania)),width=0.9, fill = "#c6aedc")+
      theme_minimal()+
      coord_flip()+
      scale_y_reverse(breaks=c(0,1,2,3, 4, 5, 6, 7))+
      theme(axis.title.y = element_blank())+
      labs(y="Liczba obudzeń",
           x="Godzina wstania")+
      
      xlim(c("05:30","05:45","06:00","06:15","06:30","06:45","07:00","07:15","07:30","07:45","08:00","08:15","08:30","08:45","09:00","09:15","09:30","09:45","10:00","10:15","10:30","10:45","11:00","11:15","11:30","11:45","12:00"))+
      
      theme(axis.text.y = element_text(color="#f9f8ef", size=7.5),
            axis.title.y = element_text(size=11, color="white"),
            axis.title.x = element_text(size=11, color="white"),
            axis.text.x = element_text(size=7.5, color="white"),
            plot.background = element_rect(fill="#000526"),
            panel.background = element_rect(fill="#000526"),
            panel.grid = element_line(color="grey", size=0.01),
            panel.grid.major.x = element_line(color=alpha("#1c3a68",0.3)),
            panel.grid.major.y = element_line(color=alpha("#1c3a68",0.3)))->hist_wstania
    ggplotly(hist_wstania,tooltip = "text")%>%  layout(font=list(family='Bellota'), margin = list(l = 50, r = 0, t = 0, b = 40)
    )
    
  })
  
  # wykres punktowy
  selectedPlot <- reactive({
    switch(input$selectedPlot,
           "Wykres punktowy" = 1,
           "Wykres wiolinowy" = 2)
  })
  
  # wybór ramki danych
  selectedData <- reactive({
    switch(input$selectedFrame,
           "Wszyscy" = df_wszyscy,
           "Śpioch 1" = df_Ula,
           "Śpioch 2" = df_Kasia,
           "Śpioch 3" = df_Natalka
    )
  })
  
  # czynnik1
  Factor <- reactive({
    data <- selectedData()
    switch(input$selectedFactor,
           "Męczący dzień" = data$czy.męczący.dzień.przed.snem,
           "Telefon przed snem" = data$czy.telefon.przed.snem,
           "Czas na powietrzu" = data$czy.dużo.czasu.na.powietrzu
           
    )
  })
  
  # czynnik 2
  Factor2 <- reactive({
    switch(input$selectedFactor,
           "Męczący dzień" = 'czy.męczący.dzień.przed.snem',
           "Telefon przed snem" = 'czy.telefon.przed.snem',
           "Czas na powietrzu" = 'czy.dużo.czasu.na.powietrzu'
           
    )
  })
  
  # czynnik 3
  Factor3 <- reactive({
    switch(input$selectedFactor,
           "Męczący dzień" = 'Męczący dzień',
           "Telefon przed snem" = 'Telefon przed snem',
           "Czas na powietrzu" = 'Czas na powietrzu'
           
    )
  })
  
  # ciekawostki
  output$text1 <- renderText({
    paste("Czy wiedziałeś że?")
  })
  
  output$text2 <- renderText({
    paste("Jasne światło emitowane przez telefon w trakcie korzystnia w nocy czy wieczorem zaszczepia w mózgu błedne przekonanie, że jest dzień. Skutkuje to problemami z zapadnięciem w sen. Zalecane jest aby przynajmniej na godzine przed pójściem spać nie korzystać z telefonu.")
  })
  
  output$text3 <- renderText({
    paste("Regularne uprawianie sportu i ruchu przyczynia się do poprawy efektywności oraz głebokości jego fazy REM.")
  })
  
  # tabelka z analizą
  output$meanTable <- renderDT({
    data <- selectedData()
    czynniki <- Factor2()
    nazwy <- Factor3()
    data2 <- data %>% 
      mutate(czas.snu = as.numeric(gsub(",", ".", czas.snu)),
             stopień = factor(case_when(
               stopień.wyspania == 1 ~ "słabo",
               stopień.wyspania == 2 ~ "średnio",
               stopień.wyspania == 3 ~ "bardzo"
             ), levels = c("słabo", "średnio", "bardzo")))
    
    # Obliczanie średnich dla każdej kombinacji stopnia wyspania i czynnika
    mean_values_combination <- data2 %>%
      group_by_at(vars(stopień, !!czynniki)) %>%
      summarize(średni_sen = round(mean(czas.snu),2) )
    
    colnames(mean_values_combination) <- c("Stopień Wyspania", nazwy, "Średni Sen [h]")
    # Zwracanie ramki danych
    datatable(mean_values_combination, 
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = TRUE ,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000526', 'color': 'white'});",
                  "}")
              ),
              rownames = FALSE
    )%>% 
      formatStyle(
        c("Stopień Wyspania", nazwy, "Średni Sen [h]"), # kolumna do dostosowania
        color = 'white',  # dostosuj kolory tekstu
        
      ) 
    
  }, rownames = FALSE)
  
  # wykres punktowy i wiolinowy
  output$pointPlot <- renderPlotly({
    data <- selectedData()
    czynniki <- Factor()
    czynnik <- Factor2()
    type <- selectedPlot()
    
    
    data2 <- data %>% 
      mutate(czas.snu = as.numeric(gsub(",", ".", czas.snu)),
             stopień = factor(case_when(
               stopień.wyspania == 1 ~ "słabo",
               stopień.wyspania == 2 ~ "średnio",
               stopień.wyspania == 3 ~ "bardzo"
             ), levels = c("słabo", "średnio", "bardzo")))
    if (type == 1){
      
      gg <- ggplot(data2, aes(x = stopień, y = czas.snu, color = as.factor(czynniki))) +
        geom_point(size = 6, alpha = 0.7, position = position_jitter(0.2)) +
        theme_bw()+
        scale_color_manual(values = c( "#e8f4f8", "#7200ea", "#ffd966")) +
        labs(
          title = paste("Stopień wyspania i długości snu od czynnika: ", input$selectedFactor),
          x = "Stopień wyspania",
          y = "Długość snu [h]",
          color = input$selectedFactor
        ) +
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              legend.background = element_rect(fill = "transparent", color = NA),
              title = element_text(size=13, color="white"),
              axis.title.x = element_text(size=11, color = "white"),
              axis.title.y = element_text(size=11, color="white"),
              axis.text.x = element_text(size=10.5, color="white"),
              axis.text.y = element_text(size=10.5, color="white"),
              legend.text = element_text(color="white"),
              panel.grid = element_line(color="grey", size=0.01),
              panel.grid.major.x = element_line(color=alpha("grey",0.5)),
              panel.grid.major.y = element_line(color=alpha("grey",0.5))
        )
    }
    if (type == 2){
      gg <- ggplot(data2, aes(x = stopień, y = czas.snu, fill = as.factor(czynniki))) +
        geom_violin(position = "dodge", alpha = 0.7, width = 0.6) +
        theme_bw()+
        scale_fill_manual(values = c( "#e8f4f8", "#7200ea", "#ffd966")) +
        labs(
          title = paste("Stopień wyspania i długości snu od czynnika: ", input$selectedFactor),
          x = "Stopień wyspania",
          y = "Długość snu [h]",
          fill = input$selectedFactor
        ) +
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              legend.background = element_rect(fill = "transparent", color = NA),
              title = element_text(size=13, color="white"),
              axis.title.x = element_text(size=11, color = "white"),
              axis.title.y = element_text(size=11, color="white"),
              axis.text.x = element_text(size=10.5, color="white"),
              axis.text.y = element_text(size=10.5, color="white"),
              legend.text = element_text(color="white"),
              panel.grid = element_line(color="grey", size=0.01),
              panel.grid.major.x = element_line(color=alpha("grey",0.5)),
              panel.grid.major.y = element_line(color=alpha("grey",0.5))
        )
    }
    
    
    ggplotly(gg) %>% layout(font=list(family='Bellota'),
                            title=list(x=0.5,yanchor='top'))
  })
  
  # wykres słupkowy
  output$barPlot <- renderPlotly({
    data <- selectedData()
    czynniki <- Factor()
    
    data2 <- data %>% 
      mutate(czas.snu = as.numeric(gsub(",", ".", czas.snu)),
             stopień = factor(case_when(
               stopień.wyspania == 1 ~ "słabo",
               stopień.wyspania == 2 ~ "średnio",
               stopień.wyspania == 3 ~ "bardzo"
             ), levels = c("słabo", "średnio", "bardzo")))
    
    gg <- ggplot(data2, aes(x = stopień, fill = as.factor(czynniki))) +
      geom_bar(position = "stack", width = 0.6) +
      theme_bw() +
      scale_fill_manual(values = c("#e8f4f8", "#7200ea", "#ffd966")) +
      labs(
        x = "Stopień wyspania",
        y = "Ilość",
        title = paste("Zależność stopnia wyspania od czynnika: ", input$selectedFactor),
        fill = input$selectedFactor
      ) +
      theme(plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            legend.background = element_rect(fill = "transparent", color = NA),
            title = element_text(size=13, color="white"),
            axis.title.x = element_text(size=11, color = "white"),
            axis.title.y = element_text(size=11, color="white"),
            axis.text.x = element_text(size=9.5, color="white"),
            axis.text.y = element_text(size=9.5, color="white"),
            legend.text = element_text(color="white"),
            panel.grid = element_line(color="grey", size=0.01),
            panel.grid.major.x = element_line(color=NA),
            panel.grid.major.y = element_line(color=alpha("grey", 0.5))
      )
    ggplotly(gg) %>% layout(font=list(family='Bellota'),
                            title=list(x=0.5,yanchor='top'))
    
    
  })
  
  #WYBÓR DANYCH
  selected_data <- reactive({
    switch(input$dataset,
           "Wszyscy" = df_wszyscy,
           "Śpioch 1" = df_Ula,
           "Śpioch 2" = df_Kasia,
           "Śpioch 3" = df_Natalka)
  })
  
  #PROCENTY 1
  output$infoBox1 <- renderValueBox({
    ile_procent_sen <- round(mean(selected_data()$czy.był.sen) * 100)
    
    valueBox(
      tags$p(
        paste0(ile_procent_sen, "%"),
        style = "font-size: 175%; text-align: center; color: #FFFFFF; font-family: 'Bellota', sans-serif;"
      ),
      tags$p(
        "tak często mamy sen",
        style = "font-size: 125%; text-align: center; color: #FFFFFF; font-family: 'Bellota', sans-serif;"
      )
    )
  })
  
  #PROCENTY 2
  output$infoBox2 <- renderValueBox({
    ile_procent_pobudki <- round(mean(selected_data()$czy.pobudki.w.nocy) * 100)
    
    valueBox(
      tags$p(
        paste0(ile_procent_pobudki, "%"),
        style = "font-size: 175%; text-align: center; color: #FFFFFF; font-family: 'Bellota', sans-serif;"
      ),
      tags$p(
        "tak często budzimy się w nocy",
        style = "font-size: 125%; text-align: center; color: #FFFFFF; font-family: 'Bellota', sans-serif;"
      )
    )
  })
  
  #FUNKCJA DO WYKRESU GOFROWEGO
  prepare_waffle_data <- function(data, num_columns) {
    data$x <- rep(1:num_columns, length.out = nrow(data))
    return(data)
  }
  
  #WYKRES GOFROWY
  output$waffleChart <- renderPlot({
    
    waffle_data <- waffle_iron(prepare_waffle_data(selected_data(), 7), aes_d(group = jaki.sen))
    
    waffle_data$label = fontawesome("fa-star")
    
    ggplot(waffle_data, aes(x, y, colour = group), size = 0.8, n_rows = 7, flip = TRUE) +
      geom_text(aes(label=label), family='fontawesome-webfont', size=12) +
      
      scale_colour_manual(values = c("#FCC214", "#FCE288", "#FFFEE8", "#BEA4F0", "#7200EA")) +
      
      theme(
        axis.title.x = element_text(size=11, color = "#000526"),
        axis.title.y = element_text(size=11, color="#000526"),
        axis.text.x = element_text(size=7.5, color="#000526"),
        axis.text.y = element_text(size=7.5, color="#000526"),
        title = element_text(size = 14, color = "white", family = "Bellota", hjust = 0.5),
        legend.position = "none",
        plot.background = element_rect(fill="#000526", color = "#000526"),
        panel.background = element_rect(fill="#000526", color = "#000526"),
        panel.grid = element_line(color="grey", size=0.01),
        panel.grid.major.x = element_line(color=alpha("grey",0.5)),
        panel.grid.major.y = element_line(color=alpha("grey",0.5)),
        panel.border = element_rect(color = "#000526", fill = NA, size = 0.5),
      ) +
      coord_fixed(ratio = 1) +
      coord_flip() +
      scale_x_reverse()
    
  }, height = function() {
    if (input$dataset == "Wszyscy") {
      return(650)
    } else {
      return(400)
    }
  })  
  
  #WYKRES BĄBELKOWY
  output$bubble_chart <- renderPlotly({
    selected_data() %>%
      #df_Natalka %>% 
      group_by(jaki.sen, stopień.wyspania) %>%
      summarise(Liczba_Obserwacji = n()) %>%
      plot_ly(
        x = ~jaki.sen,
        y = ~stopień.wyspania,
        size = ~Liczba_Obserwacji,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = ~factor(jaki.sen, levels = unique(jaki.sen), labels = c("#FCC214", "#FCE288", "#FFFEE8", "#BEA4F0", "#7200EA")),
          line = list(color = "white", width = 1),
          sizemode = "area",
          sizeref = 0.03
        )
      ) %>% 
      layout(
        xaxis = list(title = "Rodzaj snu",
                     titlefont = list(size = 22, color = "#FFFFFF", family = "Bellota"),
                     tickfont = list(size = 18, color = "#FFFFFF", family = "Bellota")),
        yaxis = list(title = "Stopień wyspania",
                     tickvals = c(1, 2, 3),
                     titlefont = list(size = 22, color = "#FFFFFF", family = "Bellota"),
                     tickfont = list(size = 18, color = "#FFFFFF", family = "Bellota")),
        
        showlegend = FALSE,
        font = list(size = 20, color = "#FFFFFF", family = "Bellota"),
        plot_bgcolor = "#000526",
        paper_bgcolor = "#000526"
      ) %>%
      add_text(
        x = ~jaki.sen,
        y = ~stopień.wyspania,
        text = ~Liczba_Obserwacji,
        textposition = "middle center",
        inherit = FALSE,
        textfont = list(color = "white", family = "Bellota", weight = "extra-bold")
      )
  })
  
}


shinyApp(ui = ui, server = server)