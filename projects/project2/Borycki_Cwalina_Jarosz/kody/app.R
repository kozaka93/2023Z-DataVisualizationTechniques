library(jpeg)
library(png)
library(shinythemes)
library(base64enc)
library(httr)
library(networkD3)
library(ggplot2)
library(tidyverse)
library(plotly)
library(shiny)

source("./db/variables.R")
source("./db/items.R")
source("./charts/animatedMap.R")
source("./charts/championsChart.R")
source("./charts/heatMap.R")
source("./charts/itemsSankeyGraph.R")
source("./charts/mapPlot.R")
source("./charts/playerOverallStatsChart.R")
source("./charts/timePlayChart.R")

ui <- navbarPage(
  title = tags$a("liGGa", id = "navbar-title", href = "https://github.com/jrsh44/R-dashboard"),

  # ABOUT PAGE
  tabPanel(
    "O projekcie",
    tags$div(
      class = "about-container",
      tags$div(class = "typoH5", "Informacje o projekcie"),
      tags$div(class = "typoH1", "League of Legends Stats"),
      tags$img(class = "tab4-decorator-lg", src = "assets/decorator-hr-lg.png"),
      tags$div(class = "tab-about-content",
        tags$div(class = "typoBodyBold", 
        "Projekt ma na celu wyciągnięcie wniosków/ przeanalizowanie zarówno wyborów gracza co do postaci , przedmiotów i czasu rozpoczęcia meczu  jak i jego performane w trakcie rozgrywki oraz uzyskanie informacji grając, którymi postaciami gracz radzi sobie najlepiej.
Dostęp do danych z gier został uzyskanych poprzez interfejs", 
          tags$a("API", href ="https://developer.riotgames.com/")
        )
      ),
      tags$div(class = "typoH2", "Twórcy"),
      tags$img(class = "tab-credits-decorator", src = "assets/decorator-hr.png"),
      tags$div(class = "tab-about-credits", 
        tags$div(class = "tab-about-credits-item",
          tags$img(class = "tab-about-credits-img", src = "assets/janek.png"),
          tags$div(class = "tab-about-credits-text", "Jan Cwalina")
        ),
        tags$div(class = "tab-about-credits-item",
          tags$img(class = "tab-about-credits-img", src = "assets/mati.png"),
          tags$div(class = "tab-about-credits-text", "Mateusz Jarosz")
        ),
        tags$div(class = "tab-about-credits-item",
          tags$img(class = "tab-about-credits-img", src = "assets/bartek.png"),
          tags$div(class = "tab-about-credits-text", "Bartłomiej Borycki")
        )
      )
    )
  ),

  # Zakładka 1
  tabPanel(
    "Ogólne",
    tags$head(
      tags$title("League of Legends Stats"),
      tags$link(rel = "icon", href = "assets/favicon.png"),
      tags$link(rel = "stylesheet", href = "fonts.css"),
      tags$link(rel = "stylesheet", href = "styles.css"),
    ),
    tags$div(
      class = "tab-wrapper",
      tags$div(
        class = "tab-title-text",
        tags$div(class = "typoH1", "Ogólne statystyki"),
        tags$img(class = "tab-decorator-lg", src = "assets/decorator-hr-lg.png")
      ),
      tags$div(
        class = "tab-section",
        tags$div(
          class = "tab-section-content",
          tags$div(
            class = "tab-title-text",
            tags$div(class = "typoH2", "Porównanie championów"),
            tags$div(class = "typoH5", "którymi gramy"),
            tags$img(class = "tab-decorator", src = "assets/decorator-hr.png"),
          ),
          tags$div(
            class = "typoBodyBold",
            "Obecnie w grze dostępnych jest ponad 160 postaci podzielonych na różne kategorie, takie jak zabójcy, strażnicy, magowie czy strzelcy. Stojąc przed tak szerokim wyborem istotne staje się określenie, którym bohaterem radzimy sobie najlepiej,"
          ),
          tags$div(class = "typoBody", textOutput("t1_stat_desc")),
          tags$div(
            class = "tab-champ-buttons",
            selectInput(
              inputId = "t1_player",
              label = "Gracz:",
              choices = c("Jan", "Bartek", "Mateusz")
            ),
            selectInput(
              inputId = "t1_stat",
              label = "Statystyka:",
              choices = c(
                "Zabójstwa",
                "Śmierci",
                "Współczynnik KDA",
                "Współczynnik zwycięstw",
                "Liczba gier"
              )
            ),
          ),
        ),
        tags$div(class = "tab-section-content",
          plotlyOutput("t1_champions_chart"),
        ),
      ),
      tags$div(class = "tab-vertical-container",
        tags$div(class = "tab-title-text",
          tags$div(class = "typoH2", "Kiedy najczęściej gramy"),
          tags$img(class = "tab-decorator", src = "assets/decorator-hr.png"),
          ),
        tags$div(class = "tab-vertical-desc-wrapper",
          tags$div(class = "tab1-time-text-wrapper", 
            tags$div(class = "typoBodyBold", "Każdy z nas ma wiele różnych obowiązków takich jak studia czy praca, ale łączy nas fakt, że zawsze znajdziemy czas, aby pograć w Ligę. Poniższa heatmapa prezentuje kiedy najczęściej zdarza nam się grać w przeciągu całego tygodnia.")
          ),
          tags$div(class = "tab1-time-button-wrapper", 
            selectInput(inputId = "t1_player_time",label = "Gracz:", choices = c("Jan","Bartek","Mateusz")),
          ),
        ),
        tags$div(class = "tab-horizontal-chart-wrapper",
          plotlyOutput("t1_time_played_chart")
        ),
      )
    )
  ),

  #Zakładka 2
  tabPanel(
    "Mapy",
    tags$div(class = "tab-wrapper",
      tags$div(
        class = "tab-title-text",
        tags$div(class = "typoH1", "Przebieg gier"),
        tags$img(class = "tab-decorator-lg", src = "assets/decorator-hr-lg.png")
      ),
      tags$div(
        class = "tab-section",
        tags$div(
          class = "tab-section-content",
          tags$div(
            class = "tab-title-text",
            tags$div(class = "typoH2", "Aktywność na mapie"),
            tags$div(class = "typoH5", "w zależności od pozycji"),
            tags$img(class = "tab-decorator", src = "assets/decorator-hr.png"),
          ),
          tags$div(
            class = "typoBodyBold",
            "Podczas rozgrywki, każdy gracz jest przypisany do jednej z pięciu pozycji. Zobaczmy, jak wygląda nasza aktywność, mierzona w łącznej liczbe zabójstw, śmierci i asyst, w różnych częściach mapy, w zależności od pozycji na której gramy"
          ),
          tags$div(class = "typoBody", textOutput("t2_desc")),
          tags$div(
            class = "tab-map-buttons",
            selectInput(
                      inputId = "t2_map_type",
                      label = "Typ:",
                      choices = c("Heatmap", "Scatter", "Animated"),
                      selected = "Heatmap"
                    ),
            selectInput(
                      inputId = "t2_map_player",
                      label = "Gracz:",
                      choices = c("Jan", "Bartek", "Mateusz"),
                      selected = "Bartek"
                    ),
            uiOutput("t2_dynamic_input")
            ),
          ),
        tags$div(class = "tab-map-wrapper",
                 plotlyOutput("t2_map"), ),
      ),
    )
  ),
  
  # Zakładka 3
  tabPanel("Szczegółowe",
    tags$div(class = "tab-wrapper",
      tags$div(class = "tab-title-text",
        tags$div(class = "typoH1", "Szczegółowe statystyki"),
        tags$img(class = "tab-decorator-lg", src = "assets/decorator-hr-lg.png")
      ),
      tags$div(class = "tab-section", 
        tags$div(class = "tab-adv-wrapper",
          tags$div(class = "tab-title-text",
            tags$div(class = "typoH2", "Statystyki Przywoływacza"),
            tags$img(class = "tab-decorator", style="width: 350px;", src = "assets/decorator-hr.png"),
            ),
          tags$div(class = "typoBodyBold", "Ze względu na możliwość wyboru jednej z pięciu ról (pozycji) oraz jednego z około 160 championów określenie wpływu na przebieg gry jest co najmniej bardzo trudnym zadaniem. Wraz z zespołem ustaliliśmy jednak 4 statystyki mogące stanowić o poziomie gracza."),
          tags$div(class = "tab-adv-buttons",
            selectInput(inputId = "summoner",label = "Gracz:", choices = c("Jan","Bartek","Mateusz")),
            uiOutput("t3_dynamic_input_0"),
            uiOutput("t3_dynamic_input"),
            uiOutput("t3_dynamic_input2"),
            selectInput(inputId = "type", label = "Typ:",choices = c("Density","Chronologically")))
        ),
        tags$div(class = "tab-adv-grid-container",
          plotlyOutput("t3_dmg_per_death"),
          plotlyOutput("t3_minions_per_minute"),
          plotlyOutput("t3_kill_participation"),
          plotlyOutput("t3_kda")
        )
      ),
      tags$div(class = "tab-vertical-container",
        tags$div(class = "tab-title-text",
          tags$div(class = "typoH2", "Wybór postaci i przedmiotów"),
          tags$img(class = "tab-decorator", src = "assets/decorator-hr.png"),
          ),
        tags$div(class = "tab-vertical-desc-wrapper",
          tags$div(class = "tab3-sankey-text-wrapper", 
            tags$div(class = "typoBodyBold", "Poprzez zabójstwa, wykonywanie zadań, niszczenie budowli przeciwnika czy 'farmienie' minionów gracz otrzymuje złoto, które następnie może być wymienione w sklepie na przedmioty. Najcenniejsze, oznaczone tagiem 'Mythic' dają unikalne wzmocnienia. Z tego względu każdy przywoływacz może używać tylko 1 przedmiotu mitycznego w danym momencie. Ze względu na umiejętności postaci i ich użyteczność w różnych aspektach rozgrywki, pewne przedmioty są szczególnie sugerowane pewnym klasom championów, co obrazuje poniższy wykres. Pierwszy węzeł wykresu ma formę legendy.")
          ),
          tags$div(class = "tab3-sankey-button-wrapper", 
            selectInput(inputId = "sankey_summoner",label = "Gracz:", choices = c("Jan","Bartek","Mateusz")),
          ),
        ),
        tags$div(class = "tab3-sankey-legend-wrapper",
          tags$div(class="tab3-sankey-legend-item",
            tags$div(class="circle", style="background-color: #E18417;"),
            tags$div(class="circle-text", "Fighter")
          ),
          tags$div(class="tab3-sankey-legend-item" ,
            tags$div(class="circle", style="background-color: #7C17E1;"),
            tags$div(class="circle-text", "Mage")
          ),
          tags$div(class="tab3-sankey-legend-item" ,
            tags$div(class="circle", style="background-color: #E41D1D;"),
            tags$div(class="circle-text", "Slayer")
          ),
          tags$div(class="tab3-sankey-legend-item" ,
            tags$div(class="circle", style="background-color: #00BF3B;"),
            tags$div(class="circle-text", "Tank")
          ),
          tags$div(class="tab3-sankey-legend-item" ,
            tags$div(class="circle", style="background-color: #004AAD;"),
            tags$div(class="circle-text", "Marksman")
          ),
          tags$div(class="tab3-sankey-legend-item" ,
            tags$div(class="circle", style="background-color: #03F6FF;"),
            tags$div(class="circle-text", "Specialist")
          ),
          tags$div(class="tab3-sankey-legend-item" ,
            tags$div(class="circle", style="background-color: #EDCC23;"),
            tags$div(class="circle-text", "Controller")
          ),
        ),
        tags$div(class = "tab-horizontal-chart-wrapper",
          sankeyNetworkOutput("t3_sankey")
        ),
      )
    )
  )
)

server <- function(input, output) {
  # Zakładka 1
  output$t1_stat_desc <- renderText({
    if (input$t1_stat == "Zabójstwa") {
      "Liczba zabójstw informuje o tym, ilu łącznie przeciwników udało nam się pokonać podczas rozgrywki daną postacią. Im więcej zabójstw, tym silniejszy mamy wpływ na przebieg gry."
    } else if (input$t1_stat == "Śmierci") {
      "Liczba śmierci informuje o tym, ile razy łącznie bohater został pokonany przez przeciwników grając danym championem."
    } else if (input$t1_stat == "Współczynnik KDA") {
      "Współczynnik KDA to suma zabójstw i asyst podzielona przez liczbę śmierci. Jest to miara efektywności gracza w walce."
    } else if (input$t1_stat == "Współczynnik zwycięstw") {
      "Współczynnik zwycięstw odzwierciedla procent wygranych gier spośród wszystkich rozegranych."
    } else if (input$t1_stat == "Liczba gier") {
      "Liczba gier rozegranych daną postacią pozwala określić nie tylko popularność postaci, ale również doświadczenie gracza w jej obsłudze."
    }
  })
  
  output$t1_champions_chart <- renderPlotly({
    f_plot_champions(input$t1_player, input$t1_stat)
  })
  
  output$t1_time_played_chart <- renderPlotly({
    f_plot_time(input$t1_player_time)
  })
  
  
  # Zakładka 2
  output$t2_desc <- renderText({
    if(input$t2_map_type == "Animated"){
      "Animacja pokazuje naszą aktywność na mapie w każdej minucie z osobna, co wizualizuje, jak zmienia się dynamika naszej rozgrywki w czasie"
    }else{""}
  })
  output$t2_dynamic_input <- renderUI({
    myStatsPosition <- df_item_champ %>%
      dplyr::filter(player_id == as.vector(
        summoner %>% filter(name %in% input$t2_map_player) %>% select(puuid)
      ))
    most_played <-
      myStatsPosition %>% group_by(position) %>%
      summarise(n = n()) %>% slice_max(order_by = n, n = 1) %>%
      select(position) %>% pull()
    selectInput(
      inputId = "t2_position",
      label = "Pozycja:",
      choices = c(unique(unlist(
        myStatsPosition$position
      ))),
      selected = most_played
    )
    
  })
  
  output$t2_map <- renderPlotly({
    if (input$t2_map_type == "Animated") {
      f_animated_map(
        input$t2_map_player,
        c("death", "kill", "assist"),
        "All",
        input$t2_position,
        win = c(T, F),
        team_id = c(200, 100)
      )
    } else if (input$t2_map_type == "Scatter") {
      f_map(
        input$t2_map_player,
        c("death", "kill", "assist"),
        "All",
        input$t2_position,
        win = c(T, F),
        team_id = c(200, 100)
      )
    } else{
      f_heat_map(
        input$t2_map_player,
        c("death", "kill", "assist"),
        "All",
        input$t2_position,
        win = c(T, F),
        team_id = c(200, 100)
      )
    }
  })
  
  
  # Zakładka 3
    observeEvent(input$summoner, {
      output$t3_dynamic_input_0 <- renderUI({
        positions <- c("TOP","JUNGLE","MIDDLE","BOTTOM","UTILITY")
        myStatsPosition <-  unique((df_item_champ %>% dplyr::filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner))))$position)
        selectInput(inputId = "Position",label = "Pozycja:",
                    choices = positions[positions %in% myStatsPosition])
        
      })
    })
    
    observeEvent(input$Position, {  
    output$t3_dynamic_input <- renderUI({
        myStatsPosition <- df_item_champ %>%
          dplyr::filter(
        player_id == as.vector(
          summoner %>% filter(name %in% input$summoner) %>% select(puuid)
        ),
        position == input$Position
      )
        if (nrow(myStatsPosition) == 0) {
          selectInput(inputId = "id1",
                      label = "Champion:",
                      choices = c("None"))
        } else {
          selectInput(
        inputId = "id1",
        label = "Champion:",
        choices = c("All", unique(
          unlist(myStatsPosition$champion_name)
        ))
      )
        }
        
      })
    })
  
    
    observeEvent(input$id1, {
      output$t3_dynamic_input2 <- renderUI({
        if (input$type=="Density") {
          myStatsPosition <- df_item_champ %>% 
            filter(player_id == as.vector(summoner %>% filter(name %in% input$summoner) %>% select(puuid)),position==input$Position)
          if (nrow(myStatsPosition)==0) {
            selectInput(inputId = "compare",
                        label = "Porównaj z:",
                        choices = c("None"))
          } else {
            champs <- unique(unlist(myStatsPosition$champion_name))
            if (input$id1=="All" | input$id1=="None") {
              selectInput(inputId = "compare", label = "Porównaj z:", choices = c("Don't"))
            } else {
              selectInput(inputId = "compare", label = "Porównaj z:", choices = c("Don't","All",champs[champs != input$id1]))
            }
          }
        }
      })
    })

      output$t3_dmg_per_death <- renderPlotly({
        if (!is.null(input$id1)){
          if (input$type=='Density') {
            f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath",input$compare)
          } else {
            f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath")
          }
        }
      })

    
  output$t3_dmg_per_death <- renderPlotly({
    if (!is.null(input$id1)){
      if (input$type=='Density') {
        f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"DmgPerDeath",input$compare)
      } else {
        f_overall_stats_plot(input$summoner,
                            input$Position,
                            input$id1,
                            input$type,
                            "DmgPerDeath")
      
      }
    }
  })
  
  output$t3_kill_participation <- renderPlotly({
    if (!is.null(input$id1)){
      if (input$type=='Density') {
        f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kill_participation",input$compare)
      } else {
        f_overall_stats_plot(input$summoner,
                            input$Position,
                            input$id1,
                            input$type,
                            "kill_participation")
      }
    }
  })
  
  output$t3_minions_per_minute <- renderPlotly({
    if (!is.null(input$id1)){
      if (input$type=='Density') {
        f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"MinionsPerMinute",input$compare)
      } else {
        f_overall_stats_plot(input$summoner,
                            input$Position,
                            input$id1,
                            input$type,
                            "MinionsPerMinute")
      }
    }
  })
  
  output$t3_kda <- renderPlotly({
    if (!is.null(input$id1)){
      if (input$type=='Density') {
        f_overall_stats_plot(input$summoner,input$Position,input$id1,input$type,"kda",input$compare)
      } else {
        f_overall_stats_plot(input$summoner,
                            input$Position,
                            input$id1,
                            input$type,
                            "kda")
      }
    }
  })
  
  output$t3_sankey <- renderSankeyNetwork({

    htmlwidgets::onRender(f_items_sankey_graph(input$sankey_summoner),
                                             'function(el, x) {
    d3.selectAll(".node text")
        .style("fill", "#c8aa6e").style("font-size","14px");
  }'
    )
    
  })
  
  output$t3_sankey_legend <- renderSankeyNetwork({
    htmlwidgets::onRender(f_sankey_lenend_graph(),
                          'function(el, x) {
    d3.selectAll(".node text")
        .style("fill", "#c8aa6e").style("font-size","14px");
  }'
    )
  })
  
}


shinyApp(ui, server)





