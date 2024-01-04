library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(stringr)

setwd('C:/Users/Agata Krawczyk/Desktop/HW2')
players <- read.csv("wta_players.csv")
rankings <- read.csv("wta_rankings_current.csv")
rankings2 <- read.csv("wta_rankings_20s.csv")
rankings <- rbind(rankings,rankings2)
top_5 <- players %>% 
  filter(name_last %in% c("Swiatek","Sabalenka","Gauff","Rybakina","Pegula")) %>% 
  mutate(tenisitka=str_c(name_last,name_first,sep=" "))
colnames(top_5)[1] <- "player"         
x <- rankings %>% 
  filter(player %in% top_5$player) %>% 
  mutate(week=lubridate::week(ymd(as.Date(as.character(ranking_date), format = "%Y%m%d")))) %>% 
  mutate(months=month.name[month(as.Date(as.character(ranking_date), format = "%Y%m%d"))]) %>% 
  mutate(date=as.Date(as.character(ranking_date), format = "%Y%m%d")) %>% 
  arrange(date)
merged <- inner_join(top_5,x,by="player")
merged$tenisitka <- factor(merged$tenisitka,                 
                           levels = c("Sabalenka Aryna","Swiatek Iga","Gauff Cori","Pegula Jessica","Rybakina Elena"))
merged <- merged %>% 
  select(tenisitka, date,points)


ui <- fluidPage(
  titlePanel("Date Range Plot"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("range", "Select Date Range",start="2020-01-01",end="2020-12-31",min="2020-01-01",max="2023-12-31",language="pl",startview = "year"),
      br()
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output,session) {
  filtered <- reactive({
    merged %>% 
           filter( date >=min( input$range) & date <= max(input$range))
  })
  

  output$plot <- renderPlotly({ 
  plot_ly(filtered(), 
          x = ~date, 
          y = ~points, 
          color = ~tenisitka, 
          type = "scatter", 
          mode = "lines") %>%
    layout(title = list(text = "Player Points Over Time", x = 0.5),
           xaxis = list(title = "Date", showgrid = TRUE, zeroline = FALSE, gridcolor = "lightgray"),
           yaxis = list(title = "Points", showgrid = TRUE, zeroline = FALSE, gridcolor = "lightgray"),
           legend = list(title = list(text = "Player")),
           showlegend = TRUE,
           margin = list(l = 50, r = 20, b = 50, t = 80),
           plot_bgcolor = "white",
           paper_bgcolor = "white",
           font = list(family = "Arial, sans-serif", size = 14),
           hoverlabel = list(bgcolor = "white", font = list(family = "Arial, sans-serif", size = 14))
    )
    
    
  
  })
  
   
}

shinyApp(ui = ui, server = server)
