library(shiny)
library(PogromcyDanych)


ui <- fluidPage(

    titlePanel("Seriale"),
 
    sidebarLayout(
        sidebarPanel(
            selectInput("wybrany_serial",
                        "Wybierz serial:",
                        unique(serialeIMDB$serial)),
            
            checkboxInput("czy_trend", 
                          "Czy linia trendu?",
                          FALSE)
        ),

        
        mainPanel(
           plotOutput("distPlot"),
           textOutput("tekst")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      p1 <- ggplot(serialeIMDB[serialeIMDB$serial == input$wybrany_serial, ],
             aes(x = id, y = ocena)) + 
        geom_point(aes(color = sezon)) + 
        labs(title = paste("Rozkład ocen dla serialu", input$wybrany_serial),
             x = "Odcinek",
             y = "Ocena",
             color = "Sezon") + 
        theme_minimal()
      
      if (input$czy_trend) {
        p1 <- p1 + geom_smooth(se = FALSE)
      }
      
      p1
      
    })
    
    output$tekst <- renderText({
      wybrany_serial <- serialeIMDB[serialeIMDB$serial == input$wybrany_serial,]
      max_ocena <- wybrany_serial$ocena[which.max(wybrany_serial$ocena)]
      odc <- wybrany_serial$odcinek[which.max(wybrany_serial$ocena)]
      sez <- wybrany_serial$sezon[which.max(wybrany_serial$ocena)]
      
      paste0("Najwyżej oceniony odcinek to ", 
             odc, 
             " z sezonu ",
             sez, 
             ". Jego ocena to ",
             max_ocena,
             ".")
      
    })
}

shinyApp(ui = ui, server = server)
