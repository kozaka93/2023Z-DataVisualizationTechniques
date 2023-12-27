library(shiny)
library(rbokeh)

ui <- fluidPage(
  titlePanel("CHRISTMAS TREE"),
  sidebarLayout(
    sidebarPanel(
      selectInput("color_select", label = "Select Color of Christmas decorations", choices = c("Red", "Blue", "Yellow"), selected = "Red")
    ),
    mainPanel(
      rbokehOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderRbokeh({
    x_tree_trunk <- c(0, 0)
    y_tree_trunk <- c(1, 4)
    
    x1 = seq(-0.6, 0.6, length = 10)
    x2 = seq(-0.9, 0.9, length = 17)
    x3 = seq(-1.2, 1.2, length = 25)
    x4 = seq(-1.5, 1.5, length = 32)
    x5 = seq(-1.8, 1.8, length = 35)
    
    plot <- figure(xlim = c(-2.5, 2.5), ylim = c(1, 4.2),xgrid = FALSE, ygrid = FALSE, xaxes = FALSE, yaxes = FALSE) %>%
      ly_lines(x = x_tree_trunk, y = y_tree_trunk, color = "#9a5626", line_width = 20) %>%
      ly_lines(x = runif(200, -2, 2) * seq(0.1, 0.4, length = 200), 
               y = seq(4, 3.5, length = 200),
               width = 5,
               color = "#228B01") %>% 
      ly_lines(x = runif(200, -2, 2) * seq(0.4, 0.5, length = 200), 
               y = seq(3.5, 3, length = 200),
               width = 5,
               color = "#228B01") %>% 
      ly_lines(x = runif(200, -2, 2) * seq(0.5, 0.7, length = 200), 
               y = seq(3, 2.5, length = 200),
               width = 5,
               color = "#228B01") %>% 
      ly_lines(x = runif(200, -2, 2) * seq(0.7, 0.9, length = 200), 
               y = seq(2.5, 2, length = 200),
               width = 5,
               color = "#228B01") %>% 
      ly_lines(x = runif(200, -2, 2) * seq(0.9, 1, length = 200), 
               y = seq(2, 1.5, length = 200),
               width = 5,
               color = "#228B01") %>% 
      ly_lines(x = c(0,0), 
               y = c(4, 4.15),
               width = 5,
               color = "#228B01") %>% 
      ly_lines(x = c(-20, 20), 
               y = c(1,1),
               width = 50,
               color = "#e5f4ff",alpha = 1
      )  %>%
      ly_points(x = runif(60, -3.5, 3.5) * seq(0, 1, length = 60), 
                y = seq(1, 1.2, length = 60),
                size = 60,
                color = "#e5f4ff", alpha = 10) %>%
      theme_plot(
        background_fill_color = '#8cb6d0',
      ) %>% 
      ly_points(x = x1, 
                y = 1/6*x1^2+3.5+runif(10,-0.02, 0.02),
                size = 6,
                color = "gold",
                alpha = 2,
                glyph = 23) %>% 
      ly_points(x = x2, 
                y = 1/7*x2^2+3+runif(17,-0.02, 0.02),
                size = 6,
                color = "gold",
                alpha = 2,
                glyph = 23) %>% 
      ly_points(x = x3, 
                y = 1/8*x3^2+2.5+runif(25,-0.02, 0.02),
                size = 6,
                color = "gold",
                alpha = 2,
                glyph = 23) %>% 
      ly_points(x = x4, 
                y = 1/9*x4^2+2+runif(32,-0.02, 0.02),
                size = 6,
                color = "gold",
                alpha = 2,
                glyph = 23) %>% 
      ly_points(x = x5, 
                y = 1/10*x5^2+1.5+runif(35,-0.02, 0.02),
                size = 6,
                color = "gold",
                alpha = 2,
                glyph = 23)%>% 
      ly_points(x = runif(6000,-50, 50), 
                y = runif(6000,0, 10),
                size = 6,
                color = "white",
                glyph = 'asterisk') 
    
    chosen_color <- switch(input$color_select,
                           "Red" = "red",
                           "Blue" = "blue",
                           "Yellow" = "yellow")
    
    my_plot <-plot %>% ly_points(x = runif(70, -2, 2) * seq(0.15, 1, length = 70),
                                 y = seq(2, 1.22, length = 70)^2,
                                 size = 10,
                                 alpha = 1.4,
                                 color = chosen_color)
    
    my_plot
  })
}

shinyApp(ui = ui, server = server)


