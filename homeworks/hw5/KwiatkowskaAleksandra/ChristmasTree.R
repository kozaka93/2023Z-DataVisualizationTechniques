library(plotly)
library(dplyr)


# Generowanie danych
t1 <- seq(0, 50, by = 0.7)^0.85
x1 <- t1 * c(sin(t1), sin(t1 + pi))
y1 <- t1 * c(cos(t1), cos(t1 + pi))
z1 <- -2 * c(t1, t1)
color1 <- rep(c("gold", "firebrick2"), each = length(t1))

df_bombki <- data.frame(
  x = x1,
  y = y1,
  z = z1,
  color = color1
)


# Wizualizacja choinki

plot <- plot_ly() %>% 
  add_markers(
  data = df_bombki,
  x = ~x,
  y = ~y,
  z = ~z,
  type = "scatter3d",
  color = ~color,
  colors = c("gold", "firebrick2"),
  line = list(width = 5, color = c("forestgreen"), reverscale = FALSE),
  marker = list(width = 5)
) %>%
  # Gwiazdka
  add_markers(
  x = 0,
  y = 0,
  z = 3.5,
  marker = list(size = 3, color = 'gold', symbol = 'diamond')
) %>%
  add_markers(
    x = 0,
    y = 0,
    z = 3.5,
    marker = list(size = 10, color = 'gold', symbol = 'cross')
  ) %>%
  add_markers(
    x = 0,
    y = 0,
    z = 3.5,
    marker = list(size = 5, color = 'gold', symbol = 'x')
  ) %>% 
  # Prezenty
  add_trace(
    type = 'mesh3d',
    x = c(20, 20, 25, 25, 20, 20, 25, 25),
    y = c(25, 30, 30, 25, 25, 30, 30, 25),
    z = c(-50, -50, -50, -50, -55, -55, -55, -55),
    
    i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
    j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
    k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
    
    facecolor = rep("navy", 12)
  ) %>% 
add_trace(
    type = 'mesh3d',
    x = c(10, 10, 18, 18, 10, 10, 18, 18),
    y = c(25, 35, 35, 25, 25, 35, 35, 25),
    z = c(-46, -46, -46, -46, -55, -55, -55, -55),
    
    i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
    j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
    k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
    
    facecolor = rep("darkgreen", 12)
  ) %>% 
add_trace(
    type = 'mesh3d',
    x = c(25, 25, 32, 32, 25, 25, 32, 32),
    y = c(15, 22, 22, 15, 15, 22, 22, 15),
    z = c(-48, -48, -48, -48, -55, -55, -55, -55),
    
    i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
    j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
    k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
    
    facecolor = rep("orangered", 12)
  ) %>% 
add_trace(
  type = 'mesh3d',
  x = c(27, 27, 30, 30, 27, 27, 30, 30),
  y = c(24, 27, 27, 24, 24, 27, 27, 24),
  z = c(-52, -52, -52, -52, -55, -55, -55, -55),
  
  i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
  j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
  k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
  
  facecolor = rep("slateblue", 12)
) %>% 
  layout(
    showlegend = F,
    title = list(text = "Merry Christmas", font = list(family = "Balto", size = 26, color = "#112716"),
                 y = 0.92, xanchor = "middle"),
    scene = list(
      xaxis = list(
        title = "",
        showticklabels = FALSE
      ),
      yaxis = list(
        title = "",
        showticklabels = FALSE
      ),
      zaxis = list(
        title = "",
        showticklabels = FALSE
      )
    )
  )

htmlwidgets::saveWidget(plot, file = "ChristmasTree.html")
