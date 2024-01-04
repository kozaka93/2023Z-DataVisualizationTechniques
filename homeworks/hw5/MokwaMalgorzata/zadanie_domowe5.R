library(plotly)

a <- seq(-15 * pi, 100 * pi, length.out = 500)
b <- seq(-1, 0, length.out = 500)
y <- 0

wykres <- plot_ly(
  x = cos(a + b) * b,
  y = y,
  z = b,
  type = "scatter3d",
  mode = "lines",
  line = list(color = "#096836", width = 4)
) %>% 
  add_trace(
    y = cos(a + b) * b,
    x = y,
    z = b,
    type = "scatter3d",
    mode = "lines",
    showlegend = FALSE
  ) %>% 
  add_markers(
    x = 0,
    y = 0,
    z = 0,
    marker = list(size = 10, color = 'yellow', symbol = 'cross'),
    showlegend = FALSE
  ) %>%
  add_markers(
    x = 0,
    y = 0,
    z = 0,
    marker = list(size = 3, color = 'yellow', symbol = 'x'),
    showlegend = FALSE
  )

x1 <- c(0, 0, 0, 0, 0, 0, 0)
y1 <- c(0.0075, 0.1810, 0.4247, -0.8104, -0.5238, -0.2814, -0.1645)
z1 <- c(-0.1262, -0.7354, -0.4769, -0.9318, -0.8236, -0.4929, -0.4228)

x2 <- c(0, 0, 0, 0, 0, 0, 0)
y2 <- c(0.8104, 0.5238, 0.2814, 0.1645, -0.0075, -0.1810, -0.4247)
z2 <- c(-0.9318, -0.8236, -0.4929, -0.4228, -0.1262, -0.7354, -0.4769)

x3 <- c(0.9098, -0.3593, -0.5759, -0.1737, -0.9098, 0.1308, -0.0620, -0.2539)
y3 <- c(0, 0, 0, 0, 0, 0, 0, 0)
z3 <- c(-0.9298, -0.9098, -0.6853, -0.4749, -0.9298, -0.9699, -0.0801, -0.2705)

x4 <- c(0.3593, 0.5759, 0.1737, -0.1308, 0.0620, 0.2539)
y4 <- c(0, 0, 0, 0, 0, 0)
z4 <- c(-0.9098, -0.6853, -0.4749, -0.9699, -0.0801, -0.2705)

bombki1 <- data.frame(x1, y1, z1)
bombki2 <- data.frame(x2, y2, z2)
bombki3 <- data.frame(x3, y3, z3)
bombki4 <- data.frame(x4, y4, z4)

wykres1 <- wykres %>% 
  add_trace(bombki1, x = ~x1, y = ~y1, z = ~z1, type = "scatter3d",  mode = 'markers', marker = list(color = 'blue', size = 15), showlegend = FALSE) %>%  
  add_trace(bombki2, x = ~x2, y = ~y2, z = ~z2, type = "scatter3d",  mode = 'markers', marker = list(color = 'red', size = 10), showlegend = FALSE) %>% 
  add_trace(bombki3, x = ~x3, y = ~y3, z = ~z3, type = "scatter3d",  mode = 'markers', marker = list(color = 'yellow', size = 15), showlegend = FALSE) %>% 
  add_trace(bombki4, x = ~x4, y = ~y4, z = ~z4, type = "scatter3d",  mode = 'markers', marker = list(color = 'orange', size = 10), showlegend = FALSE) 

wykres2 <- wykres1 %>% 
  layout(
    title = list(
      text = "Wesołych Świąt!",
      x = 0.5,
      y = 0.95,
      xref = 'paper',  
      yref = 'paper', 
      font = list(size = 30)
    ),
    scene = list(xaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
                 zaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
                 aspectratio = list(x = 2, y = 2, z = 2),
                 bgcolor = '#958FD7'))
wykres2  
