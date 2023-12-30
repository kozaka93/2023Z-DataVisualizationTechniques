library(ggplot2)
library(plotly)

x <- seq(-10, 10, 0.1)
y <- seq(-10, 10, 0.1)
z <- outer(x, y, function(x, y) {-sqrt(x^2 + y^2)})

set.seed(2205)
x_red <- runif(25, min = -10, max = 10)
y_red <- runif(25, min = -10, max = 10)
z_red <- -sqrt(x_red^2 + y_red^2) + 0.5

x_yellow <- runif(35, min = -10, max = 10)
y_yellow <- runif(35, min = -10, max = 10)
z_yellow <- -sqrt(x_yellow^2 + y_yellow^2) + 0.4

x_blue <- runif(45, min = -10, max = 10)
y_blue <- runif(45, min = -10, max = 10)
z_blue <- -sqrt(x_blue^2 + y_blue^2) + 0.3


color_scale <- list(
    c(0, "#163020"),
    c(1, "#739072")
)

plot_ly(
    x = ~x, 
    y = ~y, 
    z = ~z, 
    type = "surface", 
    surfacecolor = ~z, 
    colors = c("#163020", "#739072"),
    showlegend = FALSE,
    showscale = FALSE
) %>%
    add_trace(
        x = ~x_red, 
        y = ~y_red, 
        z = ~z_red, 
        type = "scatter3d", 
        mode = "markers", 
        marker = list(size = 10, color = '#ed5720')) %>%
    add_trace(
        x = ~x_blue, 
        y = ~y_blue, 
        z = ~z_blue, 
        type = "scatter3d", 
        mode = "markers", 
        marker = list(size = 5, color = '#4848c9')) %>%
    add_trace(
        x = ~x_yellow, 
        y = ~y_yellow, 
        z = ~z_yellow, 
        type = "scatter3d", 
        mode = "markers", 
        marker = list(size = 8, color = '#ffbf00'))