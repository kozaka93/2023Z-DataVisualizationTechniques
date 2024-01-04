library(plotly)
library(ggplot2)

arclen <- 2 * pi
plot <- plot_ly()
plot2 <- plot_ly()

generate_tree_plot <- function(ppr = 100, revolutions = 1, branch_count = 10, height = 11) {
    point_count <- ceiling(ppr * revolutions)

    width <- height * 3/10
    branch_offset <- arclen / branch_count
    branch_offset <- seq(from = 0, by = branch_offset, length.out = branch_count)

    z <- rep(seq(1, height+1, length.out = point_count), 2*branch_count)

    spread <- (1:point_count) / ppr * arclen
    grid <- expand.grid(spread, branch_offset)
    spread <- grid$Var1 + grid$Var2
    spread <- c(spread, rev(spread))

    x <- sin(spread) * rev(z-1) * width / height
    y <- cos(spread) * rev(z-1) * width / height

    color_choices <- c("green", "darkgreen", "forestgreen", "yellow")
    probabilities <- c(0.325, 0.325, 0.325, 0.03)
    colors <- sample(color_choices, size = point_count * branch_count * 2, replace = TRUE, prob = probabilities)
    df <- data.frame(x, y, z, colors)

    plot <- plot %>% 
        add_trace(  df,
                    x = ~x,
                    y = ~y,
                    z = ~z,
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(size = 6, color = ~colors))
    return(plot)
}

generate_trunk_plot <- function(ppr = 5, height = 10.5, ring_density = 0.2, width = 0.2) {

    ring_count <- floor(height / ring_density)
    ring_heights <- rep(seq(0, height, length.out = ring_count), each = ppr)
    spread <- rep((1:ppr) / ppr * arclen, ring_count)
    
    x <- sin(spread) * width
    y <- cos(spread) * width

    df <- data.frame(x, y, ring_heights)
    
    plot <- plot %>% 
        add_trace(  df,
                    x = ~x,
                    y = ~y,
                    z = ~ring_heights,
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(size = 10, color = "saddlebrown"))
    return(plot)
}

generate_tree_base <- function(width = 3.4, ring_density = 0.2) {

    ring_radia <- seq(0, width, by = ring_density)
    point_counts <- round(ring_radia*50)

    spread <- sapply(point_counts, function(point_count) seq(0, arclen, length.out = point_count))
    x <- unlist(Map(function(vector, r) sin(vector) * r, spread, ring_radia))
    y <- unlist(Map(function(vector, r) cos(vector) * r, spread, ring_radia))
    z <- rep(1, sum(point_counts))

    df <- data.frame(x, y, z)

    color_choices <- c("green", "darkgreen", "forestgreen")
    colors <- sample(color_choices, size = sum(point_counts), replace = TRUE)
    df <- data.frame(x, y, z, colors)

    plot <- plot %>% 
        add_trace(  df,
                    x = ~x,
                    y = ~y,
                    z = ~z,
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(size = 5, color = ~colors))
    return(plot)
}

generate_snow <- function(width = 4) {
    point_count = 100
    x <- seq(-width, width, length.out = point_count)
    y <- seq(-width, width, length.out = point_count)
    grid <- -abs(expand.grid(x, y))
    shiftz <- runif(point_count * point_count, -0.25, 0.25)
    # shiftx <- runif(point_count * point_)
    x <- rep(x, each = point_count)
    y <- rep(y, times = point_count)
    z <- sin(2*grid$Var1*pi) * sin(2*grid$Var2*pi)
    z <- z - exp(grid$Var1 * grid$Var1 / 6) / 10 - exp(grid$Var2 * grid$Var2 / 6) / 10
    z <- pmax(z + shiftz - 0.7, -3)

    df = data.frame(x, y, z)

    plot <- plot %>% 
        add_trace(  df,
                    x = ~x,
                    y = ~y,
                    z = ~z,
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(size = 4, color = "white"))
    return(plot)
}

plot <- plot %>% 
    layout(scene = list(
        bgcolor = "#02012c",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        zaxis = list(range = c(-2.5, 14), visible = FALSE)
    ))
plot <- generate_tree_plot()
plot <- generate_trunk_plot()
# plot <- generate_tree_base()
plot <- generate_snow()
print(plot)
