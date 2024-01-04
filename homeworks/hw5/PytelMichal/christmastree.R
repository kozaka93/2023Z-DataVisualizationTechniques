library(ggplot2)

# Define coordinates for three triangles forming the inverted tree
triangle1 <- data.frame(
  x = c(0, 1, -1),
  y = c(2, 0, 0)
)

triangle2 <- data.frame(
  x = c(0, 0.7, -0.7),
  y = c(2.5, 0.8, 0.8)
)

triangle3 <- data.frame(
  x = c(0, 0.5, -0.5),
  y = c(3, 1.5, 1.5)
)

triangle4 <- data.frame(
  x = c(0, 0.4, -0.4),
  y = c(3.5, 2.1, 2.1)
)

# Define trunk coordinates
trunk <- data.frame(
  x = c(-0.1, 0.1, 0.1, -0.1),
  y = c(0, 0, -0.3, -0.3)
)

# Generate random decorations
set.seed(5)
decorations1 <- data.frame(
  x = runif(50, min = -0.3, max = 0.3),
  y = runif(50, min = 0, max = 2.8)
)
set.seed(10)
decorations2 <- data.frame(
  x = runif(50, min = -0.5, max = 0.5),
  y = runif(50, min = 0, max = 2)
)
set.seed(20)
snow <- data.frame(
  x = runif(200, min = -1.5, max = 1.5),
  y = runif(200, min = 0.3, max = 4)
)

# Create ground coordinates (a rectangle at the bottom)
ground <- data.frame(
  x = c(-1.5, 1.5, 1.5, -1.5),
  y = c(-0.3, -0.3, -0.8, -0.8)
)

# Plot the inverted Christmas tree shape using triangles, trunk, decorations, and ground
tree_plot <- ggplot() +
  
  geom_polygon(data = ground, aes(x, y), fill = "white") +  # Lighter brown ground color
  geom_polygon(data = triangle1, aes(x, y), fill = "#228B22") +
  geom_polygon(data = triangle2, aes(x, y), fill = "#228B22") +
  geom_polygon(data = triangle3, aes(x, y), fill = "#228B22") +
  geom_polygon(data = triangle4, aes(x, y), fill = "#228B22") +
  geom_polygon(data = trunk, aes(x, y), fill = "#8B4513") + # Brown trunk
  geom_point(data = decorations1, aes(x, y), color = "gold", size = 3) + # Random decorations
  geom_point(data = decorations2, aes(x, y), color = "red", size = 3) + # Random decorations
  geom_point(data = snow, aes(x, y), color = "white", size = 2) + # Random decorations
  theme_void() +
  theme(panel.background = element_rect(fill = "black"), legend.position = "none")

tree_plot





