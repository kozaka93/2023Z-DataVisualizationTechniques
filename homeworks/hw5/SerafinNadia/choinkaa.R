library(ggplot2)

# background 
n <- 199
xmast <- data.frame(x = runif(n, 1, 60),
                     y = runif(n, 1, 60),
                     s = runif(n, min = 0, max = 60)) %>%
  ggplot() +
  geom_point(aes(x, y, size = s),
             color = "white", pch = 42) +
  theme(panel.background = element_rect("black"))

# trunk
xCoordinate <- c(27, 33, 33, 27, 27)
yCoordinate <- c(0, 0, 10, 10, 0)
points <- data.frame(xCoordinate, yCoordinate)
xmast <- xmast + 
  geom_polygon(data = points,
               aes(x = xCoordinate, y = yCoordinate, fill = "#bb6e1b"))

# first layer 
xCoordinate <- c(5, 12, 20, 30, 40, 48, 55, 30)
yCoordinate <- c(8, 7, 8, 6, 8, 7, 8, 30)
points <- data.frame(xCoordinate, yCoordinate)
xmast <- xmast + 
  geom_polygon(data = points, 
               aes(x = xCoordinate, y = yCoordinate, fill = "#90358e"))

# second layer
xCoordinate <- c(8, 16, 22, 30, 38, 44, 52, 30)
yCoordinate <- c(15, 13, 15, 12, 15, 13, 15, 35)
points <- data.frame(xCoordinate, yCoordinate)
xmast <- xmast + 
  geom_polygon(data = points, 
               aes(x = xCoordinate, y = yCoordinate, fill = "#394998"))

# third layer
xCoordinate <- c(11, 18, 24, 30, 36, 42, 49, 30)
yCoordinate <- c(22, 20, 22, 19, 22, 20, 22, 40)
points <- data.frame(xCoordinate, yCoordinate)
xmast <- xmast + 
  geom_polygon(data = points, 
               aes(x = xCoordinate, y = yCoordinate, fill = "#2a8e58"))

# fourth layer
xCoordinate <- c(14, 20, 25, 30, 35, 40, 46, 30)
yCoordinate <- c(29, 27, 29, 26, 29, 27, 29, 45)
points <- data.frame(xCoordinate, yCoordinate)
xmast <- xmast + 
  geom_polygon(data = points, 
               aes(x = xCoordinate, y = yCoordinate, fill = "#eae316"))

# fifth layer
xCoordinate <- c(17, 22, 26, 30, 34, 38, 43, 30)
yCoordinate <- c(35, 33, 35, 32, 35, 33, 35, 50)
points <- data.frame(xCoordinate, yCoordinate)
xmast <- xmast + 
  geom_polygon(data = points, 
               aes(x = xCoordinate, y = yCoordinate, fill = "#e87725"))

# sixtf layer
xCoordinate <- c(19, 24, 27, 30, 33, 36, 41, 30)
yCoordinate <- c(41, 39, 41, 39, 41, 39, 41, 55)
points <- data.frame(xCoordinate, yCoordinate)
xmast <- xmast + 
  geom_polygon(data = points, 
               aes(x = xCoordinate, y = yCoordinate, fill = "#df2830"))

# star
xCoordinate <- c(23, 27.5, 30, 32.5, 37, 34.5, 37, 32.5, 30, 27.5, 23, 25.5, 23)
yCoordinate <- c(48, 48, 45, 48, 48, 52, 55.5, 55.5, 59.5, 55.5, 55.5, 52, 48)
points <- data.frame(xCoordinate, yCoordinate)
xmast <- xmast + 
  geom_polygon(data = points, 
               aes(x = xCoordinate, y = yCoordinate, fill = "#eac514"))

# final
xmast <- xmast +
  scale_fill_identity() + 
  theme_bw() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") + 
  theme_void() +
  coord_cartesian(c(1, 60), c(1, 60)) +
  theme(plot.background = element_rect(fill="black", colour="black"),
        panel.background = element_rect(fill="black")) +
  guides(fill = FALSE,
         size = FALSE)

# presentation
xmast

# saving
ggsave("xmas-tree.png", plot = xmast, width = 6, height = 6, units = "in", dpi = 300)

