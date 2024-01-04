library(ggplot2)
library(dplyr)
library(gganimate)

#tło
 tree <- ggplot() +
  theme(panel.background = element_rect("darkblue"))

# pieniek
x <- c(42, 58, 58, 42)
y <- c(0, 0, 13, 13)
points <- data.frame(x, y)
tree <- tree + 
  geom_polygon(data = points,
               aes(x = x, y = y, fill = "#964B00"))

# dół choinki
x <- c(5, 20, 35, 50, 65, 80, 95, 50)
y <- c(9, 8, 10, 8, 10, 8, 9, 30)
points <- data.frame(x, y)
tree <- tree + 
  geom_polygon(data = points, 
               aes(x = x, y = y, fill = "#047200"), shape = 1)

# środek
x <- c(10, 20, 35, 50, 65, 80, 90, 50)
y <- c(22, 21, 23, 21, 23, 21, 22, 44)
points <- data.frame(x, y)
tree <- tree + 
  geom_polygon(data = points, 
               aes(x = x, y = y, fill = "#048900"), shape = 1)

# góra
x <- c(16, 20, 35, 50, 65, 80, 84, 50)
y <- c(36, 35, 37, 35, 37, 35, 36, 59)
points <- data.frame(x, y)
tree <- tree + 
  geom_polygon(data = points, 
               aes(x = x, y = y, fill = "#06b200"), shape = 1)

# tekst1
tree <- tree +
  annotate("text", x = 50, y = 75, label = "Merry",
           color = "red", size = 16, family = "serif")

# tekst2
tree <- tree +
  annotate("text", x = 50, y = 68, label = "Christmas",
           color = "red", size = 16, family = "serif")

# gwiazdeczka
star <- data.frame(
  x = c(50, 48, 44, 48, 45, 50, 55, 52, 56, 52),
  y = c(63, 60, 60, 58, 55, 57, 55, 58, 60, 60)
)
tree <- tree +
  geom_polygon(data = star, aes(x = x, y = y, fill = "gold"), shape = 1)

# śnieżek
płatki_sniegu <- data.frame(x = runif(100000, 1, 100),
                    y = runif(100000, 1, 80),
                    time = seq(1, 1000, 1))


# sklejanie
tree <- tree +
  scale_fill_identity() + 
  theme_bw() +
  scale_x_continuous(breaks = NULL, limits = c(0, 100)) +
  scale_y_continuous(breaks = NULL, limits = c(0, 80)) +
  labs(x = "", y = "") + 
  theme_void() +
  theme(plot.background = element_rect(fill="darkblue", colour="darkblue"),
        panel.background = element_rect(fill="darkblue")) +
  guides(fill = FALSE,
         size = FALSE) +
  geom_point(data = płatki_sniegu, aes(x, y), color = "white", pch = 42, size = 3) +
  transition_time(time)

# animacja
tree
animate(tree)
anim_save("choineczka.gif")
