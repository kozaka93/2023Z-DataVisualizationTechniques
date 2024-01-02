library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)


y <- seq(0,1500,10)

x5 <- seq(465, 310/1.62, length.out = 50) * c(-1,1)

x4 <- seq(310, 186/1.62, length.out = 40) * c(-1,1)

x3 <- seq(186, 93/1.62, length.out = 30) * c(-1,1)

x2 <- seq(93, 31/1.62, length.out = 20) * c(-1,1)

x1 <- seq(31, 0, length.out = 11) * c(-1,1)

x <- c(x5, x4, x3, x2, x1)

l5 <- seq(-465, 310/1.62, length.out = 50)

l4 <- seq(-310, 186/1.62, length.out = 35)

l3 <- seq(-186, 93/1.62, length.out = 20)

l2 <- seq(-93, 31/1.62, length.out = 10)

lancuch <- data.frame(x = c(l5,l4,l3,l2),
                      y = c(seq(0,500,length.out = 50),seq(500,900,length.out = 35),seq(900,1200,length.out = 20),seq(1200,1400,length.out = 10)))

choina <- data.frame(x = x, y = y)

pien <- data.frame(x = rep(93, 21) * c(-1, 1),
                   y = seq(-200, 0, 10))

gwiazda <- data.frame(x = 0, y = 1500)

bombki <- data.frame(x = c(-100, 100, 300, -200, 0, 150, -100, 100, 30),
                     y = c(80, 270, 100, 400, 600, 530, 830, 950, 1250))

snieg <- data.frame(x = runif(100000, -1000, 1000),
                    y = runif(100000, -200, 1700),
                    t = seq(1, 1000, 1))

ggplot(choina, aes(x = x, y = y)) +
  geom_path(color = "#00852C", size = 1) +
  scale_y_continuous(limits = c(-200, 1700)) +
  scale_x_continuous(limits = c(-1000, 1000)) +
  geom_path(data = pien, aes(x = x, y = y), color = "#4B0007", size = 1) +
  geom_point(data = lancuch, aes(x = x, y = y), shape = "\u2666", size = 15, color = "#FFD700") +
  geom_point(data = gwiazda, aes(x = x, y = y), shape = "\U2735", size = 55, color = "yellow") +
  geom_point(data = bombki, aes(x = x, y = y), shape = "\U1F4A3", size = 30, color = "#BB0202") +
  theme_void() +
  theme(panel.background = element_rect(colour = "#030122", fill = "#030122")) +
  geom_point(data = snieg, aes(x, y), color = "white", pch = 42, size = 18) +
  transition_time(t) -> kartka

animate(kartka, nframes = 200, width = 1920, height = 1080, renderer = gifski_renderer('C:/Users/szlin/Desktop/Programowanie/TWD/HW 5/kartka.gif'))
