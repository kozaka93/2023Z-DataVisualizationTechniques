library(ggplot2)
library(dplyr)
library(ggstar)
library(ggimage)
library(ggplotify)
library(yulab.utils)
library(magick)

x1 = data.frame(x = c(-0.5,0,0.5), y = c(0.7,1,0.7))
x2 = data.frame(x = c(-0.7, 0, 0.7), y = c(0.4, 0.8, 0.4))
x3 = data.frame(x = c(-1, 0, 1), y = c(0, 0.5, 0))
x4 = data.frame(x = c(-0.2,0.2), y = c(-0.15, -0.15))
prezent1 = data.frame(x = c(0.3, 0.3, 0.5, 0.5), y = c(-0.15, -0.05, -0.05, -0.15))
prezent2 = data.frame(x = c(0.38, 0.38, 0.42, 0.42), y = c(-0.15, -0.05, -0.05, -0.15))
prezent3 = data.frame(x = c(0.3, 0.3, 0.5, 0.5), y = c(-0.11, -0.09, -0.09, -0.11))

sniegX <- runif(200, -1, 1)
sniegY <- runif(200, -0.2, 1)
snieg <- data.frame(sniegX, sniegY)
gwiazda = data.frame(x = c(0), y = c(1))
bombka1 <- data.frame(x = -0.2,y = 0.8, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka2.png")
bombka2 <- data.frame(x = 0.1,y = 0.6, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka2.png")
bombka3 <- data.frame(x = -0.25,y = 0.48, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka2.png")
bombka4 <- data.frame(x = 0.2,y = 0.2, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka2.png")
bombka5 <- data.frame(x = 0.4,y = 0.1, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka3.png")
bombka6 <- data.frame(x = -0.25,y = 0.3, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka3.png")
bombka7 <- data.frame(x = 0.2,y = 0.8, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka3.png")
bombka8 <- data.frame(x = -0.45,y = 0.1, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka4.png")
bombka9 <- data.frame(x = 0.4,y = 0.47, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka4.png")
bombka10 <- data.frame(x = -0.23,y = 0.6, image = "C:/studia/2rok/TWD/homeworkChoinka/bombka4.png")

ggplot()+
  geom_polygon(data = x1, aes(x = x, y = y),fill = "darkgreen") +
  geom_polygon(data = x2, aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = x3, aes(x = x, y = y), fill = "darkgreen") +
  geom_polygon(data = prezent1, aes(x = x, y = y), fill = "red") +
  geom_polygon(data = prezent2, aes(x = x, y = y), fill = "yellow") +
  geom_polygon(data = prezent3, aes(x = x, y = y), fill = "yellow") +
  geom_area(data = x4, aes(x = x, y = y),fill = "brown") +
  geom_star(gwiazda, mapping = aes(x = x, y = y), colour = "yellow", fill = 'yellow', size = 15)+
  geom_image(data = bombka1, aes(x = x, y = y, image = image), size = 0.06) +
  geom_image(data = bombka2, aes(x = x, y = y, image = image), size = 0.06) +
  geom_image(data = bombka3, aes(x = x, y = y, image = image), size = 0.06) +
  geom_image(data = bombka4, aes(x = x, y = y, image = image), size = 0.06) +
  geom_image(data = bombka5, aes(x = x, y = y, image = image), size = 0.1) + 
  geom_image(data = bombka6, aes(x = x, y = y, image = image), size = 0.1) + 
  geom_image(data = bombka7, aes(x = x, y = y, image = image), size = 0.1) + 
  geom_image(data = bombka8, aes(x = x, y = y, image = image), size = 0.12) +
  geom_image(data = bombka9, aes(x = x, y = y, image = image), size = 0.12) +
  geom_image(data = bombka10, aes(x = x, y = y, image = image), size = 0.1) +
  geom_point(data = snieg, aes(x = sniegX, y = sniegY), color = "white", shape = 8) +
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'blue')) +
  labs(title = "Wesołych Świąt") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, colour = "white"))
  
  
