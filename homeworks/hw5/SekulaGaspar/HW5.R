### TWD - HW 5 ###

### libraries

library(ggplot2)
library(gganimate)

### data frames

x <- c(0,20,10,30,15,40,20,50,-50,-20,-40,-15,-30,-10,-20,0)
y <- c(100,80,82,60,65,40,45,20,20,45,40,65,60,82,80,100)
choinka <- data.frame(x=x, y=y)

pien <- data.frame(x = c(10,10,-10,-10), y = c(20,5,5,20))

bombki1 <- data.frame(x = c( -4, -19, 10, -17, 8, -16, 30, 25), y = c( 73, 68, 62, 56, 36, 27, 30, 48))
bombki2 <- data.frame(x = c(-30, -2, -2, 13, 30, 4), y = c(25, 60, 40, 48, 30, 90))

gwiazda <- data.frame(x = c(0, 2.1, 6.5, 3.3, 4, 0, -4, -3.3, -6.5, -2.1, 0),
y = c(106.3, 102.3, 101.6, 98.4, 94, 96, 94, 98.4, 101.6, 102.3, 106.3))

l1x <- seq(-8,16,0.5)
l1y <- 0.037760416666667 * l1x^2 - 0.94791666666667 * l1x + 80
l2x <- seq(15, -20, -0.5)
l2y <- 0.02 * l2x^2 + 0.9 * l2x + 55
l3x <- seq(-19, 42, 0.5)
l3y <- 0.010416666666667 * l3x^2 - 0.54166666666667 * l3x + 30
lx <- c(l1x, l2x, l3x)
ly <- c(l1y, l2y, l3y)
flags <- rep(1:4, length.out = 243)

lancuch <- data.frame(x = lx, y = ly, flags = flags)

### plot

p <- ggplot()+
  geom_polygon(data = choinka, aes(x = x,y = y), fill = "darkgreen", color = "black")+
  geom_polygon(data = pien, aes(x = x,y = y), fill = "brown4", color = "black")+
  geom_point(data = bombki1, aes(x = x, y = y), color = "red", size = 7)+
  geom_point(data = bombki2, aes(x = x, y = y), color = "#BF40BF", size = 7)+
  geom_polygon(data = gwiazda, aes(x = x, y = y), color = "goldenrod", fill = "yellow")+
  geom_point(data = lancuch, aes(x = x, y = y), color = "gold", size = 2)+
  theme_void()+
  theme(plot.background = element_rect(fill = "navy", color = NA))+
  transition_states(flags, transition_length = 0.5,
                     state_length = 0.1)

animate(p, renderer = gifski_renderer("choinka.gif"))
