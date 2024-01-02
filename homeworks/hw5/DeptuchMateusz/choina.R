tree <-data.frame(
   x = c(0,100,50,75,25,50,10,25,0, -25, -10, -50, -25, -75, -50, -100),
   y = c(0,0, 15,15,30,30,45,45,60, 45, 45, 30, 30, 15, 15,0)
)

pien <- data.frame(
  x = c(-10, 10, 10, -10),
  y= c(0,0,-10,-10)
)

library(ggplot2)
library(patchwork)
library(gganimate)


swiatelka <- data.frame(
  x = seq(from = -50, to = 25, length.out = 30),
  y = seq(from = 15, to = 30, length.out = 30)
)
stan = rep(c("a","b","c"),10)
swiatelka <- cbind(swiatelka, stan) 

swiatelka2 <- data.frame(
  x = seq(from = -25, to = 10, length.out = 30),
  y = seq(from = 30, to = 45, length.out = 30)
)
swiatelka2<- cbind(swiatelka2, stan) 
swiatelka3 <- data.frame(
  x = seq(from = -50, to = 50, length.out = 30),
  y = seq(from = 0, to = 15, length.out = 30))
swiatelka3 <- cbind(swiatelka3, stan) 

gwiazdka <- data.frame(
  x = c(0,-2,-15,-2,0,2,15,2),
  y = c(-10,-1,0,1,10,1,0,-1) +60
)

bombki <- data.frame(
  x = c(20,5,-70),
  y = c(35, 14, 4)
)
bombki2 <- data.frame(
  x = c(50, -33,-15),
  y = c(6, 10,28)
)

choinka <- ggplot() + 
geom_polygon(data = tree, aes(x = x, y = y), fill = "darkgreen")+
  geom_polygon(data = pien, aes(x = x, y = y), fill = "brown") +
  geom_point(data =swiatelka, aes(x = x, y=y),color = "yellow") +
  geom_point(data =swiatelka2, aes(x = x, y=y),color = "yellow") + 
  geom_point(data =swiatelka3, aes(x = x, y=y),color = "yellow") +
  geom_polygon(data = gwiazdka, aes(x,y), fill = "yellow", color = "black")+
  geom_point(data = bombki, aes(x,y), size = 10, shape = 16, color = "red")+
  geom_point(data = bombki2, aes(x,y), size = 10, shape = 16, color = "blue")+
theme_void()+
  transition_states(stan,
                    transition_length = 0,
                    state_length = 1)

a <- animate(choinka, fps = 30, nframes = 100)
a
anim_save("choina.gif")

