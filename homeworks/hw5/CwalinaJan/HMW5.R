library(ggplot2)
# Generowanie punktów dla "choinki"
choinka <- data.frame(
  y = c(rep(2,100),
        rep(3,80),
        rep(4,60),
        rep(5,80),
        rep(6,60),
        rep(7,40),
        rep(8,60),
            rep(9,40),
        rep(10,20),
        rep(11,1)),type = 5)

prez <- data.frame(x = rnorm(50,0.5),y = rep(c(4.75,5.15),each = 25),Prezenty = rep(c("Prezent 1","Prezent 2"),each = 25) )

wspolrzedne_gwiazdy <- data.frame(x = c(5.000000,
                                        4.911832,
                                        4.714683,
                                        4.857342,
                                        4.823664,
                                        5.000000,
                                        5.176336,
                                        5.142658,
                                        5.285317,
                                        5.088168,
                                        5.000000),
                                  y = c(11.80000,
                                        11.5,
                                        11.2,
                                        10.9,
                                        10.2,
                                        10.5,
                                        10.2,
                                        10.9,
                                        11.2,
                                        11.5,
                                        11.80000))
# Tworzenie wykresu
ggplot() +
  geom_polygon(aes(x = c(4.5,4.5,5.5,5.5),y = c(-1,0.2,0.2,-1)),fill = "white") +
  geom_polygon(aes(x = c(4.9,4.9,5.1,5.1),y = c(0,2,2,0)),fill = "#613111") +
  geom_boxplot(data = prez,aes(x = y,y = x,fill = Prezenty, color = Prezenty)) +
  scale_fill_manual(values = c("red2","orange")) + 
  scale_color_manual(values = c("yellow2","blue")) + 
  geom_violin(data = choinka,aes(x = type,y = y),fill = "green4") +
  theme_minimal() +
  labs(title = "Violine,polygon,boxplot,point w kształcie choinki",x = "Święta",y = "Święta") + 
  theme(panel.background = element_rect(fill = "#121650"),panel.grid = element_blank()) + 
  geom_point(aes(x = c(4.70,5.1,5.2,4.8,5.04,4.89,5.02),
                 y = c(3,3.01,2.97,4.5,4.7,8,8.7)),
                 color = c("red", "green", "blue", "gold", "purple", "orange", "pink"),
             size = 8,alpha = 0.7) + 
  geom_polygon(data = wspolrzedne_gwiazdy, aes(x = x, y = y), fill = "yellow", color = "black") +
  geom_point(data = data.frame(x = rnorm(150,5,0.5),y = rnorm(150,6,4)),aes(x = x,y = y),shape = 8,color = 'white') + 
  xlim(4.5,5.5) + ylim(-1,14)
  


