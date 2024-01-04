library(ggimage)
library(ggplot2)
setwd("C:/Users/macie/Desktop/STUDIA/SEMESTR3/Techniki Wizualizacji Danych/Prace Domowe/Praca domowa choinka")

points <- data.frame(
  x = c(0, 50, 100, 0),
  y = c(100, 300, 100, 100)
)

# Przykładowe dane dla wnętrza obszaru
points_pien <- data.frame(
  x = c(37.5, 62.5, 62.5, 37.5, 37.5),
  y = c(100, 100, 75, 75, 100)
)
points_lancuch1 <- data.frame(
  x = c(5,16,25,37.5,50,62.5,75,87.5),
  y = c(115,130,160,140,165,150,175,150)
)
star_points <- data.frame(
  x=50,
  y=298
)
points_lancuch2 <- data.frame(
  x=c(25,37.5,45,60,67),
  y=c(200,180,220,200,230)
)
points_babeczki1 <- data.frame(
  x=c(50,75,45),
  y=c(120,182,250)
)
points_babeczki2 <- data.frame(
  x=c(25,55),
  y=c(140,225)
)
points_babeczki3 <- data.frame(
  x=c(52,75,62.5),
  y=c(265,125,162)
)
points_cukierek <- data.frame(
  x=c(50,37.5,27,80),
  y=c(180,215,111,107)
)
points_snieg <- data.frame(
  x=c(0,6.25,12.5,19,25,55,62.5,70,75,80,87.5,93,100),
  y=c(150,300,225,175,275,290,305,230,255,300,240,175,115)
)
# Stwórz wykres ścieżki i wypełnij obszar kolorem
ggplot() +
  geom_polygon(data = points, aes(x = x, y = y), fill = "green", alpha=0.5) +
  geom_polygon(data = points_pien, aes(x = x, y = y), fill = "brown", alpha = 0.5) +
  geom_path(data=points_lancuch1, aes(x=x,y=y), color="gold", size = 4) +
  geom_point(data=star_points, aes(x=x,y=y), shape="square", color="yellow", size=10) +
  geom_point(data=star_points, aes(x=x,y=y), shape="diamond", color="yellow", size=15) +
  geom_path(data=points_lancuch2, aes(x=x,y=y), color="gold", size = 4) +
  geom_point(data=points_babeczki1, aes(x=x,y=y), color="blue", size=5) +
  geom_point(data=points_babeczki2, aes(x=x,y=y), color="red", size=5) +
  geom_point(data=points_babeczki3, aes(x=x,y=y), color="purple", size=5) +
  geom_point(data=points_snieg, aes(x=x,y=y), color="white", size=5, shape = "cross") +  
  geom_point(data=points_snieg, aes(x=x,y=y), color="white", size=5, shape = "plus") +
  geom_image(data=points_cukierek,aes(x=x, y=y, image="C:/Users/macie/Desktop/STUDIA/SEMESTR3/Techniki Wizualizacji Danych/Prace Domowe/Praca domowa choinka/cukierek_bt.png"),size=0.1) +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank())
