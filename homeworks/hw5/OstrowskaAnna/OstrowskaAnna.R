# Instalacja i załadowanie potrzebnych bibliotek
install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)
library(tidyr)

set.seed(123)

# Generowanie punktów dla x<=0
x1l <- runif(500, min = -5, max = 0)
x2l <- runif(1300, min = -10, max = 0)
x3l <- runif(2000, min = -15, max = 0)

generate_y <- function(x, min, max) {
  y <- numeric(length(x))
  for (i in seq_along(x)){
    y[i] <- runif(1, min=min, max=max[i])
  }
  return(y)
}
y1l <- generate_y(x1l, 23, (x1l+29))
y2l <- generate_y(x2l, 13, (x2l+24))
y3l <- generate_y(x3l, 0, (x3l+15))
# Połączenie zbiorów danych
data_l <- rbind(cbind(x1l, y1l), cbind(x2l, y2l), cbind(x3l, y3l))

# Generowanie punktów dla x>0
x1r <- runif(500, min = 0, max = 5)
x2r <- runif(1300, min = 0, max = 10)
x3r <- runif(2000, min = 0, max = 15)

y1r <- generate_y(x1r, 23, (-x1r+29))
y2r <- generate_y(x2r, 13, (-x2r+24))
y3r <- generate_y(x3r, 0, (-x3r+15))
# Połączenie zbiorów danych
data_r <- rbind(cbind(x1r, y1r), cbind(x2r, y2r), cbind(x3r, y3r))

#polaczenie obu stron - lewej i prawej od x=0
data_all <- as.data.frame(rbind(data_r, data_l))
colnames(data_all) <- c("x","y")

#rysowanie choinki (prostej)
data_all %>% 
  ggplot(aes(x = x, y = y)) +
    geom_point( color = "darkgreen", fill = "darkgreen") +
    theme_void() +
    theme(legend.position = "none") +
    coord_fixed(ratio = 1) -> tree
#dodanie gwiazdy
tree_with_star <- tree +
  annotate("text", x = 0, y = 29.5, label= "23", size = 8, color = "#D0B844", fontface = "bold")
#dodanie tla, prezentow,sniegu
snow_points <- data.frame(x = runif(300, min=-19, max=19), y = runif(300, min=-2, max=31))
snow <- data.frame(x=runif(600, min=-19, max=19), y = runif(600, -2.5, -1))

ggplot()+
  geom_point(aes(x=0, y=-1.1), color = "#7F4D22", fill = "#7F4D22", shape=15, size=11)+
  geom_point(data=data_all,aes(x=x,y=y), color = "darkgreen", fill = "darkgreen") +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1)+
  geom_point(data=snow_points, aes(x=x, y = y),color = "white", size = 1, shape=8) +
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank()) +
  annotate("text", x = -0.5, y = 29.5, label= "'23", size = 7.5, color = "#D0B844", fontface = "bold")+
  geom_point(data=snow, aes(x=x, y = y),color = "white", size = 1, shape=8) +
  geom_point(aes(x=-5.6, y=0.9), color="#C11F50", shape=15, size=6.5)+
  geom_point(aes(x=-5.6, y=0.9), color="pink", shape=4, size=6.2)+
  geom_point(aes(x=-4, y=-2), color="#BC1FC1", shape=15, size=7)+
  geom_point(aes(x=-4, y=-2), color="darkblue", shape=3, size=4.7)+
  geom_point(aes(x=-7.3, y=-2), color="#223E7F", shape=15, size=7)+
  geom_point(aes(x=-7.3, y=-2), color="black", shape=3, size=4.7)-> tree2
#dodam jeszcze bombki z danymi o swietach, zrodlo:https://superbiz.se.pl/wiadomosci/nie-choinka-i-nie-prezenty-oto-ulubione-zwyczaje-swiateczne-polakow-aa-CmmC-b5dt-ZHjf.html
line <- "|"
text<- "97%\nPolaków\nobchodzi\nŚwięta"
text2<- "85%\ndzieli się\nopłatkiem"
text3 <- "83%\nubiera\nchoinkę"
text4 <- "78%\ndaje\nprezenty"
text5<-"66%\nje karpia"


# Tworzenie wykresu koncowego
tree2+
  annotate("text", x=-1, y = 20.6, label=line, size=7, vjust = 0.25, fontface="bold", color="gold")+
  geom_point(aes(-1,20.2), color="#BC1FC1", shape=19, size=9.9)+
  annotate("text", x=-1, y = 19.4, label=text, size=1.31, vjust = 0.25, fontface="bold")+
  annotate("text", x=4, y = 4.8, label=line, size=7, vjust = 0.25, fontface="bold", color="gold")+
  geom_point(aes(4,4.5), color="#C11F50", shape=19, size=9.6)+
  annotate("text", x=4, y = 4.1, label=text2, size=1.28, vjust = 0.25, fontface="bold")+
  annotate("text", x=3, y = 23.85, label=line, size=6, vjust = 0.25, fontface="bold", color="gold")+
  geom_point(aes(3,23.7), color="#C11F50", shape=19, size=6.9)+
  annotate("text", x=3.06, y = 23.5, label=text5, size=1.13, vjust = 0.25, fontface="bold")+
  annotate("text", x=-2.4, y = 10.1, label=line, size=7, vjust = 0.25, fontface="bold", color="gold")+
  geom_point(aes(-2.4,10.2), color="#DC5184", shape=19, size=7.7)+
  annotate("text", x=-2.4, y = 9.84, label=text4, size=1.08, vjust = 0.25, fontface="bold")+
  annotate("text", x=5, y = 15.48, label=line, size=7, vjust = 0.25, fontface="bold", color="gold")+
  geom_point(aes(5,15.2), color="#DC51C1", shape=19, size=9.2)+
  annotate("text", x=5, y = 14.8, label=text3, size=1.25, vjust = 0.25, fontface="bold")+
  labs(title = "Tradycje świąteczne Polaków")+
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=14, y=-4, label="źródło:https://superbiz.se.pl/wiadomosci", size=1.2)+
  coord_fixed()-> static_tree
ggsave("choinka.jpg", static_tree, width=5.4, height=2.82)
  

