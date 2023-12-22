library(dplyr)
library(ggplot2)
library(gganimate)
library(transformr)

x<- c(1,20,2,19,5,16,6.5,14,8.5,12,9.5) #punkty do łączenia dla choinki
y<-c(1,1,2,2,3,3,4,4,4.75,4.75,5.25)
drzewko<- data.frame(x,y)

ggplot()+
  geom_path(data = drzewko,aes(x=x,y=y),color = "darkgreen", size = 4)+
  labs(title="WESOŁYCH ŚWIĄT!")+
  theme(plot.title = element_text(color="#a62d1c",hjust=0.47, size = 15),
      axis.text.x = element_blank(), #usuniecie wszystkich elementow zeby zostalo tylko tło
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.text = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill="black"),
      plot.background = element_rect(fill="black"))->choinka

 
#nalozenie swiatelek
ze_swiatelkami<-choinka+
  geom_path(data=drzewko, aes(x=x,y=y),linetype = "dotted", color = "yellow", size=1.2)


#linie gwiazdy 
gwiazda1<-data.frame(x1=c(9.73,9.73),y1=c(4.87,5.8))
gwiazda2<-data.frame(x2=c(8.35,11.15),y2=c(5.37,5.37))
gwiazda3<- data.frame(x3=c(10.7,8.7),y3=c(5.7,5))
gwiazda4<- data.frame(x4=c(8.7, 10.7),y4=c(5.7,5))


ze_swiatelkami+
  geom_path(data = gwiazda1, aes(x=x1,y=y1),color="#ecd621", size =2.5)+
  geom_path(data = gwiazda2, aes(x=x2,y=y2),color="#ecd621", size =2.5)+
  geom_path(data = gwiazda3, aes(x=x3,y=y3),color="#ecd621", size =1.6)+
  geom_path(data = gwiazda4, aes(x=x4,y=y4),color="#ecd621", size =1.6)->z_gwiazda


#punkty bombek
bombki_1<-data.frame(x=c(5,15,11,7.5,13,10), y=c(2,2,3,4,4,4.55))
bombki_2<- data.frame(x=c(10,12.5,10.5,17,6.75), y=c(1.6,2.5,3.6,1.2,3))

z_bombkami<-z_gwiazda+
  geom_point(data=bombki_1, aes(x=x,y=y), color="#a62d1c", size = 6)+
  geom_point(data=bombki_2, aes(x=x,y=y), color="#e5ce29", size = 6.5, shape = "diamond")


sniezynki<- data.frame(x=c(2.7,3,4,4.1,5,6,1,2,3,1.2,1.1,1.3,6.1,1,11.5,12.5,15,14,15.5,15.4,16.3,17,17.6,18,17.5,18.3,19.5,19.4,19.9,7.4,20.1,20.1,20.2),
                       y=c(4.95,4.1,5.5,3.7,4.5,5.7,2.5,3.3,2.7,3.9,4.7,5.7,4.9,1.5,5.7,5,4.8,5.5,4,5.7,5.1,3.5,2.7,5.4,4.5,3.9,3,4.9,5.8,5.3,4.2,2.1,3.5))
final<-z_bombkami+
  geom_point(data=sniezynki, aes(x=x,y=y), color ="white", size =4, shape ="asterisk")

final
