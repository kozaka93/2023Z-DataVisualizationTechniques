library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)


setwd('C:/Users/Agata Krawczyk/Desktop/HW2')
players <- read.csv("wta_players.csv")
rankings <- read.csv("wta_rankings_current.csv")
top_5 <- players %>% 
  filter(name_last %in% c("Swiatek","Sabalenka","Gauff","Rybakina","Pegula")) %>% 
  mutate(tenisitka=str_c(name_last,name_first,sep=" "))

         
colnames(top_5)[1] <- "player"         

x <- rankings %>% 
  filter(player %in% top_5$player) %>% 
  mutate(week=lubridate::week(ymd(as.Date(as.character(ranking_date), format = "%Y%m%d")))) %>% 
  mutate(months=month.name[month(as.Date(as.character(ranking_date), format = "%Y%m%d"))]) %>% 
  mutate(date=as.Date(as.character(ranking_date), format = "%Y%m%d"))


merged <- inner_join(top_5,x,by="player")
merged$tenisitka <- factor(merged$tenisitka,                 # Relevel group factor
                         levels = c("Sabalenka Aryna","Swiatek Iga","Gauff Cori","Pegula Jessica","Rybakina Elena"))

nazwa <- c("1","2","3","4")
dat <- c(as.Date("2023-01-29"),as.Date("2023-06-11"),as.Date("2023-07-16"),as.Date("2023-09-10"))
slam <- data.frame(nazwa,dat)
ggplot(merged,aes(x=date,y=points,color=tenisitka))+
  scale_x_date(minor_breaks = "1 week", 
               date_labels = "%B",expand = c(0.005,0),
               date_breaks = "1 month")+
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000),limits=c(0,12000),expand=c(0,0))+
  geom_vline(data=slam,aes(xintercept=dat+1,color=nazwa),
             color="#848484",
             linewidth=1)+
  annotate("text", x=as.Date("2023-01-29")-3, y=1100, label="Australian Open", angle=90, size=2.5,fontface="bold",color="#848484")+
  annotate("text", x=as.Date("2023-06-11")-3, y=1000, label="Roland Garros", angle=90, size=2.5,fontface="bold",color="#848484")+
  annotate("text", x=as.Date("2023-07-16")-3, y=800, label="Wimbledon", angle=90, size=2.5,fontface="bold",color="#848484")+
  annotate("text", x=as.Date("2023-09-10")-3, y=600, label="US Open", angle=90, size=2.5,fontface="bold",color="#848484")+
  
  theme(axis.text.x =element_text(vjust = 3,hjust=-0.1))+
  scale_color_manual(values=c("Sabalenka Aryna"="#73C247","Swiatek Iga"="#7fcdbb","Gauff Cori"="#41b6c4","Pegula Jessica"="#2c7fb8","Rybakina Elena"="#253494"))+
  theme(panel.grid.minor.x=element_line(color="#B1B1B2",linetype="dotted" , linewidth = 0.2),
        panel.grid.major.x =element_line(color="#B1B1B2",linetype="solid",linewidth=0.5),
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y=element_line(color="#B1B1B2",linetype="dotted" , linewidth = 0.2),
        panel.grid.major.y =element_line(color="#B1B1B2",linetype="solid",linewidth=0.5))+
  geom_line(size= 1)+
  geom_point(size=1.6)+
  
  labs(title="Zmiany w ilości punktów WTA dla obecnych top 5 tenisistek na przestrzeni 2023 roku",x="miesiąc",y="liczba punktów")
  

