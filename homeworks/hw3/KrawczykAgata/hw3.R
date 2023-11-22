library(dplyr)
library(ggplot2)
library(maps)

setwd('C:/Users/Agata Krawczyk/Desktop/hw3')
df <- read.csv('data.csv',sep=";")
c <- read.csv('Countries-Europe.csv',sep=";")

colnames(df) <- c("region","Year","code","pe")
w2hr <- map_data("world")  


df2 <- inner_join(df,w2hr,by="region") %>% 
  mutate("p"=as.numeric(str_replace(pe,",","\\."))) %>% 
  filter(region %in% c$name)
  
  
ggplot() + 
 geom_polygon(data = df2, aes(x = long, y = lat, group = group, fill=p),color="white")+
  coord_map("mercator")+
  scale_fill_steps(n.breaks=6,limits=c(20,60),name="procent kobiet",low="#FFD4DE",high="#59071B")+
  scale_y_continuous(limits = c(30,72),expand=c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme( panel.background = element_rect(fill='white'), 
         plot.background = element_rect(fill='white', color=NA), 
         legend.background = element_rect(fill='white'), 
         legend.box.background = element_rect(fill='white'),
         axis.text = element_text(color="black",size = 9),
         axis.title = element_blank(),
         title = element_text(size=20),
         panel.grid.major = element_line(color="gray",linewidth = 0.1),
         panel.grid.minor = element_line(color="gray",linewidth=0.1),
         legend.title = element_text(size=10),
         plot.title = element_text(size=14)
         )+
  labs(title="Udział procentowy kobiet wśród absolwentów kierunków STEM")

