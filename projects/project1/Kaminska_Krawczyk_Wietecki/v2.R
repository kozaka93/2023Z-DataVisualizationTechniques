library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)


#setwd('C:/Users/Agata Krawczyk/Desktop/italian gastronomich recipes dataset')
#cheese_per_cap1 <- read.csv('cheese_per_cap1.csv',sep=";")

cheese_per_cap <- cheese_per_cap1 %>% 
  select("Year","Cheddar","Other_than_cheddar","Total_am","Mozzarella","Other_than_mozzarella","Total_it") %>% #,"Swiss.2","Blue.3","Brick","Muenster","Cream.and.Neufchatel","Hispanic.4","Other") %>% 
  filter(!is.na(Year))
cheese_per_cap <- cheese_per_cap %>% 
  mutate("year2"=as.numeric(Year)) %>% 
  mutate("cheddar"=as.numeric(str_replace(Cheddar,",","\\."))) %>% 
  mutate("inne amerykańskie"=as.numeric(str_replace(Other_than_cheddar,",","\\."))) %>% 
  mutate("amerykańskie"=as.numeric(str_replace(Total_am,",","\\."))) %>% 
  mutate("mozzarella"=as.numeric(str_replace(Mozzarella,",","\\."))) %>% 
  mutate("inne włoskie"=as.numeric(str_replace(Other_than_mozzarella,",","\\."))) %>% 
  mutate("włoskie"=as.numeric(str_replace(Total_it,",","\\."))) %>% 
  select(year2,cheddar,mozzarella,"inne amerykańskie","amerykańskie","inne włoskie","włoskie")

total_cons <- cheese_per_cap1 %>% 
  select("Year","All_cheese","Total_am") %>% #,"Swiss.2","Blue.3","Brick","Muenster","Cream.and.Neufchatel","Hispanic.4","Other") %>% 
  filter(!is.na(Year)) %>% 
  mutate("year2"=as.numeric(Year)) %>% 
  mutate("total"=as.numeric(str_replace(All_cheese,",","\\."))) %>% 
  mutate("total_american"=as.numeric(str_replace(Total_am,",","\\.")))
  
p4 <- total_cons %>% 
  ggplot()+
  geom_ribbon(fill="darkgoldenrod",aes(x=year2,ymin=total_american*0.45,ymax=total*0.45),alpha=0.9)+
  geom_ribbon(fill="#ffc425",aes(x=year2,ymin=0,ymax=total_american*0.45),alpha=0.9)+
  theme(
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA), 
    legend.background = element_rect(fill='transparent'), 
    legend.box.background = element_rect(fill='transparent'),
    axis.text = element_text(color="#540606",size = 9),
    axis.title = element_text(size=14,color="#540606"),
    title = element_text(size=20),
    panel.grid.major = element_line(color="darkgoldenrod",linewidth = 0.7),
    panel.grid.minor = element_line(color="darkgoldenrod",linewidth=0.1)
  )+
  scale_x_continuous(expand=c(0,0.5))+
  scale_y_continuous(expand=c(0,0.6))+
  labs(x="Rok",y="Kilogramy per capita")

p4
ggsave('p4.png', p4, bg='transparent',units = "px",height=1000,width=1500)

r1970 <- cheese_per_cap %>% 
    filter(year2==1970) %>% 
    select(-year2)

  
z1 <-as.data.frame(t(r1970)) 

p1 <- ggplot(z1, aes(x=rownames(z1),y=V1*0.45))+
  geom_col(fill="#ffc425")+
  scale_y_continuous(limits = c(0,8))+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    axis.text.x = element_text(color="#540606",size = 10,angle = 45,vjust=1.1,hjust=1),
    axis.text.y = element_text(color="#540606",size = 10),
    axis.title.y = element_text(size=14,color="#540606"),
    axis.title.x = element_blank(),
    title = element_text(size=20),
    panel.grid.major.y =  element_line(color="darkgoldenrod",linewidth = 0.1),
    panel.grid.minor.y = element_line(color="darkgoldenrod",linewidth=0.1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks =element_blank() 
    #transparent legend panel
  )+
  labs(y="Kilogramy per capita")
p1
ggsave('1970.png', p1, bg='transparent',units = "px",height=1000,width=1000)

r2008 <- cheese_per_cap %>% 
  filter(year2==2008) %>% 
  select(-year2)

z2 <-as.data.frame(t(r2008)) 

p2 <- ggplot(z2, aes(x=rownames(z2),y=V1*0.45))+
  geom_col(fill="#ffc425")+
  scale_y_continuous(limits = c(0,8))+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    axis.text.x = element_text(color="#540606",size = 10,angle = 45,vjust=1.1,hjust=1),
    axis.text.y = element_text(color="#540606",size = 10),
    axis.title.y = element_text(size=14,color="#540606"),
    axis.title.x = element_blank(),
    title = element_text(size=20),
    panel.grid.major.y =  element_line(color="darkgoldenrod",linewidth = 0.1),
    panel.grid.minor.y = element_line(color="darkgoldenrod",linewidth=0.1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks =element_blank() 
    #transparent legend panel
  )+
  labs(y="Kilogramy per capita")
p2
ggsave('2008.png', p2, bg='transparent',units = "px",height=1000,width=1000)



r2021 <- cheese_per_cap %>% 
  filter(year2==2021) %>% 
  select(-year2)

z3 <-as.data.frame(t(r2021)) 

p3 <- ggplot(z3, aes(x=rownames(z3),y=V1*0.45))+
  geom_col(fill="#ffc425")+
  scale_y_continuous(limits = c(0,8))+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    axis.text.x = element_text(color="#540606",size = 10,angle = 45,vjust=1.1,hjust=1),
    axis.text.y = element_text(color="#540606",size = 10),
    axis.title.y = element_text(size=14,color="#540606"),
    axis.title.x = element_blank(),
    title = element_text(size=20),
    panel.grid.major.y =  element_line(color="darkgoldenrod",linewidth = 0.1),
    panel.grid.minor.y = element_line(color="darkgoldenrod",linewidth=0.1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks =element_blank() 
    #transparent legend panel
  )+
  labs(y="Kilogramy per capita")
p3
ggsave('2021.png', p3, bg='transparent',units = "px",height=1000,width=1000)

