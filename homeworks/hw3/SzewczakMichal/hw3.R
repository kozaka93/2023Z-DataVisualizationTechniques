library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
install.packages("mapdata")
world<-map_data("world")
pkb<-read.csv("C:/mojepliki/PW/TWD/pkb_per_capita_2023_edit3.csv",sep=",")
pkb<-pkb %>% 
  mutate(GDP_per_capita_in_2023=case_when(
    GDP_per_cap<500 ~ "under 500",
    GDP_per_cap<2500 ~ "500-2 500",
    GDP_per_cap<10000 ~"2 500-10 000",
    GDP_per_cap<25000 ~ "10 000-25 000",
    GDP_per_cap<50000 ~ "25 000-50 000",
    GDP_per_cap>=50000 ~ "over 50 000",
    
  ))
pkb_to_map<-merge(world,pkb,by.x="region",by.y="Country")
pkb_to_map$GDP_per_capita_in_2023<-as.factor(pkb_to_map$GDP_per_capita_in_2023)
pkb_to_map$GDP_per_capita_in_2023<-ordered(pkb_to_map$GDP_per_capita_in_2023,levels=c("over 50 000","25 000-50 000","10 000-25 000","2 500-10 000","500-2 500","under 500"))
pkb_to_map<-pkb_to_map %>% 
  arrange(order)

plot2<-pkb_to_map %>% 
  filter(region!="Antarctica") %>% 
  ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=GDP_per_capita_in_2023),color="black",linewidth=0.1)+
  scale_fill_manual(values = c("green4","springgreen2","palegreen","lightsalmon","darkorange","darkred"),na.value = "grey", name="GDP per capita (USD)")

plot2<-plot2+coord_fixed(1.3)
plot2<-plot2+theme_bw()
plot2<-plot2+labs(title="GDP per Capita in 2023 in USD",subtitle = "IMF Data")
plot2+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5,size = 12),legend.title = element_text(size=10))




