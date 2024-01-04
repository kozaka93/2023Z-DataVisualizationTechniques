library(plotly)
library(dplyr)
library(ggplot2)
wiek<-seq(1,99,1)
wiek2<-seq(101,199,1)
wiek2<-c(wiek,wiek2)
people<-read.csv("C:/mojepliki/PW/TWD/men.csv")
people<-people %>% 
  filter(men<0) 
people$age<-wiek
people<-people %>% 
  mutate(women= case_when(
    wiek<20 ~ -men*0.99,
    wiek<60 ~ -men*1.02,
    wiek<75 ~ -men*1.2,
    wiek>75 ~ -men*1.4
  ))
men2<-people$men
men2<-rep(men2,times=2)
people2<-as.data.frame(men2)
people2$age<-wiek2
people2$men<-case_when(
  people2$age<100 ~ people2$men,
  people2$age>100 ~ -people2$men*0.99,
  people2$age>120 ~ -people2$men*1.01,
  people2$age>140 ~ -people2$men*0.99,
  people2$age>160 ~ -people2$men*1.15,
  people2$age>170 ~ -people2$men*1.27,
  people2$age>180 ~ -people2$men*1.44,
  people2$age>190 ~ -people2$men*2.1
)
people2$age<-case_when(
  people2$age<100 ~ people2$age,
  people2$age>100 ~ people2$age-100
)
people2$gr<-case_when(
  people2$age<4 ~ "b",
  people2$age>3 ~ "g"
)
bombki_x<-rep(c(-10000,-29300,-278000,62000,134123,202202,0,-18000,-120120),times=198/9)
bombki_y<-rep(c(12,19,34,60,43,33,98,49,40),times=198/9)
bombki2_x<-rep(c(-100000,29300,278000,-12000,234123,202202,1000,-180000,120120),times=198/9)
bombki2_y<-rep(c(14,19,36,63,43,33,74,52,40),times=198/9)
people2$bombki_x<-bombki_x
people2$bombki_y<-bombki_y
people2$bombki2_x<-bombki2_x
people2$bombki2_y<-bombki2_y

snow_x<-rep(c(-266000,-200121,-59000,63000,122000,300000),times=198/6)
snow_y<-rep(c(100,80,92,94,82,74))
people2$snowx<-snow_x
people2$snowy<-snow_y
ggplot( people2 ) + 
  geom_bar(aes(x = age, y = men,fill=gr),stat ="identity",show.legend = FALSE) +
  geom_point(aes(x=bombki_y,y=bombki_x,size=2,colour="cyan"),show.legend = FALSE)+
  geom_point(aes(x=bombki2_y,y=bombki2_x,size=2,colour="red"),show.legend = FALSE)+
  coord_flip()+
  scale_fill_manual(values=c("brown","darkgreen"))+
  labs(x="",y="")+
  theme(panel.background = element_rect(fill="black", colour="black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
