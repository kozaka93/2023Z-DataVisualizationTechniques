library(dplyr)
library(ggplot2)


swiat=c(-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11)
b=c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10)
b2=40-abs(4*b)
wesaolych=60-abs(5*x)
df=data.frame(x,y)
df2=data.frame(b,b2)
ggplot(df,aes(x,y))+
  geom_point(shape="I",size=10,color="beige")+
  geom_point(aes(x,y+2),shape="*",size=7,color="orange")+
  geom_col(color="darkgreen",fill="darkgreen",width=1)+
  theme(panel.background = element_rect(fill = '#00013D', color = '#00013D'),panel.grid = element_blank(),panel.border = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),plot.background =element_rect(fill = '#00013D', color = '#00013D') )+
  geom_point(aes(0,62),size=27,shape="\u2605",color="gold")
