library(dplyr)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
inflacja<-data.frame(month=c("I","II","III","IV","V","VI","VII","VIII","IX","X"),
                     inflation=c(12.7,12.5,12.5,12.6,12.7,12.4,12.4,12.0,11.9,11.9))
wzrost_gosp<-data.frame(month=c("I","II","III","IV","V","VI","VII","VIII","IX","X"),
                        growth=c(0.95,0.95,0.85,0.65,0.55,0.65,1.0,1.1,0.85,0.45))
data<-merge(inflacja,wzrost_gosp,by="month")
pl1<-ggplot(inflacja,aes(x=month,y=inflation,group=T))+
  geom_line(color="orange")+
  ylim(c(11.5,13))+
  theme_bw()+
  labs(title="Zmiany konsensusu prognoz inflacji w 2023r. dla Polski")
 
pl2<-ggplot(wzrost_gosp,aes(x=month,y=growth,group=T))+
  geom_line(color="blue2")+
  ylim(c(0.4,1.3))+
  theme_bw()+
  labs(title="Zmiany konsensusu prognoz wzrostu gospodarczego w 2023r. dla Polski")


