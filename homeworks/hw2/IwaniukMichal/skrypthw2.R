library(tidyverse)
library(colorspace)
data <- data.frame(nazwa = c("Koalicja Obywatelska", "Prawo i Sprawiedliwość",
                     "Trzecia Droga","Lewica","Konfederacja"),
           wartosc = c(44.35,22.83,15.37,8.02,6.30))
labels = paste(data$wartosc,"%",sep="")
labels[5]<-"6.30%"
data$nazwa = fct_reorder(data$nazwa,data$wartosc,.desc=T)
a <- c("#009ACD","#ff7f00","#ffc125","#ee2c2c","#698b22")

data %>% ggplot(aes(x=nazwa,y=wartosc,fill=nazwa))+geom_col()+
  geom_text(aes(label=labels),vjust=-0.5,fontface="bold",size=8)+
  theme_minimal()+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 28,color="#696969"),
        axis.text.x = element_text(face="bold",size=14))+
  coord_cartesian(ylim = c(0, 48))+
  scale_y_continuous(expand = c(0,0))+
  labs(title = "Wyniki głosowania na terenie gminy Piaseczno")+
  scale_fill_manual(values=a)+
  theme(legend.position = "none")
  
  

    