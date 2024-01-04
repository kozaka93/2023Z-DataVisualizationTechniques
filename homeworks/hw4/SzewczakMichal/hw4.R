wynagrodzenia2<-read.csv("C:/mojepliki/PW/TWD/wynagr_hw4-v2.csv")
wynagrodzenia2$rok<-as.factor(wynagrodzenia2$rok)
wynagrodzenia2$stopien<-as.factor(wynagrodzenia2$stopien)
levels(wynagrodzenia2$stopien)
str(wynagrodzenia2)
library(plotly)
library(dplyr)
text<-ifelse(wynagrodzenia2$rok==2023,
             paste0(wynagrodzenia2$stopien,"<br>Średnie wynagrodzenie w ",wynagrodzenia2$rok," roku ",wynagrodzenia2$wynagrodzenie," zł.","<br> Wzrost o ",wynagrodzenia2$wzrost_kwota,"zł (",wynagrodzenia2$wzrost_proc,"%)."),
             paste0(wynagrodzenia2$stopien,"<br>Średnie wynagrodzenie w ",wynagrodzenia2$rok," roku ",wynagrodzenia2$wynagrodzenie," zł.")
)
wynagrodzenia2$stopien<-factor(wynagrodzenia2$stopien,levels = c("stażysta","kontraktowy","mianowany","dyplomowany"))
wynagrodzenia2_ord<-wynagrodzenia2[order(wynagrodzenia2$stopien),]
#install.packages("plotly")
fig2<-plot_ly(
  data=wynagrodzenia2_ord,
  x= ~stopien,
  y= ~wynagrodzenie,
  color= ~rok,
  colors=c("blue","green"),
  type="bar",
  height=500,
  hovertemplate=text
) %>% 
  layout(
    title=list(x=0.15,y=0.95,text="Wzrost średniego wynagrodzenia nauczycieli
               w stosunku do 2022 roku"),
    legend=list(x=0.7,y=-0.25,title=list(text="rok")),
    xaxis=list(title="stopień"),
    yaxis=list(title="średnie wynagrodzenie")
  )
fig2

