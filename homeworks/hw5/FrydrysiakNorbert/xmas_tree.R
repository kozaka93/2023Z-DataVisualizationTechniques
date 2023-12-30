library(dplyr)
library(ggplot2)
library(gganimate)
library(tidyr)
library(patchwork)
library(grid)

#x=rnorm(30, mean=2, sd=1)
x=runif(30, min=0, max=1) # lepiej z rozkładu jednostajnego, bo bardziej
# symetryczny i ładna choinka, no i fajnie brać te dane losowo, bo nie ma idealnej
# choinki
y=1:length(x)
df=data.frame(x,as.character(y))
#xx=runif(30, min=0, max=1)
#yy=xx*xx
#bomki=data.frame(xx,yy)

df %>% 
  arrange(desc(x)) %>% 
  ggplot(aes(x=y,y=x))+
  geom_col(color='black', fill='green')+
  theme_void()+
  coord_flip()->p1

p2=p1+scale_y_reverse()


p=(p2+plot_spacer()+p1+plot_layout(widths = c(4, -0.4, 4)))/plot_spacer()+plot_layout(height=c(9,0.2))

p
grid.draw(linesGrob(c(0.5,0.5), c(0.01,0.95), gp=gpar(col='brown', lwd=10)))
for (i in 1:11){
  j=i/100
  grid.draw(linesGrob(c(0.49,0.51), c(j,j), gp=gpar(col='brown', lwd=20)))
}
#grid.draw(linesGrob(c(0.5,0.5), c(0.95,0.99), gp=gpar(col='yellow', lwd=15)))
#grid.draw(linesGrob(c(0.49,0.51), c(0.97,0.97), gp=gpar(col='yellow', lwd=15)))
grid.draw(polygonGrob(c(0.48,0.53,0.48,0.52), c(0.98,0.96,0.94,0.93), gp=gpar(col='yellow', fill='yellow', lwd=10)))
grid.draw(textGrob('Merry Christmas!', x=0.23, y=0.08, gp=gpar(col='black', fontsize=20)))
#grid.draw(pointsGrob(c(0.5), c(0.01), gp=gpar(col='red', lwd=10)))
