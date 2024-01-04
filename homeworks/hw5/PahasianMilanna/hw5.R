library(ggplot2)
library(dplyr)
library(gganimate)

b <- seq(0,1,length.out=500)
a <- runif(500,0,1-b)
tree <- rbind(data.frame(a = c(a, -a), b = rep(b, 2)))

b1<- seq(0,1,length.out=100)
a1 <- runif(100,0,1-b1)
bomb <- rbind(data.frame(a = c(a1, -a1), b = rep(b1, 2)))

b2 <- runif(500, min = 0, max = 1)
a2 <- runif(500, min = -1, max = 1)
snow <- data.frame(a=a2,b=b2,state=rep(1:50, each=5))

p <- ggplot(snow)+
  geom_point(aes(b,a), color = "white")+
  geom_line(data = tree,aes(b,a,color = abs(a)),size =4, show.legend = FALSE)+
  theme_void()+
  scale_color_gradient(low="#083027", high="#156654")+
  geom_jitter(data =bomb,aes(b,a, group =seq_along(b)),
              size=2, 
              shape =21,
              show.legend = FALSE,
              fill ='#f5ed9a', 
              color ='#f5ed9a')+
  geom_point(data = data.frame(a=0,b=1), aes(b, a), shape = 8, color = "#ffef47", size = 10) +
  coord_flip()+
  theme(panel.background = element_rect(fill = "#9eccde"))+
  transition_states(state, transition_length = 1, state_length = 2)+
  enter_fade() +
  exit_fade()

animate(p, duration = 5, fps = 20, renderer = gifski_renderer())
anim_save("output.gif")