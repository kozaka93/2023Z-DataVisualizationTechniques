library(png)
library(tidyverse)
library(reshape2)
library(gifski)
library(gganimate)


p <- readPNG("trujkont.png")
mred <- p[,,1]
red <- melt(mred) %>% pull(value)
mgreen <- p[,,2]
green <- melt(mgreen) %>% pull(value)
mblue <- p[,,3]
blue <- melt(mblue) %>% pull(value)


cords <- melt(mred) %>% select(Var1,Var2)
cords <- cords %>% rename(y = Var1, x = Var2)

kolory <- paste(red,green,blue)
kolory <- sapply(strsplit(kolory, " "), function(x)
  rgb(x[1], x[2], x[3]))

triangle <- cbind(cords,kolory)

triangle <- triangle %>% filter(kolory != "#FFFFFF")
triangle$x = triangle$x - 13
triangle$y = triangle$y - 13
triangle$x <- triangle$x * cos(pi) - triangle$y * sin(pi)
triangle$y <- triangle$x * sin(pi) + triangle$y * cos(pi)





random_rotate <- function(x,y){
  kat = runif(1,-pi/6,pi/6)
  new_x <- x * cos(kat) - y * sin(kat)
  new_y <- x * sin(kat) + y * cos(kat)
  df <- as.data.frame(cbind(new_x,new_y)) %>% rename (x = new_x, y = new_y)
  df
}

m <- t(rbind(1*c(seq(0,15,length.out=9),seq(2,13,length.out=8),seq(3,11,length.out=6),seq(4.5,9.5,length.out=4),5.5,7.5,6.5),
      2.6*c(0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,4,4,5))) * 10
s <- sample(1:length(m[,1]))
m <- m[s,]

t_list <- lapply(1:length(m[,1]) , function(i){
  x <- triangle$x 
  y <- triangle$y 
  df <- random_rotate(x,y)
  x <- df$x + m[i,1]
  y <- df$y + m[i,2]
  df <- as.data.frame(cbind(x,y))
  df$kolor = rgb(runif(1,0,0.7) , runif(1,0.8,0.9) , runif(1,0,0.7))
  df
  
})


cum_t_list <- lapply(1 : length(t_list), function(i) {
  df <- do.call(rbind, t_list[1:i])
  df$frame = i
  df
})

df <- do.call(rbind, cum_t_list)


g <- readPNG("gwiazda.png")
mred <- g[,,1]
red <- melt(mred) %>% pull(value)
mgreen <- g[,,2]
green <- melt(mgreen) %>% pull(value)
mblue <- g[,,3]
blue <- melt(mblue) %>% pull(value)
cords <- melt(mred) %>% select(Var2,Var1)
cords <- cords %>% rename(x = Var2, y = Var1)

kolory <- paste(red,green,blue)
kolor <- sapply(strsplit(kolory, " "), function(x)
  rgb(x[1], x[2], x[3]))

gwiazda <- cbind(cords,kolor)

gwiazda <- gwiazda %>% filter(kolor != "#FFFFFF")
gwiazda$kolor <- "gold"
gwiazda$x = gwiazda$x - 13
gwiazda$y = gwiazda$y - 13
gwiazda$x = gwiazda$x + 64
gwiazda$y = gwiazda$y + 150



df1 <- cum_t_list[[30]]
z = sample(1:length(df1$x),length(df1$x)/60)
df1$kolor[z] = "red"
z = sample(1:length(df1$x),length(df1$x)/60)
df1$kolor[z] = "blue"
z = sample(1:length(df1$x),length(df1$x)/60)
df1$kolor[z] = "yellow"
df1$frame = 31
gwiazda$frame = 31
df1 <- rbind(df1 , gwiazda)




df2 <- df1 %>% mutate(kolor = case_when(kolor == "yellow" ~ "red",
                                         kolor == "red" ~ "blue",
                                         kolor == "blue" ~ "yellow",
                                         T ~ kolor))
df2$frame = 32
gwiazda$frame = 32
df2 <- rbind(df2 , gwiazda)



df3 <- df1 %>% mutate(kolor = case_when(kolor == "yellow" ~ "red",
                                        kolor == "red" ~ "blue",
                                        kolor == "blue" ~ "yellow",
                                        T ~ kolor))
df3$frame = 33
gwiazda$frame = 33
df3 <- rbind(df3 , gwiazda)



df4 <- df1
df4$frame = 34
gwiazda$frame = 34
df4 <- rbind(df4 , gwiazda)

df5 <- df2
df5$frame = 35
gwiazda$frame = 35
df5 <- rbind(df5 , gwiazda)

df6 <- df3
df6$frame = 36
gwiazda$frame = 36
df6 <- rbind(df6 , gwiazda)

df7 <- df1
df7$frame = 37
gwiazda$frame = 37
df7 <- rbind(df7 , gwiazda)

df8 <- df2
df8$frame = 38
gwiazda$frame = 38
df8 <- rbind(df8 , gwiazda)

df9 <- df3
df9$frame = 39
gwiazda$frame = 39
df9 <- rbind(df9 , gwiazda)



df <- rbind(df,df1,df2,df3,df4,df5,df6,df7,df8,df9)



plot <- ggplot(df,aes(x=x,y=y,color=kolor)) + 
  transition_states(frame, transition_length = 2, state_length = 1) +
  geom_point(size=3) + 
  scale_color_identity()+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()  
  )

plot_anim <- animate(plot, duration = 13,
                                     fps = 3, width = 600, 
                                     height = 600, renderer = gifski_renderer())


anim_save("choinka.gif", plot_anim)






