library(ggplot2)
library(dplyr)

tree <- function() {
  df <-
    data.frame(
      x = vector(),
      y = vector(),
      color = vector(),
      size = vector()
    )
  
  y <-  seq(0, 0.1, length.out = 100)
  x <- seq(-0.05, 0.05, length.out = 100)
  
  for (i in 1:length(y)) {
    df <-
      rbind(df, data.frame(
        x = x,
        y = rep(y[i], length(x)),
        color = rep("drzewo", length(x)),
        size = rep(1, length(x))
      ))
  }
  
  
  y <-  seq(0.1, 1.5, length.out = 200)
  
  for (i in 1:length(y)) {
    m1 <- rnorm(1, (y[i] - 1.5) / 4, 0.02)
    m2 <- rnorm(1, (y[i] - 1.5) / -4, 0.02)
    
    x <- seq(m1 , m2 , length.out = ceiling(abs(m1 - m2) * 100))
    df <-
      rbind(df, data.frame(
        x = x,
        y = rep(y[i], length(x)),
        color = rep("igly", length(x)),
        size = rep(1, length(x))
      ))
    
  }
  
  
  
  tmp <- df[sample(nrow(df), 10, replace = FALSE), ] %>%
    filter(y > 0.1) %>%
    mutate(color = "bombki1", size = 2)
  df <- rbind(df, tmp)
  tmp <- df[sample(nrow(df), 10, replace = FALSE), ] %>%
    filter(y > 0.1) %>%
    mutate(color = "bombki2", size = 2)
  df <- rbind(df, tmp)
  tmp <- df[sample(nrow(df), 10, replace = FALSE), ] %>%
    filter(y > 0.1) %>%
    mutate(color = "bombki3", size = 2)
  df <- rbind(df, tmp)
  return(df)
  
}
#df <- tree()
#write.csv(df,file="drzewko.csv")

df <- read.csv("./drzewko.csv")

df %>%
  ggplot(aes(
    x = x,
    y = y,
    color = color,
    size = size
  )) + geom_point()  +
  scale_color_manual(values = c("drzewo" = "chocolate","bombki3" = "pink","bombki2" = "blue", "bombki1" = "red", "igly" = "darkgreen")) +
  xlim(-1, 1)+
  geom_point(x = 0,y=1.46,size = 15,shape = 24,fill = "yellow")+
  geom_point(x = 0,y=1.46,size = 15,shape = 25,fill = "yellow")+
  theme_void()+
  theme(panel.background = element_rect(fill = "lightblue"))+
  guides(color="none",size="none")



