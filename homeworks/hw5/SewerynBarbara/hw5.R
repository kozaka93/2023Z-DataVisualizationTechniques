setwd("C:\\Users\\basiu\\OneDrive\\Pulpit\\sem 3\\techniki wizualizacji danych\\hw5")

df <- readxl::read_xlsx("dane.xlsx")
library(dplyr)
library(ggplot2)

df %>% filter(`mod 2` == 0) ->df0

df %>% filter(`mod 2` == 1) ->df1

bombki <- data.frame(c(2,6,10,14,18,22,29,36,4,3), c(-18,17,-10,10,-7,7,-3,-1,-2,4))
colnames(bombki) <- c('x','y')

snieg <- data.frame(seq(1,40,2), c(-16,17,13,-12,11,-10,-19,19,3,-3,-7,7,12,-9,2,19,-6,-3,-10,16))
colnames(snieg) <- c('x','y')

ggplot(df0, aes(liczba,galaz)) + 
  geom_col(fill = "darkgreen", width = 1.2) +
  geom_col(data = df1, aes(liczba, galaz), fill = "green4", width = 1.2)+ coord_flip() +
  geom_point(data = bombki,aes(x,y), size = 5, color = "darkred")+ 
  theme(plot.background = element_rect(fill = "lightblue3")) +
  theme(panel.background = element_rect(fill = "lightblue3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(x = 32,y = -16, label = "Wesołego", color = "darkred", size = 6) +
  geom_text(x = 29, y = -16, label = "jajka!", color = "darkred", size = 6) +
  geom_point(data = snieg, aes(x,y), shape = 8, color = "white", size = 4) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_point(x = 0,y=0, shape = 15, color = "chocolate4", size = 8) +
  geom_point(x = 40, y = 0, shape = 24, fill = "goldenrod3", size = 8, color = "goldenrod3") +
  geom_point(x = 40, y = 0, shape = 25, fill = "goldenrod3", size = 8, color = "goldenrod3")
                   