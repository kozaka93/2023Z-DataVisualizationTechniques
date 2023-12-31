library(ggplot2)
library(dplyr)
library(tidyr)
library(tigers)
library(ggeasy)

zielony = c(0, 0)
bombki1 = c(0, 1)
bombki2 = c(0, 1)
  
# Definiowanie obszaru, na którym mogą się tworzyć igły i ozdoby
df1 = data.frame(  
  "a" = c(-0.9, 0, 0.9, -0.7, 0, 0.7, -0.4, 0, 0.4),  
  "b" = c(0, 1, 0, 0.9, 1.8, 0.9, 1.7, 2.5, 1.7)
)  

for (i in 1:3) {
P <- data.matrix(df1[((i*3)-2):(i*3),]) 

# Losowanie punktów
zielony <- rbind(zielony, rpit(2400/i, P))
bombki1 <- rbind(bombki1, rpit(30/i, P))
bombki2 <- rbind(bombki2, rpit(30/i, P))
}

colnames(zielony) <- c('a', 'b')
colnames(bombki1) <- c('a', 'b')
colnames(bombki2) <- c('a', 'b')

df2 = data.frame(  
  "a" = c(-0.15, 0.15, 0.15, -0.15, 0.15, -0.15),  
  "b" = c(0, 0, -0.3, -0.3, -0.3, 0)
)  

pien <- c(0, -0.1)

for (i in 1:2) {
  P <- data.matrix(df2[((i*3)-2):(i*3),]) 
  #print(P)
  pien <- rbind(pien, rpit(300, P))
}

colnames(pien) <- c('a', 'b')

lancuchy = data.frame(  
  "a" = c(-0.4, 0.2),  
  "b" = c(-0.04, 2.14)
) 

gwiazda <- data.frame(
  a = 0,
  b = 2.4,
  name = "*",
  stringsAsFactors = FALSE)

# Tworzenie "wykresu"
ggplot() +
  geom_point(data = data.frame(pien), 
             aes(x = a, y = b), 
             color = 'brown', 
             shape = 15, size = 3) +
  geom_point(data = data.frame(zielony), 
             aes(x = a, y = b), 
             color = 'green', 
             shape = 2, size = 3) +
  geom_line(data = lancuchy, 
            aes(x = a, y = b), 
            color = 'gray95', 
            linewidth = 5) +
  geom_point(data = data.frame(bombki1), 
             aes(x = a, y = b), 
             color = 'black', fill = 'blue', 
             shape = 21, size = 5) +
  geom_point(data = data.frame(bombki2),
             aes(x = a, y = b), 
             color = 'black', fill = 'red', 
             shape = 23, size = 5) +
  geom_text(data = gwiazda, 
            aes(x = a, y = b, label = name), 
            color = "gold", size = 45) +
  theme_dark() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = 'blue', 
                                  size = 22, 
                                  face = 'bold')) + 
  xlim(-1.2, 1.2) + ylim(-0.2, 3) +
  xlab(' ') + ylab(' ') +
  ggtitle('Wylosowana choinka:') +
  easy_center_title()
