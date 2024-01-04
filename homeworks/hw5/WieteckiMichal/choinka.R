
library("ggplot2")

x <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,
       2,3,4,5,6,7,8,9,10,11,12,13,
       3,4,5,6,7,8,9,10,11,12,
       4,5,6,7,8,9,10,11,
       5,6,7,8,9,10,
       6,7,8,9,
       7,8,
       7.5) 
y <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,
       2,2,2,2,2,2,2,2,2,2,2,2,
       3,3,3,3,3,3,3,3,3,3,
       4,4,4,4,4,4,4,4,
       5,5,5,5,5,5,
       6,6,6,6,
       7,7,
       6.5)

pieniek <- data.frame(x = 7.5, y = 0)
bombki_cz <- data.frame(y = c(2.4, 5.4), x = c(8, 6))
bombki_nieb <- data.frame(y = c(1.5, 3.7, 5), x = c(12, 5, 9))
bombki_pom <- data.frame(y = c(1.8, 3.2), x = c(4, 10))




length(x)
length(y)



choinka <- data.frame(x = x, y = y)


ggplot(pieniek, aes(x = x, y = y)) +
  geom_point(size = 10,
             shape = 15, color = "#873e23", fill = "#873e23") +
  geom_point(data = choinka, aes(x =x, y =y), color = "darkgreen", shape = 4, size = 10, stroke = 3) +
  xlim(-10, 24) + ylim(0, 10) +
  geom_point(data = bombki_cz, aes(x =x , y =y), size = 6, shape= 19, color = "red") +
  geom_point(data = bombki_nieb, aes(x =x , y =y), size = 6, shape= 19, color = "lightblue")+
  geom_point(data = bombki_pom, aes(x =x , y =y), size = 6, shape= 19, color = "orange") + 
  geom_point(data = data.frame(x = 7.5, y = 7.5), aes(x = x, y = y), size = 15,
             shape = 8, color = "gold", fill = "gold", stroke = 3) +
  theme(panel.background = element_rect(fill = "#003366"),
        plot.background = element_rect(fill = "#003366"),
        panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank())
  
  
    

