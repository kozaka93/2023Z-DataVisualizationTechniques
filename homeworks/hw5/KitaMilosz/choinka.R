library(ggplot2)

igly <- data.frame(x = c(28, 0, 24, 4, 20, 8, 16, 12),
                   y = seq(0, 10.5, by = 1.5))

bombki <- data.frame(x = c(26, 1, 22, 6, 18, 10, 16),
                   y = seq(0, 9, by = 1.5))

bombki2 <- data.frame(x = c(20, 14, 12,14,14),
                   y = c(0.5,2.3,4,5.5,6.8))

gwiazda <- data.frame(x = c(12), y = c(10.5))

prezenty <- data.frame(x = c(12,17), y = c(0,0))
l1 <- data.frame(x = c(12,12), y = c(-1,1))
l2 <- data.frame(x = c(17,17), y = c(-1,1))
l3 <- data.frame(x = c(10,14), y = c(0.2,0.2))
l4 <- data.frame(x = c(15,19), y = c(0.2,0.2))

sniezynki <- data.frame(x = c(1,2,2,8,7,21,24,22,18,27), y = c(3,7.5,10,9,6,11,8,6,4.5,2))

ggplot() + geom_path(data = igly, aes(x, y), size = 9, color = '#00CC66') +
geom_path(data = igly, aes(x, y), size = 1.5, color = '#FF0033') +
geom_point(data = bombki, aes(x,y), size = 6, color = '#3399FF') +
geom_point(data = bombki2, aes(x,y), size = 6, color = '#871F78') +
geom_point(data = gwiazda, aes(x,y), shape = 'square',size = 16, color = '#FFFF00') +
geom_point(data = gwiazda, aes(x,y), shape = 'diamond' ,size = 24, color = '#FFFF00') +
geom_point(data = sniezynki, aes(x,y), shape = 'plus', size = 12, color = '#FFFFFF') +
geom_point(data = sniezynki, aes(x,y), shape = 'cross' ,size = 12, color = '#FFFFFF') +
geom_point(data = prezenty, aes(x,y), shape = 'square',size = 30, color = '#FF2400') +
geom_path(data = l1, aes(x, y), size = 1.5, color = '#FFFF00') +
geom_path(data = l2, aes(x, y), size = 1.5, color = '#FFFF00') + 
geom_path(data = l3, aes(x, y), size = 1.5, color = '#FFFF00') +
geom_path(data = l4, aes(x, y), size = 1.5, color = '#FFFF00') + 
theme(panel.background = element_rect(fill="black"),
      plot.background = element_rect(fill="black"),
    axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.text = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank()
     )