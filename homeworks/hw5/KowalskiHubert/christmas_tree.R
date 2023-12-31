library(grid)
library(ggstar)
library(gganimate)
# Macierze transformacji
A=vector('list',4)
A[[1]]=matrix(c(0.008,0,0,0.16),nrow=2)
A[[2]]=matrix(c(0.85,-0.005,0.005,0.85),nrow=2)
A[[3]]=matrix(c(0.2,0.23,-0.26,0.22),nrow=2) 
A[[4]]=matrix(c(-0.15,0.26,0.28,0.24),nrow=2)
# Wektory przesunięć
b=vector('list',4)
b[[1]]=matrix(c(0,0))
b[[2]]=matrix(c(0,1.5))
b[[3]]=matrix(c(0,1.0))
b[[4]]=matrix(c(0,0.44))
# Modyfikacja dokładności fraktala
# n = 50_000 powoduje uzyskanie fraktala o przeciętnej dokładności, a animacja wykonuje się w czasie rzędu 2min
# n = 700_000 powoduje uzyskanie fraktala o dobrej dokładności, a animacja wykonuje się w czasie rzędu 16min
n <- 700000
x <- numeric(n)
y <- numeric(n)
x[1] <- y[1] <- 0
# Współrzędne punktów
for (i in 1:(n-1)) {
  trans <- sample(1:4, prob=c(.02, .9, .09, .08), size=1)
  xy <- A[[trans]]%*%c(x[i],y[i]) + b[[trans]]
  x[i+1] <- xy[1]
  y[i+1] <- xy[2]
}

ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) +
  geom_point(colour="green4",size=0.1) +
  theme_bw() -> tree

# Pozycje bombek
xb <- c(-1.5, 2, -0.5, 1, -0.65, 0.4, 0.7, 0.3, -0.4, -0.4, 1, 0, -1.3, 1.3, 0.4, 0.2, -0.2, -0.8, 1.2, -1.5, -0.2, 0.5, 0.3, -0.6)
yb <- c(2.3, 1.9, 3.8, 2.8, 6, 7.9, 5.5, 6.5, 2.6, 7.5, 6.6, 8.6, 4.65, 4.45, 3.6, 1, 5, 2, 1.8, 3.7, 6.6, 4.6, 9.1, 5.2)

snieg <- data.frame(x = runif(1000, -2.8, 3), y = runif(1000, 0, 10), state = sample(1:5, 1000, replace = T)) %>%
  filter(y > 3.1 * x + 10.5 | y > -3.2 * x + 11.5)


tree + 
  geom_star(data = data.frame(x = 0.34, y = 10.1), color = "gold", size = 11, fill = "gold", angle = 7, starshape = 9) + 
  geom_star(data = data.frame(x = xb, y = yb), fill = "red3", color = "red3",  starshape = 14, size = 11) + 
  geom_star(data = data.frame(x = xb, y = yb), fill = "white", color = "gold",  starshape = 9, size = 6, starstroke = 1.8) +
  theme(
    panel.background = element_rect(fill = "grey1"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  ) + 
  geom_star(data = snieg, aes(group = seq_along(state)), color = "white", fill = "white", size = 2, starshape = 24) -> full_tree

# full_tree

full_tree + 
  transition_states(state, transition_length = 1, state_length = 0.1) +
  enter_fade() +
  exit_fade() -> animated_tree

animate(animated_tree, nframes = 200, detail = 2, fps = 20, width = 1120, height = 1400, renderer = gifski_renderer("christmas_tree.gif"))

