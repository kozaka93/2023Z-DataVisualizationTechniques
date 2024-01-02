#biblioteki
library(ggplot2)
library(dplyr)
library(gganimate)
library(ggstar)

##tworzenie ramki danych do rysowania choinki

#punkty graniczne
A <- c(0.3, 0.2)
B <- c(0.7, 0.2)
C <- c(0.5, 0.9)

#ograniczenia dla konkretnych poziomów
levels <- seq(0.2, 0.9, length.out = 15)
indent_left <- seq(0.3, 0.5, length.out = 15)
indent_right <- seq(0.7, 0.5, length.out = 15)

#funkcja potrzebna do generowania punktów, sprawdza wygenerowane współrzędne należą do dobrego obszaru
is_inside_region <- function(point, level_down, level_up, left, right) {
  return (point[2] >= level_down && point[2] <= level_up && point[1] >= left && point[1] <= right)
}

#funkcja generująca losowo punkty, które utworzą choinkę
generate_random_points <- function(n, level_down, level_up, left, right) {
  points = numeric(0)
  count = 1
  while (count <= n) {
    point = c(runif(1, left, right), runif(1, level_down, level_up))
    if (is_inside_region(point, level_down, level_up, left, right)) {
      points = c(points, point)
      count = count + 1
    }
  }
  return(points)
}

#dla każdego poziomu generujemy 50 punktów
n = 50
points = numeric(0)

for (i in 1:(length(levels) - 1)) {
  points_i = generate_random_points(n, levels[i], levels[i + 1], indent_left[i], indent_right[i])
  points = c(points, points_i)
}

#przekształcamy w macierz i dodajemy punkty graniczne
points = matrix(points, ncol = 2, byrow = TRUE)
points = rbind(matrix(c(A, B), ncol = 2, byrow = TRUE), points)
points = rbind(points, matrix(C, ncol = 2, byrow = TRUE))

#tworzymy ramkę danych z punktami i dodajemy do niej kolumnę frame, po której później będziemy animować
df <- data.frame(X = points[, 1], Y = points[, 2])
df <- df %>%
  mutate(frame = row_number()) %>% 
  select(3,1,2)
df <- rbind(df, data.frame(frame = max(df$frame) + 1, X = NA, Y = NA))

##tworzymy ramkę danych dla gwiazdy
star <- data.frame(
  frame = unique(df$frame),  
  X = rep(NA, nrow(df)),
  Y = rep(NA, nrow(df)))
star <- rbind(star, data.frame(frame = max(df$frame) + 1, X = 0.5, Y = 0.91))

##rysujemy wykres choinka
choinka <- ggplot() +
  geom_path(df, mapping = aes(x = X, y = Y),size = 2.5, color = "springgreen4")+
  geom_point(df, mapping = aes(x = X, y = Y),size = 3, color = "darkgreen") +
  xlim(0, 1) + ylim(0, 1) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black")) +
  labs(caption = "Wesołych świąt!") +
  theme(plot.caption = element_text(color = "white", size = 25, hjust = 0.5, vjust = 5))+
  geom_star(data = star, aes(x = X, y = Y), size = 15, colour = "gold2", fill = "gold")

#dodajemy animacje do wykresu i zapisujemy
anim <- choinka + transition_reveal(frame)  +
  enter_fade() +
 exit_shrink()
animation <- animate(anim, nframes = 300, fps = 20, end_pause = 30)
anim_save("choinka.gif", animation, width = 500, height = 500, units = "px", res = 300)
