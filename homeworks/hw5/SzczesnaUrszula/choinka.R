library(gganimate)
library(ggplot2)


green <- data.frame(x = c(25, 34, 29, 36, 31, 38, 33, 42, 8, 17, 12,
                          19, 14, 21, 16, 25),
                    y = c(50, 40, 40, 30, 30, 20, 20, 7, 7, 20, 20, 
                          30, 30, 40, 40, 50))
brown <- data.frame(x = c(30, 30, 20, 20),
                    y = c(7, 0, 0, 7))
ornaments <- data.frame(
  x = c(15, 25, 35, 20, 30, 18, 25, 32, 21, 29, 25, 23, 27),
  y = c(13, 13, 13, 20, 20, 25, 25, 25, 30, 30, 35, 42, 42)
)

star <- data.frame(
  x = 25,
  y = 50
)
background <- data.frame(
  x = c(0, 50, 50, 0),
  y = c(-5, -5, 55, 55)
)


set.seed(123)  
num_snowflakes <- 500
snowflakes <- data.frame(
  x = runif(num_snowflakes, min = 0, max = 50),
  y = runif(num_snowflakes, min = -5, max = 55),
  frame = rep(1:num_snowflakes, each = 10)
)

p <- ggplot(data = background, aes(x, y)) +
  geom_polygon(fill = "navy") +
  geom_polygon(data = green, aes(x, y), fill = "darkgreen") +
  geom_polygon(data = brown, aes(x, y), fill = "#964B00") +
  geom_point(data = ornaments, aes(x, y), shape = 16, color = "darkred", size = 4) +
  geom_point(data = star, aes(x, y), shape = 8, color = "gold", size = 8) +
  theme_void()
  

animated_plot <- p +
  geom_point(data = snowflakes, aes(x, y), shape = 8, color = "white", size = 5) +
  transition_states(states = frame, transition_length = 5, state_length = 2) +
  enter_fade() +
  exit_fade()


animated_plot
anim_save("snowflake_animation.gif", animate(animated_plot))
str(ind)
