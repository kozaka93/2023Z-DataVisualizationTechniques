library(ggplot2)

f <- function(x) {
  exp(-x / 10) * sin(x)
}

x <- seq(from = -8*pi, to = 0, by = pi/12)

y1 <- f(x)
y2 <- -y1

choinka <- data.frame(x, y1, y2)

pien <- data.frame(x = c(-9*pi, -8*pi), y = c(0, 0))

rysunek <- ggplot(choinka) +
  geom_line(data = pien, mapping = aes(x, y), color = "brown", size = 10) +
  geom_line(aes(x, y1), color = "darkgreen", size = 5) +
  geom_line(aes(x, y2), color = "darkgreen", size = 5) +
  geom_point(aes(0, 0), shape = 8, color = "yellow", size = 15, stroke = 5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "navy")) +
  coord_flip()

# ggsave("choinka.png", rysunek, width = 10, height = 15)
