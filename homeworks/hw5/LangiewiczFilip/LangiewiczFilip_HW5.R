library(ggplot2)
library(ggstar)

start <- 10

x = seq(-start, start, by = 0.1)

pien <- seq(-2, 38, by = 0.1)

galezie <- lapply(0:20, function(i) seq(-start + 0.5 * i, start - 0.5 * i, by = 0.1))

gwiazdy <- data.frame(x = sample(-10:10,
                                 size = 100,
                                 replace = TRUE),
                      y = sample(0:38,
                                 size = 100,
                                 replace = TRUE))

# na poczatku wyrasta pien
p <- ggplot() +
  geom_line(data = data.frame(x = 0, pien = pien),
            aes(x = x, y = pien),
            color = "brown",
            linewidth = 3)


# potem galezie

for (i in 0:20) {
  p <- p + geom_line(data = data.frame(x = galezie[[i + 1]], 
                                       y = abs(0.5 * galezie[[i + 1]]) + 2 * i),
                     aes(x = x, 
                         y = y),
                     color = "darkgreen",
                     linewidth = 4)
  
}



# potem losowo wieszamy roznokolorowe bombki (losujemy ile bo moge sie potluc)
for (i in 0:17) {
  ile_bombek <- sample(0:(0.5 * length(unique(round(galezie[[i + 1]])))), 
                       size = 1)
  bombki <- sample(unique(round(galezie[[i + 1]])),
                   size = ile_bombek)
  p <- p + geom_point(data = data.frame(x = bombki,
                                       y = abs(0.5 * bombki) + 2 * i - 0.5),
                     aes(x = x, 
                         y = y),
                     color = sample(c("yellow",
                                      "red",
                                      "blue",
                                      "orange",
                                      "pink"),
                                    size = ile_bombek,
                                    replace = TRUE),
                     size = 5)
}


# i na koncu oczywiscie zawieszamy gwiazde

p <- p + geom_star(data = data.frame(x = 0,
                                 y = 38),
              aes(x = x, 
                   y = y),
              starshape = 1,
              size = 10,
              fill = "gold")


# dodajemy zyczenia i ustawiamy choinke w dobrym miejscu w domu i gotowe
p <- p + ggtitle("Wesołych Świąt!") +
  geom_point(data = gwiazdy,
             aes(x = x,
                 y = y),
             color = "white",
             shape = 8) +
  coord_fixed(ratio = 1) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#330066",
                                       color = "black"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold.italic",
                             margin = margin(t = 20),
                             size = 25,
                             hjust = 0.5,
                             color = "red"))

p 

