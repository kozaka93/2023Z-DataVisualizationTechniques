library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

#choinka z rozkladu wykladniczego

#tak sie przyjemnie sklada ze galąź choinki przypomnia gęstość rozkładu wykładniczego
#przyblizamy gestosc histogramem
#kazde odpalenie programu generuję nową choinkę, a skoro generujemy bardzo duzo liczb
#z rozkladu do ksztalt choinki zawsze bedzie odpowiedni 

data1 = data.frame(value = rexp(10000, 2))
data2 = data.frame(value = rexp(10000, 1.5) + 1)
data3 = data.frame(value = rexp(10000, 1) + 2)
data4 = data.frame(value = rexp(10000, 0.8) + 3.5)
data5 = data.frame(value = rexp(6000, 0.5) + 5)

data_combined = rbind(data1, data2, data3, data4, data5)

#funkcja do generowania wspolrzednych bombek, tak zeby lezaly wewnatrz histogramu
#tak na oko ograniczam rozkladem
func <- function(y) {
  if (0 <= y & y < 1) {
    x = runif(1, 10, 600*exp(-2*y))
  }
  else if (1 <= y & y < 2) {
    x = runif(1, 10, 500*exp(-1.5*(y-1)) + 600*exp(-2*y))
  }
  else if (2 <= y & y < 3.5) {
    x = runif(1, 10, 300*exp(-(y-2)) + 500*exp(-1.5*(y-1)) + 600*exp(-2*y))
  }
  else if (3.5 <= y & y < 5) {
    x = runif(1, 10, 250*exp(-0.8*(y-3.5)) + 300*exp(-(y-2)) + 500*exp(-1.5*(y-1)) + 600*exp(-2*y))
  }
  else {
    x = runif(1, 5, 100*exp(-0.5*(y-5)) + 250*exp(-0.8*(y-3.5)) + 300*exp(-(y-2)) + 500*exp(-1.5*(y-1)) + 600*exp(-2*y))
  }
}

#bombki wieszamy losowo, coz czasami bedzie brzydko, ale to cena ktora jestesmy gotowi zaplacic za
#nieskonczenie wiele choinek... przynajmniej ja jestem

y = runif(12, 0, 8)
x = unlist(lapply(y, func))

bombki <- data.frame(x, y)

treeL <- ggplot() +
  geom_histogram(data = data_combined, aes(y=value), binwidth = 0.04, color = "darkgreen", fill = "darkgreen") +
  ylim(0, 8.5) +
  theme_void() +
  theme(plot.margin = margin(l=0)) +
  labs(x = NULL, y = NULL) +
  geom_point(data = bombki,
             aes(x = x, y = y),
             color = sample(c("#3EB489",
                              "red",
                              "gold",
                              "orange",
                              "pink"),
                            size = 12,
                            replace = TRUE),
             size = 5) +
  theme(plot.background=element_rect(fill="#adbce6"))


treeR <- treeL + scale_x_reverse() 

grid.arrange(treeR, treeL, ncol = 2)
grid.draw(linesGrob(x = c(0.5,0.5), c(0,0.95), gp=gpar(col='brown', lwd=19)))

