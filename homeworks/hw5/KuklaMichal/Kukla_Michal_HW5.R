library(ggplot2)
library(gganimate)
library(gifski)


punkty_choinki <- data.frame(
  x = c(-4,-3,-3,-3,-2,-2,-2,-2,-2,-1,-1,-1,-1,-1,
        0,0,1,1,1,1,1,2,2,2,2,2,3,3,3,4,
        -3.5,-3.5,-2.5,-2.5,-2.5,-2.5,-1.5,-1.5,-1.5,-1.5,-1.5,-1.5,-0.5,-0.5,-0.5,
        0.5,0.5,0.5,1.5,1.5,1.5,1.5,1.5,1.5,2.5,2.5,2.5,2.5,3.5,3.5),
  
  y = c(2,5,3,2,8,6,5,4,2,9,8,7,5,2,10,2,9,8,7,5,2,8,6,5,4,2,5,3,2,2,
        2.5,2,5.5,5,3.5,2,8.5,8,6.5,5,4.5,2,9.5,8,7.5,9.5,8,7.5,8.5,8,6.5,5,4.5,2,
        5.5,5,3.5,2,2.5,2),
  
  z = 1:60
)

pien <- data.frame(
  x = c(-0.5,-0.5,-0.5,-0.5,-0.5,0,0.5,0.5,0.5,0.5,0.5),
  y = c(2,1.5,1,0.5,0,0,2,1.5,1,0.5,0),
  z = 1:11
)

bombki <- data.frame(
  x = c(0.5,-1,0.5,0,-2,1.5),
  y = c(8.5,6,5.5,4,3,2.5),
  z = 1:6,
  col_b = c("red", "blue", "orange", "pink", "purple", "navy")
)


# Funkcja generująca punkty dla gwiazdy o zadanym promieniu, ilości ramion i środku
generuj_gwiazde <- function(promien, ilosc_ramion, srodek) {
  kat_miedzy_ramionami <- 2 * pi / ilosc_ramion
  kat_startowy <- pi / 2  # zaczynam od górnego ramienia
  x <- numeric(0)
  y <- numeric(0)
  for (i in 1:ilosc_ramion) {
    kat <- kat_startowy + (i - 1) * kat_miedzy_ramionami
    x <- c(x, srodek[1] + promien * cos(kat), srodek[1] + 0.5 * promien * cos(kat + 0.5 * kat_miedzy_ramionami))
    y <- c(y, srodek[2] + promien * sin(kat), srodek[2] + 0.5 * promien * sin(kat + 0.5 * kat_miedzy_ramionami))
  }
  return(data.frame(x = x, y = y))
}


punkty_gwiazdy <- generuj_gwiazde(1, 5, c(0,10))
punkty_gwiazdy$z <- 1:10

# Narysowanie choinki
anim <- ggplot() +
  geom_point(data = punkty_choinki, aes(x, y), color = "darkgreen", size = 6) +
  geom_point(data = pien, aes(x, y), color = "brown", size = 6) +
  geom_point(data = punkty_gwiazdy, aes(x,y), color = "#ebb728", size = 8) +
  geom_point(data = bombki, aes(x,y, colour = col_b), size = 18) +
  coord_fixed(ratio = 1) +
  theme(panel.background = element_rect(fill = "white"), legend.position = "none") +
  transition_states(z) +
  shadow_mark()

# Animacja
animate(anim, duration = 10, fps = 1000/80, renderer = gifski_renderer())
       
# Zapis
anim_save("choinka.gif", animate(anim), renderer = gifski_renderer())
