library(ggplot2)
library(sf) # do losowania punktow wewnatrz choinki
library(png)
library(patchwork)

logo_mini <- readPNG("C:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/pd_choinka/logo.png", native = TRUE)

punkty_choinki <- data.frame(
  x = c(5,1,4,1,3,0,-3,-1,-4,-1,-5,5),
  y = c(2,5,5,8,8,11,8,8,5,5,2,2)
)

punkty_nogi <- data.frame(
  x = c(1,1,-1,-1),
  y = c(1,2,2,1)
)

polygon <- st_polygon(list(cbind(punkty_choinki$x, punkty_choinki$y)))
polygon <- st_sfc(polygon)
n_points <- 200
bombki <- st_sample(polygon, n_points) # losuje punkty, tak aby byly wewnatrz choinki
bombki <- data.frame(
  x = st_coordinates(bombki)[, 1],
  y = st_coordinates(bombki)[, 2],
  color = sample(colors(), n_points, replace = TRUE),
  size = runif(n_points, 2, 6)
)

lampki <- st_sample(polygon, 50)
lampki <- data.frame(
  x = st_coordinates(lampki)[, 1],
  y = st_coordinates(lampki)[, 2])


choinka <- ggplot() +
  geom_polygon(data = punkty_choinki, aes(x, y), fill = "#1c9111") + # choinka
  geom_polygon(data = punkty_nogi, aes(x,y), fill = '#612400') +     # noga
  geom_polygon(data = punkty_nogi, aes(x-2.5,y-0.5), fill = 'red') + # prezent lewy
  annotate("text", x=-2.5, y=1, label= "ECTS") +
  geom_polygon(data = punkty_nogi, aes(x+2.5,y-0.5), fill = 'red') + # prezent prawy
  annotate("text", x=2.5, y=1, label= "ECTS") +
  geom_point(data = bombki, aes(x, y, color = color, size = size), show.legend = FALSE) +
  geom_point(data = lampki, aes(x,y), color = "white", size = 5, alpha = 0.3) +
  geom_point(data = lampki, aes(x,y), color = "#ffe045", size = 3) +
  geom_point(data = lampki, aes(x,y), color = "white", size = 1) +
  scale_color_identity() +
  theme_void() + 
  coord_fixed()+   # aby to ladnie wygladalo
  inset_element(p = logo_mini,                # logo mini
                left = 0.45,
                bottom = 0.9,
                right = 0.55,
                top = 1)

ggsave("C:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/pd_choinka/choinka.png", choinka, height = 5, width = 7)

