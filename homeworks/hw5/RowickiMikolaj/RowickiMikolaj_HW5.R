setwd("C:\\Semestr 3\\TWD\\Prace domowe\\Praca domowa 5\\RowickiMikolaj")

# Wczytanie bibliotek
library(ggplot2)
library(gganimate)
library(dplyr)
library(ggstar)

# Poniższy algorytm jest implementacją tzw. gry w chaos (ang. Chaos game), która
# pozwala, z wykorzystaniem generatora liczb losowych, stworzyć obraz pewnego fraktala,
# w tym przypadku trójkąta Sierpińskiego. Na początku stawia się na płaszczyźnie 
# 3 dowolne punkty (niewspółliniowe, gdyż inaczej fraktal zdegeneruje
# się do odcinka), po czym wybiera się kolejny punkt płaszczyzny, zwany punktem
# gry (game point). Następnie wybiera się dowolny z trzech punktów obranych na 
# samym początku i stawia punkt w połowie odległości między czwartym
# punktem a tym wybranym. Powtarza się ten krok, za każdym razem oznaczając punkt 
# leżący dokładnie w połowie odległości między ostatnio postawionym a jednym z 
# trzech pierwszych. Moja choinka jest złożeniem trzech takich fraktali.

set.seed(1915)


# Generuję wierzchołek
vertex <- data.frame(
  vertex_x = c(0, 1, 0.5),
  vertex_y = c(0, 0, 0.5 * sin(pi/3)),
  random_vertex = c(1, 2, 3)
)

# Funkcja do znajdowania środka odcinka łączego dwa punkty
find_midpoint <- function(point1, point2){
  distance <- abs(point2 - point1)
  min(point1, point2) + distance / 2
}

# Wartość rekomendowana dla grafiki statycznej, w przypadku gifa, warto zmienić
# na mniejszą wartość, np. 4000
n_points <- 10000

# Funkcja do tworzenia punktów zgodnie z zasadami gry w chaos
create_sierpinski_points <- function(n, vertex) {
  sierpinski_table <- data.frame(
    step = 1:n,
    random_vertex = sample(1:3, n, replace = TRUE),
    x = rep(0, n),
    y = rep(0, n),
    triangle = rep(0, n)
  )
  
  for (step in 1:n) {
    sierpinski_table[step, "x"] <-
      sample(
        seq(0, 0.5, by = 0.001),
        size = 1
      )
    
    sierpinski_table[step, "y"] <- sample(
      seq(0, sin(pi/3) * sierpinski_table[step, "x"], by = 0.001),
      size = 1
    )
  }
  
  return(sierpinski_table)
}

# Funkcja do tworzenia ramki dancyh z wygenerowanych punktów.
create_sierpinski_df <- function(points, shift_x = 0,shift_y = 0) {
  df <- merge(points, vertex, by = "random_vertex")
  df <- df[order(df$step), ]
  
  for (step in 2:n_points) {
    df[step, "x"] <- find_midpoint(
      df[step - 1, "x"],
      df[step - 1, "vertex_x"]
    )
    
    df[step, "y"] <- find_midpoint(
      df[step - 1, "y"],
      df[step - 1, "vertex_y"]
    )
  }
  
  # Przesunięcie trójkąta o wartość shift_y
  df$y <- df$y + shift_y
  df$x <- df$x + shift_x
  
  return(df)
}

# Stworzenie trzech zbiorów punktów (dla każdego trójkąta)
triangle1_points <- create_sierpinski_points(n_points, vertex)
triangle2_points <- create_sierpinski_points(n_points, vertex)
triangle3_points <- create_sierpinski_points(n_points, vertex)

# Stworzenie zbiorów danych z uwzględniem koniecznych przesunięć, gdyby ich nie 
# było wszystkie nakładały by się na siebie (wygenerowałyby się w tym samym miejscu)
triangle1_df <- create_sierpinski_df(triangle1_points)
triangle2_df <- create_sierpinski_df(triangle2_points,shift_x = 1/14 * (max(triangle1_df$x) - min(triangle1_df$x)) + 0.001, shift_y = 0.275)
triangle3_df <- create_sierpinski_df(triangle3_points, shift_x = 1/7 * (max(triangle1_df$x) - min(triangle1_df$x)) + 0.04, shift_y = 0.64)


# Dostosywanie rozmiarów trójkątów
triangle1_df$x <- triangle1_df$x 
triangle2_df$x <- triangle2_df$x * 6/7
triangle3_df$x <- triangle3_df$x * 5/7

triangle1_df$y <- triangle1_df$y * 1.9323
triangle2_df$y <- triangle2_df$y * 1.6758
triangle3_df$y <- triangle3_df$y * 1.3851

# Usuwanie zbędnych punktów w miejscach, w których dwa trójkąty nakładają się na
# siebie
triangle2_df <- triangle2_df %>%
  filter(x < 0.27711472 | x > 0.70334506 | y < 0.4608478 | y > 0.64157) %>% 
  filter(x > 0.291322404 | x < 0.234491668 | y > 0.64157335 | y < 0.5963919625)

triangle3_df <- triangle3_df %>%
  filter(x < 0.3087062 | x > 0.663524 | y < 0.8864643 | y > 1.035930225)

# Złączenie wszystkich ramek danych
all_triangles_df <- rbind(triangle1_df, triangle2_df, triangle3_df)

# Wygenerowanie zbioru danych odpowiedzialnych za śnieżynki
bg_stars <- data.frame(x = runif(70, min = min(all_triangles_df$x) - 1, max = max(all_triangles_df$x) + 1),
                       y = runif(70, min = min(all_triangles_df$y) - 0.2, max = max(all_triangles_df$y) + 0.2))

# Poniżej zakomentowany został kod niezbędny do wygenerowania animacji. Przed
# jego odkomentowaniem i uruchomieniem warto zmienić wartość parametru n_points
# na mniejszą wartość, np. 3000 lub 4000.

# sierpinski_triangle <- ggplot(all_triangles_df, aes(x, y)) +
#   geom_polygon(data = vertex, aes(x = vertex_x, y = vertex_y * 1.9323), fill = "#99CC33", alpha = 1) +
#   geom_polygon(data = vertex, aes(x = (vertex_x + 1/14 * (max(all_triangles_df$x) - min(all_triangles_df$x)) + 0.001) * 6/7, y = (vertex_y + 0.275)* 1.6758), fill = "#99CC33", alpha = 1) +
#   geom_polygon(data = vertex, aes(x = (vertex_x + 1/7 * (max(all_triangles_df$x) - min(all_triangles_df$x)) + 0.04) * 5/7,  y = (vertex_y + 0.64)* 1.3851), fill = "#99CC33", alpha = 1) +
#   geom_point(data = all_triangles_df, aes(x, y, group = step), size = 0.1, color = "forestgreen") +
#   geom_rect(aes(xmin = 0.4, xmax = 0.6, ymin =-0.2, ymax = 0), fill = "brown", color = "brown") +  # Prostokąt reprezentujący korzeń
#   geom_point(data = all_triangles_df, aes(x, y, group = step), size = 1, color = "forestgreen") +
#   geom_point(data = bg_stars, aes(x = x, y = y), shape = 8, size = 7, color = "white") +
#   geom_star(data = data.frame(x = 0.485, y = 1.55), aes(x = x, y = y), starshape = 9, size = 20, fill = "gold") +
#   geom_text(aes(x = 0.5, y = -0.5), label = "Wesołych Świąt!", color = "white", size = 18, fontface = "bold") +
#   theme_void() +
#   coord_fixed(ratio = 1) +
#   theme(panel.background = element_rect(fill = "#22049C")) +
#   expand_limits(x = c(min(all_triangles_df$x) - 1, max(all_triangles_df$x) + 1),
#                 y = c(min(all_triangles_df$y) - 0.7, max(all_triangles_df$y) + 0.2))
# 
# 
# sierpinski_animation <- sierpinski_triangle +
#   gganimate::transition_manual(frames = step, cumulative = TRUE)
# 
# anim_save("animation.gif", sierpinski_animation, width = 1393, height = 1112)

# Kod odpowiedzialny za wygenerowanie statycznego wykresu
ggplot(all_triangles_df, aes(x, y)) +
  geom_polygon(data = vertex, aes(x = vertex_x, y = vertex_y * 1.9323), fill = "#99CC33", alpha = 1) +
  geom_polygon(data = vertex, aes(x = (vertex_x + 1/14 * (max(all_triangles_df$x) - min(all_triangles_df$x)) + 0.001) * 6/7, y = (vertex_y + 0.275)* 1.6758), fill = "#99CC33", alpha = 1) +
  geom_polygon(data = vertex, aes(x = (vertex_x + 1/7 * (max(all_triangles_df$x) - min(all_triangles_df$x)) + 0.04) * 5/7,  y = (vertex_y + 0.64)* 1.3851), fill = "#99CC33", alpha = 1) +
  geom_point(data = all_triangles_df, aes(x, y, group = step), size = 0.1, color = "forestgreen") +
  geom_rect(aes(xmin = 0.4, xmax = 0.6, ymin =-0.2, ymax = 0), fill = "brown", color = "brown") +  # Prostokąt reprezentujący korzeń
  geom_point(data = all_triangles_df, aes(x, y, group = step), size = 0.0001, color = "forestgreen") +
  geom_point(data = bg_stars, aes(x = x, y = y), shape = 8, size = 4, color = "white") +
  geom_star(data = data.frame(x = 0.485, y = 1.55), aes(x = x, y = y), starshape = 9, size = 14, fill = "gold") +
  geom_text(aes(x = 0.5, y = -0.5), label = "Wesołych Świąt!", color = "white", size = 13, fontface = "bold") +
  theme_void() +
  coord_fixed(ratio = 1) +
  theme(panel.background = element_rect(fill = "#22049C")) +
  expand_limits(x = c(min(all_triangles_df$x) - 1, max(all_triangles_df$x) + 1),
                y = c(min(all_triangles_df$y) - 0.7, max(all_triangles_df$y) + 0.2))
