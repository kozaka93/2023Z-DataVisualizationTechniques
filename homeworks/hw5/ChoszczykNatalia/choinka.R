library(rgl)
library(magrittr)

t <- seq(0.1, 10*pi, 0.01)
x <- seq(10*pi, 0.1, -0.01) * cos(t)
y <- seq(10*pi, 0.1, -0.01) * sin(t)
z <- t

plot3d(
  x = c(x * 1.2,
        x * 1.1,
        x,
        x * 0.9,
        x * 0.8,
        x * 0.7,
        x * 0.6,
        x * 0.5,
        x * 0.4,
        x * 0.3,
        x * 0.2,
        x * 0.1),
  
  y = c(y * 1.2,
        y * 1.1,
        y,
        y * 0.9,
        y * 0.8,
        y * 0.7,
        y * 0.6,
        y * 0.5,
        y * 0.4,
        y * 0.3,
        y * 0.2,
        y * 0.1),
  # wiem że tak średnio ładnie jest to zdefinowane, ale dużo kombinowałam i 
  # niestety nie udało mi się zrobić tego inaczej żeby poprawnie działało
  z = rep(z, times = 12),
  col = rep(c("darkgreen", "green", "lightgreen", "red"), each = length(t)),
  axes = FALSE,
  xlab = NULL, ylab = NULL, zlab = NULL,
  size = 2
)

text3d(x = 0, y = 0, z = max(z) + 10, text = "Merry Christmas!", adj = c(0.5, 0.5, 0), cex = 2, font = 2, col = "white")

rgl.bg(color = "black")
par3d(windowRect = c(20, 20, 400, 400))

play3d(spin3d(axis = c(0, 0, 1), rpm = 25), duration = 15)

movie3d(
  movie = "choinka",
  spin3d(axis = c(0, 0, 1), rpm = 25),
  duration = 10,
  dir = getwd(),
  type = "gif",
  clean = TRUE
)


# sciezka <- "C:\Users\natal\Desktop\SEM_3\TWD"
# # Zapisanie animacji jako pliku GIF
# movie3d(spin3d(axis = c(0, 0, 1), rpm = 25), duration = 10, movie = paste0(sciezka, "/choinka"), clean = TRUE)
# 
# # Odczyt i konwersja pliku PNG do pliku GIF
# anim <- image_animate(image_read(paste0(sciezka, "/choinka*.png")), fps = 25)
# image_write(anim, paste0(sciezka, "/choinka.gif"), format = "gif")


