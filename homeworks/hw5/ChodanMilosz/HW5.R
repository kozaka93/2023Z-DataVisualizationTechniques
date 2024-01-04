library(rgl)
library(htmlwidgets)

draw_christmas_tree <- function() {
  clear3d()
  
  bg3d(color="navy")
  
  for (s in seq(0, 300, 25)) {
    z <- s
    x <- cos(seq(1, 360, 5) * pi / 180) * (300 - s)
    y <- sin(seq(1, 360, 5) * pi / 180) * (300 - s)
    points3d(x, y, z, col='green', size=2)
  }
  
  points3d(0, 0, 301, col='gold', size=8)
  
  z <- sample(0:300, 100, replace=TRUE)
  x <- sample(-500:500, 100, replace=TRUE)
  y <- sample(-500:500, 100, replace=TRUE)
  points3d(x, y, z, col='white', size=2)
  
  axes3d(tick=FALSE)
  
  aspect3d(1, 1, 1)
}

draw_christmas_tree()
widget <- rglwidget()
saveWidget(widget, file = "HW5.html")










