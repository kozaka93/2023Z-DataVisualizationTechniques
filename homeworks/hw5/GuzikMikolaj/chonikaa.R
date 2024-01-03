par(bg = 1, pch = 20, mar = rep(0, 4))
plot(0, xlim = c(-1.25, 1.25), axes = T, xlab = "", ylab = "", ylim = c(-100, 550))

rect(-0.1, -100, 0.1, 0, col = "brown", border = "brown")

z <- (500:1)/1000

y <- 0
for(i in rep(exp(-(1:125)* 0.005), 4) * z + z)
{
  y <- y + 1
  
  for(x in seq(-i, i, 0.1))
  {
    points(x, y, col = "darkgreen")
  }
}

points(0, 520, pch = 8, cex = 3, col = "gold",border = "gold")

points(runif(6, -0.77, 0.77), runif(6, 0, 100), pch = 20, cex = 3, col = sample(c("red","gold","blue")))
points(runif(7, -0.5, 0.5), runif(7, 125, 300), pch = 20, cex = 3, col = sample(c("red","gold","blue")))
points(runif(4, -0.2, 0.2), runif(4, 300, 370), pch = 20, cex = 3, col = sample(c("red","gold","blue")))

points(runif(75, -1.25, 1.25), runif(75, -100, 500+50), col = "white", cex = sample(seq(0.5, 1.2, length.out = 75)), pch = "*")
points(runif(100, -1.25, 1.25), runif(100, -100, -95), col = "white", cex = sample(seq(0.5, 1.2, length.out = 75)), pch = "️*")


