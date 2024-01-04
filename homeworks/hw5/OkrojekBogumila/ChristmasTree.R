library(tidyr)
library(dplyr)
library(plotly)

n = 5000

#    1   dodatnie
y1 <- runif(n = n, min = 0, max = 100)

x1 <- mapply(function(y_val) runif(n = 1, min = 0, max = 0.5*y_val), y1)


z1 <- mapply(function(y_val, x_val) runif(n = 1, min = 0, max = sqrt((0.25*((y_val)^2)) - x_val^2)), y1, x1)


df1 <- data.frame(x1,y1,z1)
colnames(df1) <- c("x","y","z")


####   2    x dodatnie , z ujemne

y2 <- runif(n = n, min = 0, max = 100)


x2 <- mapply(function(y_val) runif(n = 1, min = 0, max = 0.5*y_val), y2)



z2 <- mapply(function(y_val, x_val) runif(n = 1, min = -sqrt((0.25*((y_val)^2)) - x_val^2), max = 0), y2, x2)



df2 <- data.frame(x2,y2,z2)
colnames(df2) <- c("x","y","z")
####    3   x ujemene , z dodatnie

y3 <- runif(n = n, min = 0, max = 100)



x3 <- mapply(function(y_val) runif(n = 1, min = -0.5*y_val, max = 0), y3)



z3 <- mapply(function(y_val, x_val) runif(n = 1, min = 0, max = sqrt((0.25*((y_val)^2)) - x_val^2)), y3, x3)



df3 <- data.frame(x3,y3,z3)
colnames(df3) <- c("x","y","z")

####    4  ujemne

y4 <- runif(n = n, min = 0, max = 100)



x4 <- mapply(function(y_val) runif(n = 1, min = -0.5*y_val, max = 0), y4)



z4 <- mapply(function(y_val, x_val) runif(n = 1, min = -sqrt((0.25*((y_val)^2)) - x_val^2), max = 0), y4, x4)



df4 <- data.frame(x4,y4,z4)
colnames(df4) <- c("x","y","z")


df <- rbind(df1, df2, df3, df4)



colNr <- sample(1:10, 4*n, replace = TRUE)


diff <- 1

xAdd <- runif(n = n, min = -diff, max = diff)
zAdd <- runif(n = n, min = -diff, max = diff)


dff <- df %>% 
  mutate(x = x + 0.5 *x * xAdd) %>% 
  mutate(z = z + 0.5 * z * zAdd)

colors <- c("#00330a", "#01410d", "#135d16", "#2b7413", "#1a812e", "#0c8835", "#039e12", "#56a82f", "#8ccc35", "#add36e")

dff1 <- data.frame(dff,colNr) 
dff <- dff1 %>% 
  arrange(y) %>% 
  mutate(y = -y)



#markers

bn = 50

###### bombki

yb1 <- sample(1:100, bn, replace = TRUE)
xb1 <- mapply(function(y_val) runif(n = 1, min = 0, max = 0.5*y_val), yb1)
zb1 <- mapply(function(y_val, x_val) sqrt(0.25*(y_val)^2 - x_val^2), yb1,xb1)

dfb1 <- data.frame(xb1,yb1,zb1)
colnames(dfb1) <- c("x","y","z")

yb2 <- sample(1:100, bn, replace = TRUE)
xb2 <- mapply(function(y_val) runif(n = 1, min = -0.5*y_val, max = 0), yb2)
zb2 <- mapply(function(y_val, x_val) sqrt(0.25*(y_val)^2 - x_val^2), yb2,xb2)

dfb2 <- data.frame(xb2,yb2,zb2)
colnames(dfb2) <- c("x","y","z")

yb3 <- sample(1:100, bn, replace = TRUE)
xb3 <- mapply(function(y_val) runif(n = 1, min = 0, max = 0.5*y_val), yb3)
zb3 <- mapply(function(y_val, x_val) -sqrt(0.25*(y_val)^2 - x_val^2), yb3,xb3)

dfb3 <- data.frame(xb3,yb3,zb3)
colnames(dfb3) <- c("x","y","z")

yb4 <- sample(1:100, bn, replace = TRUE)
xb4 <- mapply(function(y_val) runif(n = 1, min = -0.5*y_val, max = 0), yb4)
zb4 <- mapply(function(y_val, x_val) -sqrt(0.25*(y_val)^2 - x_val^2), yb4,xb4)

dfb4 <- data.frame(xb4,yb4,zb4)
colnames(dfb4) <- c("x","y","z")

yb5 <- sample(50:100, bn, replace = TRUE)
xb5 <- mapply(function(y_val) runif(n = 1, min = -0.5*y_val, max = -0.3*y_val), yb5)
zb5 <- mapply(function(y_val, x_val) -sqrt(0.25*(y_val)^2 - x_val^2), yb5,xb5)

dfb5 <- data.frame(xb5,yb5,zb5)
colnames(dfb5) <- c("x","y","z")

yb6 <- sample(50:100, bn, replace = TRUE)
xb6 <- mapply(function(y_val) runif(n = 1, min = -0.5*y_val, max = -0.3*y_val), yb6)
zb6 <- mapply(function(y_val, x_val) sqrt(0.25*(y_val)^2 - x_val^2), yb6,xb6)

dfb6 <- data.frame(xb6,yb6,zb6)
colnames(dfb6) <- c("x","y","z")

yb7 <- sample(50:100, bn, replace = TRUE)
xb7 <- mapply(function(y_val) runif(n = 1, min = 0.3*y_val, max = 0.5*y_val), yb7)
zb7 <- mapply(function(y_val, x_val) -sqrt(0.25*(y_val)^2 - x_val^2), yb7,xb7)

dfb7 <- data.frame(xb7,yb7,zb7)
colnames(dfb7) <- c("x","y","z")

yb8 <- sample(50:100, bn, replace = TRUE)
xb8 <- mapply(function(y_val) runif(n = 1, min = 0.3*y_val, max = 0.5*y_val), yb8)
zb8 <- mapply(function(y_val, x_val) sqrt(0.25*(y_val)^2 - x_val^2), yb8,xb8)

dfb8 <- data.frame(xb8,yb8,zb8)
colnames(dfb8) <- c("x","y","z")


dfb <- rbind(dfb1, dfb2, dfb3, dfb4, dfb5, dfb6, dfb7, dfb8)


xbAdd <- runif(n = bn, min = -diff, max = diff)
zbAdd <- runif(n = bn, min = -diff, max = diff)


dffb1 <- dfb %>% 
  mutate(y = -y) %>% 
  mutate(x = x + 0.5 *x * xbAdd) %>% 
  mutate(z = z + 0.5 * z * zbAdd)

colNr <- sample(1:4, 8*bn, replace = TRUE)
col <- c("#5c3698", "#e3c900", "#19aae1", "#f65a9d")

dffb <- data.frame(dffb1, colNr)

plot_ly(dff, y = ~x, x = ~z,  z = ~y, type = "scatter3d", mode = "lines",
                marker = list(size = 3, opacity = 0.7), color = ~colNr,
                colors = colors,
                hovertemplate = paste("x: %{x}<br>y: %{y}<br>z: %{z}"), showlegend = FALSE) %>%
  add_markers(data = dffb %>% filter(colNr == 1), x = ~z,
               y = ~x,
               z = ~y,
               marker = list(size = 8, opacity = 1, color = col[1]),
               mode = "markers", showlegend = TRUE,
              name = "purple") %>% 
  add_markers(data = dffb %>% filter(colNr == 2), x = ~z,
              y = ~x,
              z = ~y,
              marker = list(size = 8, opacity = 1, color = col[2]),
              mode = "markers", showlegend = TRUE,
              name = "yellow") %>% 
  add_markers(data = dffb %>% filter(colNr == 3), x = ~z,
              y = ~x,
              z = ~y,
              marker = list(size = 8, opacity = 1, color = col[3]),
              mode = "markers", showlegend = TRUE,
              name = "blue") %>% 
  add_markers(data = dffb %>% filter(colNr == 4), x = ~z,
              y = ~x,
              z = ~y,
              marker = list(size = 8, opacity = 1, color = col[4]),
              mode = "markers", showlegend = TRUE,
              name = "pink") %>% 
  hide_colorbar() %>%
  layout(title = list(text = "<b><i>Merry Christmas</i></b>", x = 0.5, y = 0.9, font = list(size=30, color = "#c20505")), 
         legend = list(title = list(text = "Baubles' colours:")),
         paper_bgcolor = "black",
         hovermode = FALSE,
         scene = list(xaxis = list(showgrid = FALSE, visible = FALSE),
                      yaxis = list(showgrid = FALSE, visible = FALSE),
                      zaxis = list(showgrid = FALSE, visible = FALSE)))
