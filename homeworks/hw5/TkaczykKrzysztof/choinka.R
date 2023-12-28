library(plotly)
library(dplyr)

#promień podstawy
R <- c(8.5,9,6.5,4.5)
#promień wodzący
L <- c(16,16,12,7)
#wysokość stożka
H <- sqrt(L^2 -R^2)

liczba_stopni <- length(R)
liczba_igiel <- c(120,120,60,30)
liczba_galezi <- c(20,20,15,10) 

przesuniecie <- 0

#generowanie choinki
for( i in 1:length(R)){
#generowanie punktów na okręgu
theta <- seq(0,2*pi,length.out = liczba_igiel[i])
theta <- rep(theta,liczba_galezi[i])  
  
#generowanie stożka stożków
przesuniecie <- ifelse(i > 2, przesuniecie + H[i-1]/2, 0)  
h <- seq(0,H[i],length.out = liczba_galezi[i])

#promień dla każdej warstwy
r <- (R[i]*(H[i]-h))/H[i]

tmp <- tibble(r = rep(r,each=liczba_igiel[i]), h = rep(h,each = liczba_igiel[i]) + przesuniecie,theta = theta)

tmp <- tmp %>% mutate(x = r * cos(theta),
                    y = r * sin(theta),
                    u = -x,
                    v = -y,
                    w = H[i] + przesuniecie) 
if(i == 1){
  df <- tmp
}else{
  df <- rbind(df,tmp)
}
}


df %>% mutate(idx = 1:length(df$r)) ->df


bombki = sample(2400:length(df$r), length(df$r)/5)

df <- df %>% 
  mutate(color = ifelse(idx %in% bombki & x != 0 & y != 0, 
                         TRUE,
                         FALSE
                         ) 
          ) 
df_tylko_bombki <- df %>% filter(color)
df_nie_bombki <- df %>% filter(!color,x!=0, y!=0)
df_czubek <- df %>% filter(x == 0, y == 0)

kolory <- grDevices::rainbow(length(bombki))
kolory <- sample(kolory,length(kolory), replace = FALSE)

p <- plot_ly(
) 

p <- p %>% add_trace(
  type = "cone",
  name = "igła",
  sizemode= 'scaled',
  showscale = F,
  sizeref = 40,
  cmin = 100,
  x = df_nie_bombki$x,
  y = df_nie_bombki$y,
  z = df_nie_bombki$h,
  u = df_nie_bombki$u,
  v = df_nie_bombki$v,
  w = df_nie_bombki$w,
  colors = c("#005A32")
)

p <- p %>% add_trace(
  type = "scatter3d",
  name = "bombka",
  x = df_tylko_bombki$x,
  y = df_tylko_bombki$y,
  z = df_tylko_bombki$h,
  mode = "markers",
  marker= list(color = kolory, 
               size = 4),
  showlegend = FALSE
)

p <- p %>% add_trace(
  type = "cone",
  name = "czubek",
  sizemode= 'scaled',
  showscale = F,
  sizeref = 7,
  cmin = 100,
  name = "czubek",
  x = df_czubek$x,
  y = df_czubek$y,
  z = df_czubek$h,
  u = df_czubek$u,
  v = df_czubek$v,
  w = df_czubek$w,
  colorscale = list (list(0,"yellow"),list(1, "yellow")),
  sizeref = 3
)

n = 15
df_snieg <- tibble( x = rep(runif(500,min= -max(R + 5), max = max(R + 5)), n),
                    y = rep(runif(500,min= -max(R + 5), max = max(R + 5)), n),
                    z = rep(runif(500,min= 0, max = max(df$h + 5)),n),
                    group = rep(1:n, each = 500))

df_snieg <- df_snieg %>%  mutate(z = ifelse(group > 1, (z-0.5*group)%%max(df$h + 5), z))

p <- p %>% add_trace(
  type = "scatter3d",
  name = "śnieg",
  x = df_snieg$x,
  y = df_snieg$y,
  z = df_snieg$z,
  frame=df_snieg$group,
  mode = "markers",
  marker= list(color = "white", 
               size = 1),
  showlegend = FALSE
) %>% animation_opts(800,
                     redraw = TRUE)  %>% 
  animation_slider(hide = TRUE)

p <- p %>% layout(
  scene = list(zaxis = list(backgroundcolor="white",
                            showbackground=TRUE,
                            showgrid = FALSE,
                            showticklabels = FALSE,
                            title = ""
                            ),
               xaxis = list(backgroundcolor="#000080",
                            showbackground=TRUE,
                            showgrid = FALSE,
                            showticklabels = FALSE,
                            title = ""
                            ),
               yaxis = list(backgroundcolor="#000080",
                            showbackground=TRUE,
                            showgrid = FALSE,
                            showticklabels = FALSE,
                            title = ""
                            ),
               camera = list(eye = list(x = -0.95, y = 0.9, z = 0.5)))
)

p
