library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)

# Parametry --------------------------------------------------------------------
detal_galezi = 7
ilosc_galezi = 10
# ilosc_bombek_zlatych = 50
# ilosc_bombek_srebrnych = 25
detal_lancucha = 5
rozrzut_lancucha = 0.05
przechyl = -1.5
rozrzut_galezi = 0.2

# Przygotowanie danych ---------------------------------------------------------
df <- data.frame()
for (j in 1:ilosc_galezi){
  x <- c()
  z <- c()
  for (i in j:ilosc_galezi){
    x_new <- c(-seq((i-j)*0.1, (i-j+1)*0.1, length.out = detal_galezi), seq((i-j)*0.1, (i-j+1)*0.1, length.out = detal_galezi))
    x <- c(x, x_new)
    z <- c(z, rep(i, detal_galezi*2))}
  y <- j+abs(x)*przechyl+runif(length(x), -rozrzut_galezi, rozrzut_galezi)
  df <- bind_rows(df, data.frame(x, y, z), data.frame(x = rep(0, detal_galezi), y = seq(j-1, j, length.out = detal_galezi), z = j-1))}
df <- df %>% mutate(x = round(x, 3), y = round(y, 3), czesc = "choinka")
df <- bind_rows(df, data.frame(x = rep(0, detal_galezi), y = seq(-1, 0, length.out = detal_galezi), z = 0, czesc = "pien"))
# bombki <- df %>% filter(czesc != "pien") %>% sample_n(ilosc_bombek_zlatych) %>% mutate(z = ilosc_galezi, czesc = "bombka_zlota")
# df <- bind_rows(df %>% filter(!(x %in% bombki$x & y %in% bombki$y)), bombki)
# bombki <- df %>% filter(czesc != "pien" & czesc != "bombka_zlota") %>% sample_n(ilosc_bombek_srebrnych) %>% mutate(z = ilosc_galezi+2, czesc = "bombka_srebrna")
# df <- bind_rows(df %>% filter(!(x %in% bombki$x & y %in% bombki$y)), bombki)
for (j in 2:ilosc_galezi-1){
  x <- c(seq((ilosc_galezi-j)*0.1, (ilosc_galezi-j+1)*0.1, length.out = detal_lancucha))
  z <- c(rep(ilosc_galezi+1+j, detal_lancucha))
  for (i in j:ilosc_galezi-1){
    x <- c(x, c(-seq((i-j)*0.1, (i-j+1)*0.1, length.out = detal_lancucha), seq((i-j)*0.1, (i-j+1)*0.1, length.out = detal_lancucha)))
    z <- c(z, rep(ilosc_galezi+1+j, detal_lancucha*2))}
  y <- c(x*przechyl+j+runif(length(x), -rozrzut_lancucha, rozrzut_lancucha))
  df <- bind_rows(df, data.frame(x, y, z, czesc = "lancuch"))}
df <- df %>% filter((y < x * 15 + 13 & czesc == "lancuch") | czesc != "lancuch")
df <- bind_rows(df, data.frame(x = rep(0, 3), y = rep(ilosc_galezi+0.25, 3), z = c(ilosc_galezi*2+1, ilosc_galezi*2+1, ilosc_galezi*2+5), czesc = c("gwiazda1", "gwiazda2", "gwiazda1")))

# Tworzenie wykresu ------------------------------------------------------------
p <- ggplot(data = df, aes(x = x, y = y, color = czesc)) +
  geom_point(aes(size = recode(czesc,"choinka" = 6, "pien" = 7, "bombka_zlota" = 10, "bombka_srebrna" = 8, "lancuch" = 5, "gwiazda1" = 10, "gwiazda2" = 20),
                 shape = czesc)) +
  scale_color_manual(breaks = c("choinka", "pien", "bombka_zlota", "bombka_srebrna", "lancuch", "gwiazda1", "gwiazda2"),
                     values = c("#089900", "#993b00","#ffe100", "#fffefa", "#fa2525", "#fff700", "#fff700")) +
  scale_shape_manual(breaks = c("choinka", "pien", "bombka_zlota", "bombka_srebrna", "lancuch", "gwiazda1", "gwiazda2"),
                     values = c(19, 15, 19, 19, 8, 15, 18))+
  scale_size(range = c(5, 20)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = '#0362fc', colour = '#0362fc'),
        panel.border = element_rect(fill = NA, colour = '#0362fc'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        panel.grid = element_blank()) +
  transition_time(z) +
  shadow_mark()
animate(p, renderer = gifski_renderer(loop = T))
# anim_save("choinka.gif")
