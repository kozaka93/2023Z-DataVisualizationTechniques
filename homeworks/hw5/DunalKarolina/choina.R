library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)
library(png)
library(extrafont)
library(showtext)

font_import() 
loadfonts() 
showtext_auto()
font_add("Bakso Sapi", "/Users/Karolina/Library/Fonts/BaksoSapi-4BmlB.ttf")

bazaReniferow <- data.frame(idRenifera = c(16, 0, 15, 1, 14, 2, 13, 3, 12, 4, 11, 5, 10, 6, 9, 7, 8),
                   wiek = seq(0, 8, by = 0.5))
tree <- bazaReniferow

star <- data.frame(x = 8, y = 8.25)

fabrykaPrezentow <- data.frame(
  zyskNa100Szt = runif(10000, -2, 18),
  sredniCzasProdukcji = runif(10000, 0, 9.5),
  idElfaProdukujacego = rep(1:100, each = 100),
  idPrezentu = seq(1,1000,1))
snowflakes <- fabrykaPrezentow

christmas_tree <- ggplot() + 
  geom_path(data = tree, aes(idRenifera, wiek), size = 9, color = '#165B33') + 
  geom_point(data = star, aes(x, y), shape = 24, size = 15, fill = "#F8B229", color = "#F8B229") +
  geom_point(data = star, aes(x, y), shape = 25, size = 15, fill = "#F8B229", color = "#F8B229") +
  geom_point(data = snowflakes, aes(zyskNa100Szt, sredniCzasProdukcji), color = "white", pch = 42, size = 4) +
  labs(title = "Merry Christmas") + 
  xlab("") + ylab("") + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        plot.background = element_rect(fill = "#b11e31", color = "#b11e31"),
        panel.background = element_rect(fill = "#b11e31", color = "#b11e31"),
        strip.background = element_rect(fill = NA, color = NA),
        panel.border = element_rect(fill = NA, color = NA),
        plot.title = element_text(family = "Bakso Sapi", face = "bold", color = "white", size = 33, hjust = 0.5)) +
  transition_states(snowflakes$idElfaProdukujacego, transition_length = 1, state_length = 1)

animate(christmas_tree)

anim_save("/Users/Karolina/Desktop/uni/3_semestr/TWD/DunalKarolina/choina.gif", christmas_tree)

