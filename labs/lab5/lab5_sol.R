###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 5            ###
########################################### 

library(ggplot2)
library(dplyr)
library(SmarterPoland)


## Zadanie 1
# Zadania są zamieszczone w pliku .pdf w folderze lab5.
# Dane potrzebne do odtworzenia wizualizacji wczytujemy następująco:

df <- read.csv(file = "https://raw.githubusercontent.com/MI2-Education/2023Z-DataVisualizationTechniques/main/homeworks/hw1/house_data.csv", 
               encoding = "UTF-8")

options(scipen = 7)

p1 <- df %>% 
  mutate(sqft_living2 = 
           case_when(sqft_living <= 2000 ~ '(0, 2000]',
                     sqft_living <= 4000 ~ '(2000, 4000]',
                     sqft_living > 4000 ~ '(4000, +Inf)')) %>% 
  ggplot(aes(x = sqft_living2, y = price/1000, color = factor(waterfront))) + 
  geom_boxplot() + 
  theme_bw() + 
  coord_flip() +
  theme(legend.position = "top",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) + 
  scale_color_manual(values = c("darkred", "navyblue")) +
  labs(y = "Price [1k $]",
       x = "Living area [sqft]",
       color = "Waterfont")

## Zadanie 2
# Posługując się danymi z *Zadania 1* należy odwzorować poniższy wykres.

p2 <- df %>%
  group_by(zipcode) %>%
  summarise(n = n()) %>%
  filter(n > quantile(n, 0.95)) %>%
  left_join(df) %>%
  filter(yr_built == 2013 | yr_built == 2014, 
         grade == 8 | grade == 9) %>%
  ggplot(aes(x = factor(zipcode), y = price / 1000)) +
  geom_boxplot() +
  facet_grid(grade ~ yr_built) +
  theme_bw() +
  labs(
    x = "Zipcode",
    y = "Price [1k $]",
    title = "Distribution of property prices for zip codes in 2013-2014 \nfor properties with grade of 8 or 9.",
    subtitle = "We consider the 5% most numerous zip codes."
  ) +
  theme(
    plot.subtitle = element_text(face = "italic"),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    strip.background = element_rect(fill = "navyblue"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


## patchwork

install.packages("patchwork")
install.packages("grid")
install.packages("gridExtra")


library(patchwork)

p2 + p1

p1 / p2


(p1 | p2 | p1) / p2

p1 + grid::textGrob('Jakiś tekst')

## Zadanie 3
# Przygotuj tabelę z podsumowaniem ile nieruchomości znajduje się dla poszczególnych kodów pocztowych i lat z wykresu.

tab <- df %>%
  group_by(zipcode) %>%
  summarise(n = n()) %>%
  filter(n > quantile(n, 0.95)) %>%
  left_join(df) %>%
  filter(yr_built == 2013 | yr_built == 2014, 
         grade == 8 | grade == 9) %>% 
  group_by(zipcode, yr_built) %>% 
  summarise(n = n()) %>% 
  tidyr::pivot_wider(values_from = 'n', names_from = 'yr_built') %>% 
  as.data.frame()

row.names(tab) <- tab$zipcode

p2 + gridExtra::tableGrob(tab[, c(2,3)])


# no working
grid::textGrob('Jakiś tekst') + p1

wrap_elements(grid::textGrob('Jakiś tekst')) + p1


## Zadanie 4
# Utwórz nową zmienną `is_renovated`, która będzie przyjmować wartość TRUE jeżeli była renowacja i FALSE gdy jej nie było. 
# Przygotuj wykres ukazujący rozkład powierzchni mieszkalnej dla domów w podziale na liczbę pięter i status renowacji.

p3 <- df %>% 
  mutate(is_renovated = yr_renovated != 0) %>% 
  ggplot(aes(x = as.factor(floors), y = sqft_living, fill = is_renovated)) + 
  geom_violin() + 
  labs(x = "Floors",
       y = "Living area [sqft]", 
       fill = "Renovated") + 
  theme_bw()

(p1 / p2) & theme_minimal()

?patchwork::plot_layout
(p1 / p3) +
  patchwork::plot_layout(heights = c(3, 1))

?patchwork::plot_annotation
(p1 / p3) +
  patchwork::plot_annotation(title = "Raport",
                             subtitle = "Tutaj miejsce na podtytuł.",
                             caption = Sys.time()) &
  labs(title = NULL) &
  theme_classic()

(p3 + p1) / p2 + plot_layout(guides = 'collect')
p3 + p2 + p1 + guide_area() + plot_layout(guides = 'collect')


## Zadanie 5 - stworzyć wykres gęstości brzegowych:
# a) wykres punktowy dwóch wskaźników + kolor
# b) dodać po lewej rozkład zmiennej death.rate
# c) dodać na dole rozkład zmiennej birth.rate

main_plot <- ggplot(data = countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_point()

density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  coord_flip() +
  scale_y_reverse() +
  theme(legend.position = "none")


density_birth <- ggplot(data = na.omit(countries), aes(x = birth.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  scale_y_reverse() +
  theme(legend.position = "none")

density_death + main_plot + plot_spacer() + density_birth +
  plot_layout(ncol = 2, heights = c(0.7, 0.3), widths = c(0.3, 0.7))

density_death + main_plot + guide_area() + density_birth +
  plot_layout(ncol = 2, heights = c(0.7, 0.3), widths = c(0.3, 0.7), guides = 'collect')


## ggrepel
# https://ggrepel.slowkow.com/articles/examples.html

install.packages("ggrepel")

library(ggrepel)

p_points <-  ggplot(countries, aes(x = birth.rate, y = death.rate, label = country)) +
  geom_point() 

p_points + geom_text()

?ggrepel::geom_text_repel
p_points + geom_text_repel() # Warning!
