library(tidyverse)
library(maps)
library(ggplot2)

df <- read_csv('HW 3/GDP.csv')
mapka <- map_data("world")

mapka %>%
  right_join(df, by = c("region" = "Country")) -> mapka

pal = c("#ADE8F4", "#48CAE4", "#006399", "#03045E")
scale_fill_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "fill", ...) {
  binned_scale("fill", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, guide = guide, ...)  
}

  mapka %>%
    mutate(GDPlog = log10(GDP)) %>%
    ggplot(mapping = aes(x = long, y = lat, group = group, fill = GDP)) +
    geom_polygon(color = "black") +
    coord_map("mollweide", xlim = c(-9, 35), ylim = c(35, 70)) +
    labs(title = "GDP per capita in Europe in 2022") +
    theme_void() +
    theme(legend.position = "left",
          plot.title = element_text(face = "bold", size = 20),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12)) +
    scale_fill_fermenter_custom(pal,
                                trans = "log10",
                                labels = scales::label_number(scale = 1, suffix = "", accuracy = 1))
  
