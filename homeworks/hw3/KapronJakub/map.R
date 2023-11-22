library(dplyr)
library(geojsonio)
library(ggplot2)
library(broom)
library(ggthemes)

poles <- readr::read_delim("prace_domowe/hw3/12521-0041_flat.csv", delim = ";")

poles <- poles %>% 
  select(`1_variable_code...8`, `2_variable_code...13`,
         `3_variable_code...17`, `BEV027__Foreigners__number,`)

colnames(poles) <-c('region', 'sex', 'nationality', 'number')

poles <- poles %>% 
  filter(sex == "Total", nationality == 'Poland') %>% 
  select(region, number)

kreis <- geojson_read("prace_domowe/hw3/gadm41_DEU_2.json",
                           what = "sp")
kreis$id <- rownames(kreis@data)

kreisFort <- tidy(kreis)

land <- geojson_read("prace_domowe/hw3/gadm41_DEU_1.json",
                     what = "sp")

kreis <- merge(kreis, poles, by.x = "CC_2", by.y = "region")

data <- merge(kreisFort, kreis@data, by = "id")

map <- ggplot() +
  geom_polygon(data = data, mapping = aes(x = long, y = lat, group = group,
                                          fill = number), color = "grey32") +
  geom_polygon(data = land, aes(x = long, y = lat, group = group), color = "black",
               fill = NA, size = 0.9) +
  theme_map() +
  scale_fill_fermenter(direction = 1, palette = "Reds",
                       breaks = c(1000, 3000, 10000, 60000)) +
  coord_map() +
  labs(title = "Liczba Polaków nieposiadających niemieckiego obywatelstwa
zamieszkujących dany Landkreis (lub odpowiednik)",
       subtitle = "stan na 31.12.2022",
       caption = "źródło: Statistisches Bundesamt") +
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"))

ggsave("prace_domowe/hw3/map.png", plot = map, height = 8, width = 5.5, 
       dpi = 600, bg = "white")
