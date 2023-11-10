### libraries

library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(viridis)

df <- read.csv("data.csv")

### modify data frame 

prepared <- df %>%  filter((sex == "Males") & (!is.na(OBS_VALUE))) %>% 
  select(geo, OBS_VALUE, OBS_FLAG) %>% 
  mutate(Females = 100 - OBS_VALUE) %>% 
  mutate(geo = ifelse(geo == "Czechia", "Czech Republic", geo))

### prepare data frame 

european_countries <- c(
  "Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
  "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland",
  "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Kosovo",
  "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco",
  "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal",
  "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden",
  "Switzerland", "Ukraine", "United Kingdom", "Vatican City"
)

eu_map <- map_data("world", region = european_countries) %>% 
  filter(lat < 70)

eu_lab_data <- eu_map %>% 
  group_by(region) %>% 
  summarise(longtitude = mean(long), latitude = mean(lat))


plot_data <- left_join(eu_map, prepared, by = join_by(region == geo))
  

### generate plot

ggplot(plot_data, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = Females), color = "black") + 
  theme_void()+
  scale_fill_gradient2(
    low = "gold4", high = "#01796f", mid = "lemonchiffon",
    limits = c(5, 30), midpoint = 15.6,
    guide = guide_legend( keyheight = unit(4, units = "mm"), 
                          keywidth=unit(5, units = "mm"),
                          label.position = "right",
                          title.position = 'top',
                          nrow=6,
                          title = "EU = 15.6"
                          )
  )+
   labs(title = "Employed women with ICT education",
       subtitle = "as percentage of all employed people with ICT education, 2022",
       caption = "Low reliability data for women for Malta, Austria, Portugal, Croatia and Belgium. 
       Definition differs for Spain and France (see LFS methodology).
       ") +
    theme(
      text = element_text(color = "black"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      
      plot.title = element_text(size=16, hjust = 0.01, color="black", face = "bold",
                                margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", 
                                   margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
      plot.caption = element_text( size=7, color = "#4e4d47", margin = 
                                     margin(b = 0.3, r=-99, unit = "cm"), face = "italic"),
  
      legend.position = c(1, 0.68),
      
      legend.title = element_text(color = "#4e4d47", size = 12),
      legend.text = element_text(color = "#4e4d47", face = "bold", size = 9)
      
    ) + 
  coord_map()
  

