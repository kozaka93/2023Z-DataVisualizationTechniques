#źródło danych: https://ec.europa.eu/eurostat/databrowser/view/tps00003/default/table?lang=en


df <- read.csv("/Users/mateuszdeptuch/RStudio/TWD/pd3/density.csv", sep = ';')
df <- as.data.frame(apply(df, c(1, 2), function(x) ifelse(x == ":", NA, x)))
colnames(df)[1] <- "region"


library(ggplot2)
library(maps)
library(dplyr)

df <- df%>%
  select(region, X2022)
df$X2022 <- as.numeric(gsub(",", ".",df$X2022))

europe_map <- map_data("world")
europe_map <- europe_map[europe_map$region %in% rbind(df$region , "Russia", "Ukraine", "Belarus", "Moldova", "Bosnia and Herzegovina", "Kosovo"),]
europe_map <- europe_map%>%
  left_join(df, by = 'region')

ggplot(europe_map, mapping = aes(x = long, y = lat, group = group))+
  coord_map( "mollweide", xlim = c(-10, 34), ylim = c(35, 70)) + 
  theme_void()+
  geom_polygon(color = "black", aes(fill = X2022)) + 
  scale_fill_viridis_c(option = 'G',
                       trans = 'log10',
                       na.value = "darkgrey",
                       direction = -1) +  
  
  labs(title = "Population Density in EU Coutries in 2022",
       subtitle = " ",
       fill = "Density
(persons / km^2)") +
  theme(legend.position =  "top",
        plot.title = element_text( size = 20, face = 'bold', hjust = 0.5),
        legend.title  = element_text(face = 'bold'),
        legend.text = element_text(face = 'bold')) 


ggsave("plot.jpg", path = "/Users/mateuszdeptuch/RStudio/TWD/pd3", device = jpeg ) 
  



