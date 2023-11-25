library(dplyr)
library(maps)
library(mapdata)
library(patchwork)
library(plotly)


world <- map_data("world")

internet_users <- read.csv('Final.csv') %>% 
  mutate(Entity = ifelse(Entity == 'United States', 'USA', Entity)) %>% 
  mutate(Entity = ifelse(Entity == 'United Kingdom', 'UK', Entity)) %>% 
  mutate(Entity = ifelse(Entity == 'Czechia', 'Czech Republic', Entity)) %>% 
  mutate(Entity = ifelse(Entity == 'Democratic Republic of Congo', 'Democratic Republic of the Congo', Entity)) %>% 
  mutate(Entity = ifelse(Entity == 'Congo', 'Republic of Congo', Entity)) 
  
  
  
  


colnames(internet_users)[7] <- 'internet_users'
colnames(internet_users)[6] <- 'percent_of_people_using_internet'



internet_1990 <- internet_users %>% 
  filter(Year == 1990) 

internet_1990<- world %>% 
  left_join(internet_1990, by = c('region' = 'Entity'), keep =TRUE) %>% 
  filter(region != 'Antarctica')



internet_2000 <- internet_users %>% 
  filter(Year == 2000) 

internet_2000 <-world %>% 
  left_join(internet_2000, by = c('region' = 'Entity'), keep =TRUE) %>% 
  filter(region != 'Antarctica')


internet_2010 <- internet_users %>% 
  filter(Year ==2010) 

internet_2010 <- world %>% 
  left_join(internet_2010, by = c('region' = 'Entity'), keep =TRUE) %>% 
  filter(region != 'Antarctica')

internet_2020 <- internet_users %>% 
  filter(Year == 2020) 

internet_2020 <- world %>% 
  left_join(internet_2020, by = c('region' = 'Entity'), keep =TRUE) %>% 
  filter(region != 'Antarctica')
  
  
  
  
ggplot(data = internet_1990) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = percent_of_people_using_internet ), color = alpha('white', 0))+
  coord_fixed(1.3) +
  ggtitle('year 1990')+
  labs(x = NULL, y = NULL)+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),  
    legend.title = element_text(color = "white"),
    plot.title = element_text(colour = 'white', hjust = 0.5))+
  scale_fill_gradient(name = "Percentage",
                      low = "lightyellow", high = "#003f88", 
                      limits = c(0, 100))-> plot_1990

ggplot(data = internet_2000) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = percent_of_people_using_internet ), color = alpha('white', 0))+
  coord_fixed(1.3) +
  ggtitle('year 2000')+
  labs(x = NULL, y = NULL)+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),  # Set legend text color to white
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = 'white', hjust = 0.5))+
  scale_fill_gradient(name = "Percentage",
                      low = "lightyellow", high = "#003f88", 
                      limits = c(0, 100))-> plot_2000



ggplot(data = internet_2010) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = percent_of_people_using_internet ), color = alpha('white', 0))+
  coord_fixed(1.3)+
  ggtitle('year 2010')+
  labs(x = NULL, y = NULL)+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = 'white', hjust = 0.5))+
  scale_fill_gradient(name = "Percentage",
                      low = "lightyellow", high = "#003f88", 
                      limits = c(0, 100))-> plot_2010

ggplot(data = internet_2020) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = percent_of_people_using_internet ), color = alpha('white', 0))+
  coord_fixed(1.3)+
  ggtitle('year 2020')+
  labs(x = NULL, y = NULL)+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),  
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = 'white', hjust = 0.5))+
  scale_fill_gradient(name = "Percentage",
                      low = "lightyellow", high = "#003f88", 
                      limits = c(0, 100))->plot_2020

ggplotly(wykres)


combined_plots <- plot_1990 +plot_2000  + plot_2010 + plot_2020 +
  plot_layout(guides = "collect") +
  plot_annotation(title = "The percentage of people using the internet in each country over the years", theme = theme(plot.title = element_text(hjust = 0.5)))

combined_plots

ggplotly(plot_1990)

#wiem że mogłam zrobić wszystkie wykresy w pętli, ale już za daleko to zaszło więc zostawiam tak jest jest

?colorRamp

