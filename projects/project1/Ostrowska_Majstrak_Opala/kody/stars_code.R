library(ggplot2)
library(dplyr)
stars <- read.csv('yelp_academic_dataset_business_3.csv') %>% 
  filter(name == "McDonalds") %>% 
  select(stars)
  #group_by(stars) %>% 
  #summarise(value = n())


ggplot(stars, aes(x = '',y = stars))+
  geom_violin(fill = '#EEBA0B', color = 'white', size = 1.5)+
  coord_flip()+
  ylim(1,5)+
  theme(
    plot.background =element_rect(fill = "transparent", color = NA), 
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    text = element_text(color = 'white'),
    axis.line = element_line(color = 'white'),
    axis.text.x = element_text(color = 'white')
  )+
  labs(x = NULL)-> wykres1

ggsave("your_plot.png", plot = wykres1, bg = "transparent")
