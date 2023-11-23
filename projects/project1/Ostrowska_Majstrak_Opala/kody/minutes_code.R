library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)


Bigmac_Prices <- read.csv('big-mac-adjusted-index.csv') %>% 
  #mutate(name = sub("United Arab Emirates", "UAE", name, fixed = TRUE)) %>% 
  mutate(year = substr(date,1,4)) %>% 
  filter(year == 2022) 
  

salaries <- read.csv('salary_data.csv') 

df <- left_join(Bigmac_Prices, salaries, by = c("name"="country_name")) %>% 
  select(name, continent_name, average_salary, dollar_price) %>%
  mutate(salary_per_minute = average_salary / 10080) %>% 
  mutate(how_many_minutes = dollar_price / salary_per_minute) %>% 
  filter(how_many_minutes != is.na(how_many_minutes)) %>% 
  group_by(name) %>% 
  summarise(mean_minutes = mean(how_many_minutes)) %>%     
  filter(name %in% c('Argentina','Brazil', 'Costa Rica', 'Czech Republic', 'Egypt', 'France', 'India', 'Lebanon', 'Nicaragua', 'Oman', 'Philippines', 'Poland', 'Russia', 'SriLanka', 'Switzlerdland', 'Ukraine', 'United States', 'Vietnam')) %>% 
  arrange(desc(mean_minutes)) %>% 
  mutate(mean_minutes = round(mean_minutes, 1)) %>% 
  mutate(name = fct_reorder(name, desc(mean_minutes)))




wykres <- ggplot(df, aes(x= fct_rev(name), y = mean_minutes))+
  geom_bar(stat = "identity", fill = '#EEBA0B')+
  coord_flip()+
  geom_text(aes(label = mean_minutes), hjust = -0.1, color = 'white', position = position_dodge(width = 0.9))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'white'),
        axis.text.y = element_text(color = 'white'),
        axis.line = element_line(color = 'white'),
        plot.background =element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        text = element_text(color = 'white')) +
  labs(x= NULL, y = NULL)
  


ggsave("minutes_plot.png", plot = wykres, bg = "transparent", width = 10.5, height = 5)


  

wykres

