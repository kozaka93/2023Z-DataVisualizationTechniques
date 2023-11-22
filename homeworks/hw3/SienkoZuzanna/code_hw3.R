library(ggplot2)
library(mapdata)
library(dplyr)

df<-read.csv("hate_crime.csv") %>% 
  filter(!STATE_NAME %in% c("Guam","District of Columbia","Federal"))

states <- map_data("state")

crimes_count_by_state<-df %>% 
  group_by(STATE_NAME) %>% 
  summarise(crime_count=n())

df_groupped$STATE_NAME <- tolower(df_groupped$STATE_NAME)

df_crimes_by_state<-states %>% 
  left_join(df_groupped,by = c("region" = "STATE_NAME"))


states_map<-states %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color="black")+
  coord_fixed(1.3)+
  theme_minimal()+
  theme(legend.position = "None")

map<-states_map+
  geom_polygon(data=df_crimes_by_state,aes(x = long, y = lat, fill = crime_count, group = group), color = "white")+
  geom_polygon(color = "black", fill = NA) +
  scale_fill_fermenter(palette = 9, trans="log10", direction = 1)+
  labs(title="   USA Crime Rates",  
       subtitle="   by total crime count.",
       fill="Crime count")+
  theme_void()+
  theme(legend.background = element_rect(color = "steelblue", linetype = "solid"),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(face = "bold"),
        legend.margin = margin(r = 30))
