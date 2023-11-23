library(tidyr)
library(dplyr)
install.packages("rworldmap")
library(rworldmap)
library(ggplot2)
library(maps)
library(mapdata)
mcdonald <- read.csv('McDonalds.csv')
menu_india <- read.csv("India_Menu.csv")
colnames(countries)[1] <- "country"

#vege options in India
produkty_veg <- menu_india[grep("veg", menu_india$Menu.Items, ignore.case = TRUE), ] %>% 
  select()

#cost of living datatable (mcdonalds, inexpensive restaurant)
cost <- read.csv("cost-of-living.csv")
cost <- t(cost)
colnames(cost) <- cost[1, ]  
cost <- cost[-1,]
cost <- as.data.frame(cost)
cost <- cost %>% 
  select(c(1,3),) 
colnames(cost) <- c('restaurant','mc')
cost <- cost %>% 
  mutate(proportion = as.numeric(mc)/as.numeric(restaurant)) %>% 
  select(3) %>% 
  arrange(proportion)

#group by countries
library(stringr)
cost[2] <- rownames(cost)
cost2 <- cost %>% 
  mutate(country=str_split(V2, pattern = "\\.\\.", simplify = TRUE)) %>% 
  mutate(country = ifelse(country[,3]=="", country[,2], country[,3])) %>%
  select(proportion,country) %>% 
  group_by(country) %>% 
  summarise(proportion = mean(proportion)) %>% 
  mutate(country=str_replace_all(country, "\\.", " ")) 
cost2[1,1]="Poland"

#map ''"The cost of a meal at McDonald's compared to an inexpensive restaurant"
map_data <- map_data("world") %>% 
  filter(region!='Antarctica')

map_data <- map_data %>% 
  mutate(region = trimws(tolower(region)))

cost2 <- cost2 %>% 
  mutate(region=trimws(tolower(country))) %>% 
  mutate(region = case_when(
    region == 'united states' ~ 'usa',
    region == 'united kingdom' ~ 'uk',
    TRUE ~ region ))
map_prop <- map_data %>% 
  left_join(cost2)

map <- map_data %>% 
  left_join(cost2) %>% 
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=proportion), color="white", size= 0)+
  scale_fill_gradient2(low="#69E100", mid='#FCFCD2', high = '#ff8d50', midpoint=1, na.value = "#e0e0e0")+
  theme_void()+
  labs(title = "The cost of a meal at McDonald's compared to an inexpensive restaurant")+
  theme( plot.title.position = "plot",plot.title = element_text(color = "white", size=15), legend.title = element_blank(), legend.text = element_text(color = "white")) 
ggsave('mapa.png', plot=map, bg = 'transparent')

#The most popular fast food restaurants (the most sales in 2021)
fastfood <- read.csv('FastFood.csv')
fastfood2 <- fastfood %>% 
  arrange(-fastfood[2]) %>% 
  head(10) %>% 
  select(c(1:2))
colnames(fastfood2) <- c("fast_food", "sprzedaz")
fastfood_plot <- fastfood2 %>% 
  mutate(fast_food = forcats::fct_reorder(fast_food, -sprzedaz))
fastfood_plot %>% 
  ggplot(mapping = aes(x=fast_food, y = sprzedaz))+
  geom_col(fill="goldenrod")+
  labs(title = "The most popular fast food restaurants",
       x = "", 
       y = "sales in 2021 (mln)")+
  scale_x_discrete(guide = guide_axis(angle=90))+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text = element_text(face = "bold", size=10),  
        axis.title = element_text(face = "bold"), 
        plot.title = element_text(face = "bold"))

#Total change in units of fast-food restaurants between 2020 and 2021
fastfood3 <- fastfood %>% 
  arrange(-fastfood[7]) %>% 
  head(10) %>% 
  select(c(1,7)) 
fastfood3 %>% 
  ggplot(mapping = aes(x=fastfood3[,1], y = fastfood3[,2]))+
  geom_col()+
  labs(title = "Fastest growing fast food restaurants in 2020",
       x = "fast-food", 
       y = "change of units 2020-2021")+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle=30))
