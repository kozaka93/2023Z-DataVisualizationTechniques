library(tidyverse)
library(sf)
library(rnaturalearth)
options(scipen = 999)   


total_alcohol_consumption_per_capita_litres_of_pure_alcohol <- read_csv("./data/total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv", 
                                                                             col_types = cols(Code = col_skip()))

alcohol_consumption_vs_gdp_per_capita <- read_csv("./data/alcohol-consumption-vs-gdp-per-capita.csv", 
                                                       col_types = cols(Code = col_skip(), Continent = col_skip()))

alcohol_consumption_vs_gdp_per_capita <- alcohol_consumption_vs_gdp_per_capita %>% na.omit()
 

males_vs_females_who_drank_alcohol_in_last_year <- read_csv("./data/males-vs-females-who-drank-alcohol-in-last-year.csv", 
                                                                 col_types = cols(Code = col_skip(), `Population (historical estimates)` = col_skip(), 
                                                                                            Continent = col_skip()))
males_vs_females_who_drank_alcohol_in_last_year<-males_vs_females_who_drank_alcohol_in_last_year %>% na.omit()

## Points

colors <- RColorBrewer::brewer.pal(n = 9, name = "YlOrBr")

df <- alcohol_consumption_vs_gdp_per_capita %>% filter(Year==2018) %>%
   select(Entity,`GDP per capita, PPP (constant 2017 international $)`,`Population (historical estimates)`)
df <- inner_join(males_vs_females_who_drank_alcohol_in_last_year,df)

p1 <- df %>% ggplot(aes(y=`Indicator:Alcohol, consumers past 12 months (%) - Sex:Female`,x=`Indicator:Alcohol, consumers past 12 months (%) - Sex:Male`,color=`GDP per capita, PPP (constant 2017 international $)`))+
  geom_point(alpha=0.8,size=6)+
  geom_abline(intercept = 0,slope=1,lty=5,col="darkgrey")+
  theme_minimal()+
  labs(
       x="Male",y="Female",
       color = "GDP")+
  theme(plot.title = element_text(hjust = 0.5, size =13,color="#696969"),
        legend.position = c(0.92,0.25),
        axis.title.x = element_text(size=15,color="#696969"),
        axis.title.y = element_text(size=15,color="#696969"),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        legend.title = element_text(color="#696969",size=13),
        legend.text = element_text(color="#696969",size=12),
        legend.key.height = unit(1,"cm")
        )+
  scale_x_continuous(breaks=seq(0,100,25), labels = paste(c(0,25,50,75,100),"%",sep=""),limits = c(0,100))+
  scale_y_continuous(breaks=seq(0,100,25), labels = paste(c(0,25,50,75,100),"%",sep=""),limits = c(0,100))+
  scale_color_gradientn(colours = colors,trans="sqrt",breaks=seq(10000,100000,20000),labels = paste(seq(10,100,20),"k $",sep=""))
  ggsave("./charts/points.png", p1, width = 6, height = 6, dpi = 300)


## Map

df <- total_alcohol_consumption_per_capita_litres_of_pure_alcohol
df <- df %>% filter(Year==2015) %>% 
  select(Entity,`Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`)%>%
  rename(Total="Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)")
world <- ne_countries(returnclass = "sf")

ws <- world$admin
en <- df$Entity
ws[!(ws %in% en)]
en[191] <- "United States of America"  
en[46] <- "Democratic Republic of the Congo" 
en[178]<-"United Republic of Tanzania"
en[41]<-"Ivory Coast"
en[158] <- "Republic of Serbia" 
en[39] <- "Republic of the Congo"

df$Entity<-en

world_data <- left_join(world,df,by=join_by(admin==Entity))
world_data <- world_data %>% filter(!admin=="Antarctica")

colors <- RColorBrewer::brewer.pal(n = 9, name = "YlOrBr")

p2 <- ggplot(world_data) +
  geom_sf(aes(fill = Total))+
  scale_fill_gradientn(colours = colors , breaks = seq(1,15,2))+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size =15,color="#696969"),
        legend.direction = "horizontal",
        legend.position = c(0.5,-0.04),
        legend.key.width = unit(2.5,"cm"),
        legend.key.height = unit(0.4,"cm"),
        legend.text = element_text(size=10,color="#696969"))
ggsave("./charts/map.png", p2, width = 7, height = 5, dpi = 300)

  







