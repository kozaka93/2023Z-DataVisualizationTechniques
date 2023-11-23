library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(maps)
library(patchwork)
options(scipen = 12)

df <- read.csv("Production_Crops_Livestock_E_All_Data_NOFLAG.csv")
str(df)

df %>% 
  filter((Area.Code < 300) & 
           stri_detect_regex(Item, "Palm oil")) %>% 
  group_by(Area) %>% 
  summarise(sum_2020 = Y2020/1000000) %>% 
  filter(sum_2020!=0)-> df3_1

df %>% 
  filter((Area.Code < 300) & 
           stri_detect_regex(Item, "Palm oil")) %>% 
  group_by(Area) %>% 
  summarise(sum_2020 = Y2020/1000000) %>% 
  filter(sum_2020>0) %>% 
  top_frac(0.25,sum_2020)-> df3


#########

mapa<- map_data("world")

map2 <- merge.data.frame(mapa,df3, by.x = "region", by.y = "Area", all.x = T)

subset(df3, !(Area %in% map2$region)) -> excluded

df3 %>% mutate(
  Area = case_when(
    stri_detect_regex(Area, "China") ~ "China",
    stri_detect_regex(Area, "Russia") ~ "Russia",
    Area == "Congo" ~ "Republic of Congo",
    stri_detect_regex(Area, "Venezuela") ~ "Venezuela",
    stri_detect_regex(Area, "Tanzania") ~ "Tanzania",
    stri_detect_regex(Area, "e d'Ivoire") ~ "Ivory Coast",
    TRUE ~ Area
  )
) -> df_final

map2 <- merge.data.frame(mapa,df_final, by.x = "region", by.y = "Area", all.x = T)
subset(df_final, !(Area %in% map2$region)) -> excluded

map2 %>% 
  filter( ! lat < -60) %>% arrange(order)-> map2

write.csv(map2, "map2.csv")#save data for further plot drawing

###########
map2 <- read.csv("map2.csv")

tre1 = 1
tre2 = 10
bh = 5

map2 %>% 
  ggplot( aes(long, lat, group = group, fill = sum_2020, alpha = "No data or\nlow production")) +
  geom_polygon(
    color = "black",
    linewidth = 0.1
  ) +
  scale_fill_fermenter(palette = "YlOrRd",
                       trans = "log10",
                       direction = 1,
                       na.value = "#DDDDDD")+
  coord_fixed(1.3) +
  theme(
    text = element_text(colour = "white", size = 12),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "transparent", color =NA),
    panel.background = element_rect(fill = "transparent", color =NA),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(color = "black", linewidth = 0.2),
    legend.margin = margin(b=-0.2,t=0,r=0,l=0,unit = "cm")
  ) + 
  labs(
    fill = "Production\n(million tonnes)\nlog scale",
    alpha = "No data or\nlog production")+
  scale_alpha_manual("", values = 1)+
  guides(
    fill = guide_colorsteps(order =1, 
                            frame.colour = "black"),
    alpha = guide_legend(order = 2, 
                         override.aes = list(fill = "#DDDDDD")))-> m1

m1

c(sum(df3$sum_2020),sum(df3_1$sum_2020,na.rm = T)) -> sumTotal

c(df3$sum_2020[df3$Area %in% c("Indonesia", "Malaysia")]/sumTotal[2])
# sum for considered countries = 73.09732 M tonnes
# total 75.87555 75.87555

ggsave("map_trasparent3.png", plot = m1, bg = "transparent", width = 10, height = 6)
