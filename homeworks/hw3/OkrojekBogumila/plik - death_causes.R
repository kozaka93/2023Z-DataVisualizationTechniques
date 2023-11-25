library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(tidyr)


df_5 <- read.csv('20220327 annual-number-of-deaths-by-cause.csv')
df_6 <- read.csv('world_population.csv')


y <- df_5 %>% 
  filter(Year == 2015) %>% 
  select(c(1,14)) %>%  # 14 - road injuries 
  filter(!is.na(Deaths...Road.injuries...Sex..Both...Age..All.Ages..Number.))

a <- df_6 %>% 
  select(c(3,5, 8))

####### poprawa nazw

y[y$Entity == "Democratic Republic of Congo",1] <- "Democratic Republic of the Congo"
y[y$Entity == "Congo",1] <- "Republic of Congo"
y[y$Entity == "Cote d'Ivoire",1] <- "Ivory Coast"
y[y$Entity == "Czechia",1] <- "Czech Republic"
y[y$Entity == "Micronesia (country)",1] <- "Micronesia"
y[y$Entity == "Timor",1] <- "Timor-Leste"
y[y$Entity == "United States",1] <- "USA"
y[y$Entity == "United Kingdom",1] <- "UK"

a[a$Country.Territory == "DR Congo",1] <- "Democratic Republic of the Congo"
a[a$Country.Territory == "Republic of the Congo",1] <- "Republic of Congo"
a[a$Country.Territory == "United States",1] <- "USA"
a[a$Country.Territory == "United Kingdom",1] <- "UK"

#######

b <- full_join(y, a, by = c("Entity" = "Country.Territory")) 
c <- b %>% 
  mutate(death_by_x_per_1000 = 1000*Deaths...Road.injuries...Sex..Both...Age..All.Ages..Number./X2015.Population)
  
w <- map_data("world") %>% filter(long <= 180)

z <- full_join(w,c, by = c("region" ="Entity"))

z[z$region == "French Guiana",10] <- z[z$region == "France",10][1]

###### generowanie mapy

ggplot() + 
  geom_polygon(data = z, color="white", aes(x = long, y = lat, group = group, fill =cut(death_by_x_per_1000,
                            breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                            labels = c("0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7")))) +
  coord_map("mollweide") +
  scale_fill_discrete(type = c("#cde7f4", "#92d0f0", "#46abdd", "#008ed7", "#065bb7", "#01337e", "#020255"), na.value="lightgrey") +
  labs(title = "Deaths caused by road injuries", fill = "Road injury crude death rate per 1000 population",
       subtitle = "in 2015")+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border =  element_blank(),
        panel.grid.major = element_line(colour = "grey", linewidth = 0.3, linetype = 3),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(vjust = 0.5, hjust = 0.5, size=40),
        plot.subtitle = element_text(vjust = 0.5, hjust = 0.5, size=25),
        legend.position = "bottom",
        legend.text = element_text(
          hjust = 0 ,vjust=0.5, size=15),
        legend.title = element_text(
          hjust = 0.4 ,vjust=1, size=20),
  ) 

