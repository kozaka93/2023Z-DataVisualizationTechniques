library(ggplot2)
library(tidyr)
library(dplyr)
library(extrafont)
#font_import()

### wczytanie danych
map_world <- map_data("world")
df_1 <- read.csv('dane/final.csv')


### przygotowanie danych

x <- df_1 %>% 
  separate_rows(Commodity_Description, sep= ", ") %>%  
  filter(Commodity_Description == "Coffee") %>% 
  filter(Attribute_Description == "Production") %>% 
  group_by(Country_Name, Year) %>% 
  summarise(produkcja_w_roku = sum(Value)) %>% 
  mutate(produkcja_w_roku = 60*produkcja_w_roku) %>% 
  group_by(Country_Name) %>% 
  summarise(srednia_produkcja = mean(produkcja_w_roku))

xx <- x

## ujednolicenie nazw państw
xx <- xx %>% 
  filter(Country_Name != "China, Peoples Republic of") %>% 
  filter(Country_Name != "Russian Federation") 

xx[xx$Country_Name == "Congo (Brazzaville)",1] <- "Republic of Congo"
xx[xx$Country_Name == "Congo (Kinshasa)",1] <- "Republic of Congo"
xx[xx$Country_Name == "Congo, Democratic Rep of the",1] <- "Democratic Republic of the Congo"

xx[xx$Country_Name == "Cote d'Ivoire",1] <- "Ivory Coast"
xx[xx$Country_Name == "Jamaica & Dep",1] <- "Jamaica"
xx[xx$Country_Name == "Jamaica and Dep",1] <- "Jamaica"

xx[xx$Country_Name == "Korea, Republic of",1] <- "North Korea"
xx[xx$Country_Name == "Korea, South",1] <- "South Korea"

xx[xx$Country_Name == "South Africa, Republic of",1] <- "South Africa"
xx[xx$Country_Name == "Tanzania, United Republic of",1] <- "Tanzania"

xx[xx$Country_Name == "United Kingdom",1] <- "UK"
xx[xx$Country_Name == "United States",1] <- "USA"

xx[xx$Country_Name == "Yemen (Sanaa)",1] <- "Yemen"
xx[xx$Country_Name == "EU-27",1] <- "European Union"

xx[xx$Country_Name == "European Union",1] <- "Austria, Belgium, Bulgaria, Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Romania, Slovakia, Slovenia, Spain, Sweden"
xx <- xx %>%
  separate_rows(Country_Name, sep= ", ")

xx <- xx %>% 
  group_by(Country_Name) %>% 
  summarise(srednia_produkcja = sum(srednia_produkcja))

write.csv(xx, "dane/meanProduction.csv", row.names=FALSE)


##

df <- read.csv("dane/meanProduction.csv",header=T) 


total<-left_join(map_world, df, by = c("region" = "Country_Name")) %>% 
  filter(region != "Antarctica")

x <- total %>% 
  group_by(region) %>% 
  count() %>% 
  full_join(df, by = c("region" = "Country_Name")) %>% 
  filter(is.na(srednia_produkcja)) %>% 
  filter(n <200)

tot <- total %>% 
  filter(!region %in% x$region) %>% 
  mutate(srednia_produkcja = ifelse(is.na(srednia_produkcja), 0, srednia_produkcja))

tot$region<-factor(tot$region)
tot <- tot[order(tot$order),]

### rysowanie wykresu

p<-ggplot(tot, aes(long, lat, group=group,
                   fill=cut(srednia_produkcja,
                            breaks = c(-1, 0, 1000, 20000, 60000, 120000, 400000, 1000000, 5000000, Inf),
                            labels = c("0", "0 - 1k","1k - 20k", "20k - 60k", "60k - 120k", "120k - 400k", "400k - 1m", "1m - 5m", "> 5m")))) +
  geom_polygon(color="grey") +
  scale_fill_discrete(
    type = c("#ffffff", "#fae6ca", "#edcda6", "#dcaf91", "#d19469", "#d97736", "#b05625", "#9e5035", "#683b2c"),
    na.value="white") +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(breaks = c(-23.27, 23.27), labels = c("the Tropic of Cancer", "the Tropic of Capricorn"))+
  labs(fill = "Coffee produced [t]", title = "Coffee Production") +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color = NA),
        plot.title = element_text(
          hjust = 0.5 ,vjust=0.5, size=90,family = "Bell MT"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
          size = 52,family = "Bell MT", lineheight = 0, color = "black", hjust = 0.5),
        axis.ticks = element_blank(),
        panel.border =  element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "black", linetype = "dotted", linewidth = 3),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        legend.background = element_blank(),
        legend.text = element_text(
          hjust = 0 ,vjust=0.5, size=52.5,family = "Bell MT"),
        legend.title = element_text(
          hjust = 0.4 ,vjust=1, size=60,family = "Bell MT"),
        legend.position = c(-0.11,0.8),
        legend.box.background = element_rect(colour = "black")
  )

p

### zapis do pliku

ggsave('coffeeProductionMap.png', p, bg='transparent', width = 38.1, height = 19.2)
