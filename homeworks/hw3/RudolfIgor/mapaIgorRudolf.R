
#used dataset https://stats.oecd.org/index.aspx?DataSetCode=ANHRS
work_dataset <- read.csv('dataset-time.csv')

library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(ggthemes)

dim(work_dataset)

#considering only data from 2022

work_dataset<- work_dataset %>% filter(TIME==2019) %>% 
  select(Country, Employment.status, Value)

#now beacuse we have two groups Dependent employment and
#Total employment, for each country let's take mean of them

work_dataset

work_dataset <- work_dataset %>%
  group_by(Country) %>%
  summarise(
    Total_mean = mean(Value[Employment.status == "Total employment"], na.rm = TRUE),
    Dependent_mean = mean(Value[Employment.status == "Dependent employment"], na.rm = TRUE),
    Final_value = ifelse(all(!is.na(Total_mean), !is.na(Dependent_mean)), (Total_mean + Dependent_mean) / 2, Value[1])
  ) %>%
  select(Country, Final_value)


work_dataset<- as.data.frame(work_dataset)

work_dataset

#now let's consider only european union countries

european_union_countries_ours<- c("Austria", "Belgium", "Czechia", "Denmark",
                              "Estonia", "Finland", "France", "Germany",
                              "Greece", "Hungary", "Ireland", "Italy",
                              "Latvia", "Lithuania", "Luxembourg",
                              "Netherlands", "Poland", "Portugal", "Slovak Republic",
                              "Slovenia", "Spain", "Sweden")

work_dataset<-work_dataset %>% filter(Country %in% european_union_countries_ours)

work_dataset<- work_dataset %>% mutate(Country = ifelse(Country == "Czechia", "Czech Republic", Country))

work_dataset<- work_dataset %>% mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))

needed_european_countries <- c("Bulgaria","Cyprus","Estonia","Finland","Greece","Ireland","Latvia",
                              "Lithuania","Luxembourg","Malta","Romania" ,"Sweden","Portugal", 
                              "Spain", "France", "Germany","Austria", "Belgium", 
                              "UK", "Netherlands", "Denmark", "Poland", "Italy", "Croatia", 
                              "Slovenia", "Hungary", "Slovakia", "Czech Republic")

work_dataset

european.union <- map_data("world", region = needed_european_countries)

#now let's add to european union needed fill 

all_europe_df<- data.frame(region= needed_european_countries)

all_europe_df<- all_europe_df %>% left_join(work_dataset, by = c("region"="Country"))

europan_union_all_data<- left_join(european.union,all_europe_df, by="region" )

#now drawing a map

europan_union_all_data <- europan_union_all_data %>%
  mutate(
    Category = cut(Final_value, breaks = c(1300, 1380, 1461, 1542, 1623, 1704, 1785), 
                   include.lowest = TRUE, labels = FALSE)
  )

europe_map<-ggplot(europan_union_all_data, aes(x = long, y = lat, group = group, fill = as.factor(Category))) +
  geom_polygon(color = "black") +coord_map("mercator")+
  scale_fill_manual(values = c("#d3ffd3", "#98FB98", "#32CD42", "#008006", "#006404", "#004505"), na.value = "grey50",
                    labels=c("1300-1380", "1381-1461", "1462-1542", "1543-1623", "1624-1704","1705-1785" )) +
  coord_fixed(1.3) +
  theme_map()+
  labs(fill="Average hours", subtitle = "actually worked per worker in the european union", 
       na.value="No data")+
  ggtitle("Map of the average annual hours in the year 2019")+
  theme(legend.position = c(-0.01, 0.67),
        plot.title = element_text(family = "Arial", color = "darkgreen"),
        plot.subtitle = element_text(face="bold"))

europe_map
