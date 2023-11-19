kraje <- c("Poland", "Italy", "Romania", "France", "Germany", "Hungary", "Spain", "Portugal", "Greece", "Czech Republic", "Austria", "Netherlands",
            "Belgium","UK", "Bulgaria", "Croatia", "Latvia", "Slovenia", "Slovakia", "Sweden", "Lithuania", "Denmark", "Cyprus",
            "Estonia", "Ireland", "Luxembourg", "Malta", "Finland")
orchard_area <- c(160843.80, 55809.89, 55050, 38298, 28260,25044, 23222, 11306, 9377, 7818, 7674, 6949, 5788,5743, 4957, 4459, 3191, 2355, 2321, 1655, 1511, 1254, 376, NA, NA, NA, NA, NA
)# dane w ha
arable_land <- c(11054, 6914, 8966, 18210, 11713, 4309, 12023,892, 1815, 2490, 1325, 1011,
                 863, 6092, 3462, 822, 1318,174, 1349,2540,2209, 2394, 96, 685, 441, 62, 9,2244) #dane w tys. ha
data.frame(kraje, orchard_area, arable_land) -> rama #nie umiem obsługiwać excela więc przepisałem ręcznie
w1 <- map_data("world")
europe <- w1 %>% filter(region %in% kraje)
europe <- left_join(europe, rama, by=c("region"="kraje"))
europe <- europe %>% mutate(orchard_percentage=orchard_area/(1000*arable_land))
europe %>% ggplot()+geom_polygon(aes(x=long, y=lat, group=group, fill=orchard_percentage))+scale_fill_distiller(trans="log10", direction=1, palette=5)+
  labs(title="Udział sadów w powierzchni użytków rolnych w krajach Unii Europejskiej", subtitle = "2017r.", fill="Udział")+
  theme_void()
