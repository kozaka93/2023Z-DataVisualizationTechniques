library(XML)
library(geosphere)
setwd("D:\\r_pro_2\\R_files")
activities %>% arrange(-Distance) %>% filter(row_number() %in% c(1,3,6,36,8)) -> chosen_activities

# Aktywności:
# 1) activities/7759896918.gpx Najdłuższe
# 2) activities/5883404610.gpx Radom
# 3) activities/7350288688.gpx Warka 4,5 h
# 4) activities/7298422824.gpx Góra 2h 17 min
# 5) activities/5548459678.gpx Zegrze


# 1 trasa
gpx_1 <- htmlTreeParse(file="D:\\r_pro_2\\activities\\7759896918.gpx",useInternalNodes = TRUE)
coords_1 <- xpathSApply(doc = gpx_1, path = "//trkpt", fun = xmlAttrs)
ele_1 <- xpathSApply(doc = gpx_1, path = "//trkpt/ele", fun = xmlValue)

mydata_1 <- data.frame(
  lat = as.numeric(coords_1["lat", ]),
  lon = as.numeric(coords_1["lon", ]),
  ele = as.numeric(ele_1)
)

mydata_1 %>% mutate(distance=0) -> mydata_1
mydata_1 %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_1
mydata_1 %>% mutate(time=21526/21309) -> mydata_1
mydata_1 %>% mutate(speed=distance*3.6/time) -> mydata_1

mydata_1 %>% filter(speed<100) -> mydata_1
mydata_1 %>% mutate(summarized_distance=0) -> mydata_1
mydata_1 %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_1
mydata_1 %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_1

mydata_1$speed<-ifelse(mydata_1$speed>40,40,round(mydata_1$speed))
mydata_1 %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_1
mydata_1 %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_1

coords_of_kilometers_1<-last_lon_lat_each_km_1 %>% left_join(avg_speed_per_km_1,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_1$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_1$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_1

mydata_1 %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_1

write.csv(mydata_1,file = "activity_Kuba_1.csv")

mydata_1$ele<-mydata_1$speed

########2 trasa############

gpx_2 <- htmlTreeParse(file="D:\\r_pro_2\\activities\\5883404610.gpx",useInternalNodes = TRUE)
coords_2 <- xpathSApply(doc = gpx_2, path = "//trkpt", fun = xmlAttrs)
ele_2 <- xpathSApply(doc = gpx_2, path = "//trkpt/ele", fun = xmlValue)

mydata_2 <- data.frame(
  lat = as.numeric(coords_2["lat", ]),
  lon = as.numeric(coords_2["lon", ]),
  ele = as.numeric(ele_2)
)

mydata_2 %>% mutate(distance=0) -> mydata_2
mydata_2 %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_2
mydata_2 %>% mutate(time=20754/13859) -> mydata_2
mydata_2 %>% mutate(speed=distance*3.6/time) -> mydata_2

mydata_2 %>% filter(speed<100) -> mydata_2
mydata_2 %>% mutate(summarized_distance=0) -> mydata_2
mydata_2 %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_2
mydata_2 %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_2

mydata_2$speed<-ifelse(mydata_2$speed>40,40,round(mydata_2$speed))
mydata_2 %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_2
mydata_2 %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_2

coords_of_kilometers_2<-last_lon_lat_each_km_2 %>% left_join(avg_speed_per_km_2,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_2$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_2$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_2

mydata_2 %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_2

write.csv(mydata_2,file = "activity_Kuba_2.csv")

mydata_2$ele<-mydata_2$speed


######## 3 trasa #########

gpx_3 <- htmlTreeParse(file="D:\\r_pro_2\\activities\\7350288688.gpx",useInternalNodes = TRUE)
coords_3 <- xpathSApply(doc = gpx_3, path = "//trkpt", fun = xmlAttrs)
ele_3 <- xpathSApply(doc = gpx_3, path = "//trkpt/ele", fun = xmlValue)

mydata_3 <- data.frame(
  lat = as.numeric(coords_3["lat", ]),
  lon = as.numeric(coords_3["lon", ]),
  ele = as.numeric(ele_3)
)

mydata_3 %>% mutate(distance=0) -> mydata_3
mydata_3 %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_3
mydata_3 %>% mutate(time=15857/15341) -> mydata_3
mydata_3 %>% mutate(speed=distance*3.6/time) -> mydata_3

mydata_3 %>% filter(speed<100) -> mydata_3
mydata_3 %>% mutate(summarized_distance=0) -> mydata_3
mydata_3 %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_3
mydata_3 %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_3

mydata_3$speed<-ifelse(mydata_3$speed>40,40,round(mydata_3$speed))
mydata_3 %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_3
mydata_3 %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_3

coords_of_kilometers_3<-last_lon_lat_each_km_3 %>% left_join(avg_speed_per_km_3,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_3$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_3$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_3

mydata_3 %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_3

write.csv(mydata_3,file = "activity_Kuba_3.csv")

mydata_3$ele<-mydata_3$speed


########## Trasa nr 4 ########### 

gpx_4 <- htmlTreeParse(file="D:\\r_pro_2\\activities\\7298422824.gpx",useInternalNodes = TRUE)
coords_4 <- xpathSApply(doc = gpx_4, path = "//trkpt", fun = xmlAttrs)
ele_4 <- xpathSApply(doc = gpx_4, path = "//trkpt/ele", fun = xmlValue)

mydata_4 <- data.frame(
  lat = as.numeric(coords_4["lat", ]),
  lon = as.numeric(coords_4["lon", ]),
  ele = as.numeric(ele_4)
)

mydata_4 %>% mutate(distance=0) -> mydata_4
mydata_4 %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_4
mydata_4 %>% mutate(time=8253/7979) -> mydata_4
mydata_4 %>% mutate(speed=distance*3.6/time) -> mydata_4

mydata_4 %>% filter(speed<100) -> mydata_4
mydata_4 %>% mutate(summarized_distance=0) -> mydata_4
mydata_4 %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_4
mydata_4 %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_4

mydata_4$speed<-ifelse(mydata_4$speed>40,40,round(mydata_4$speed))
mydata_4 %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_4
mydata_4 %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_4

coords_of_kilometers_4<-last_lon_lat_each_km_4 %>% left_join(avg_speed_per_km_4,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_4$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_4$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_4

mydata_4 %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_4

write.csv(mydata_4,file = "activity_Kuba_4.csv")

mydata_4$ele<-mydata_4$speed



#### Trasa 5 ######

gpx_5 <- htmlTreeParse(file="D:\\r_pro_2\\activities\\5548459678.gpx",useInternalNodes = TRUE)
coords_5 <- xpathSApply(doc = gpx_5, path = "//trkpt", fun = xmlAttrs)
ele_5 <- xpathSApply(doc = gpx_5, path = "//trkpt/ele", fun = xmlValue)

mydata_5 <- data.frame(
  lat = as.numeric(coords_5["lat", ]),
  lon = as.numeric(coords_5["lon", ]),
  ele = as.numeric(ele_5)
)

mydata_5 %>% mutate(distance=0) -> mydata_5
mydata_5 %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_5
mydata_5 %>% mutate(time=19714/13454) -> mydata_5
mydata_5 %>% mutate(speed=distance*3.6/time) -> mydata_5

mydata_5 %>% filter(speed<100) -> mydata_5
mydata_5 %>% mutate(summarized_distance=0) -> mydata_5
mydata_5 %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_5
mydata_5 %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_5

mydata_5$speed<-ifelse(mydata_5$speed>40,40,round(mydata_5$speed))
mydata_5 %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_5
mydata_5 %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_5

coords_of_kilometers_5<-last_lon_lat_each_km_5 %>% left_join(avg_speed_per_km_5,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_5$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_5$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_5

mydata_5 %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_5

write.csv(mydata_5,file = "activity_Kuba_5.csv")

mydata_5$ele<-mydata_5$speed

21309/21526 26801

13859/20754 24417

15341/15857 18371

7979/8253 9083

13454/19714 31262