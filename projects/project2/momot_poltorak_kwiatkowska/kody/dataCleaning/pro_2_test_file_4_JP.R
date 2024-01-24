library(XML)
library(dplyr)
library(geosphere)

activities_Ola<-read.csv("D:\\r_pro_2\\R_files\\activities_Ola.csv")

# Aktywności
# 1) TOP5_Ola_1.gpx  2023-09-02 12:44:32 czas: 8166 sek
# 2) TOP5_Ola_2.gpx 2023-08-13 10:12:01 czas: 9292 sek
# 3) TOP5_Ola_3.gpx 2023-06-08 10:24:21 czas: 4618 sek
# 4) TOP5_Ola_4.gpx 2023-07-23 11:22:28 czas: 6175 sek 
# 5) TOP5_Ola_5.gpx  2023-06-11 11:06:00 czas: 6942 sek


# 1 trasa

gpx_1_ola <- htmlTreeParse(file="D:\\r_pro_2\\activities\\TOP5_Ola_1.gpx",useInternalNodes = TRUE)
coords_1_ola <- xpathSApply(doc = gpx_1_ola, path = "//trkpt", fun = xmlAttrs)
ele_1_ola <- xpathSApply(doc = gpx_1_ola, path = "//trkpt/ele", fun = xmlValue)

mydata_1_ola <- data.frame(
  lat = as.numeric(coords_1_ola["lat", ]),
  lon = as.numeric(coords_1_ola["lon", ]),
  ele = as.numeric(ele_1_ola)
)
1163
mydata_1_ola %>% mutate(distance=0) -> mydata_1_ola
mydata_1_ola %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_1_ola
mydata_1_ola %>% mutate(time=8166/1163) -> mydata_1_ola
mydata_1_ola %>% mutate(speed=distance*3.6/time) -> mydata_1_ola

mydata_1_ola %>% filter(speed<100) -> mydata_1_ola
mydata_1_ola %>% mutate(summarized_distance=0) -> mydata_1_ola
mydata_1_ola %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_1_ola
mydata_1_ola %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_1_ola

mydata_1_ola$speed<-ifelse(mydata_1_ola$speed>40,40,round(mydata_1_ola$speed))
mydata_1_ola %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_1_ola
mydata_1_ola %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_1_ola

coords_of_kilometers_1_ola<-last_lon_lat_each_km_1_ola %>% left_join(avg_speed_per_km_1_ola,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_1_ola$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_1_ola$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_1_ola

mydata_1_ola %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_1_ola

write.csv(mydata_1_ola,file = "activity_Ola_1.csv")

mydata_1_ola$ele<-mydata_1_ola$speed


### 2 trasa ###
1278

gpx_2_ola <- htmlTreeParse(file="D:\\r_pro_2\\activities\\TOP5_Ola_2.gpx",useInternalNodes = TRUE)
coords_2_ola <- xpathSApply(doc = gpx_2_ola, path = "//trkpt", fun = xmlAttrs)
ele_2_ola <- xpathSApply(doc = gpx_2_ola, path = "//trkpt/ele", fun = xmlValue)

mydata_2_ola <- data.frame(
  lat = as.numeric(coords_2_ola["lat", ]),
  lon = as.numeric(coords_2_ola["lon", ]),
  ele = as.numeric(ele_2_ola)
)

mydata_2_ola %>% mutate(distance=0) -> mydata_2_ola
mydata_2_ola %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_2_ola
mydata_2_ola %>% mutate(time=9292/1278) -> mydata_2_ola
mydata_2_ola %>% mutate(speed=distance*3.6/time) -> mydata_2_ola

mydata_2_ola %>% filter(speed<100) -> mydata_2_ola
mydata_2_ola %>% mutate(summarized_distance=0) -> mydata_2_ola
mydata_2_ola %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_2_ola
mydata_2_ola %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_2_ola


mydata_2_ola$speed<-ifelse(mydata_2_ola$speed>40,40,round(mydata_2_ola$speed))
mydata_2_ola %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_2_ola
mydata_2_ola %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_2_ola

coords_of_kilometers_2_ola<-last_lon_lat_each_km_2_ola %>% left_join(avg_speed_per_km_2_ola,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_2_ola$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_2_ola$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_2_ola

mydata_2_ola %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_2_ola

write.csv(mydata_2_ola,file = "activity_Ola_2.csv")

mydata_2_ola$ele<-mydata_2_ola$speed


### 3 Trasa ###
4618/722

gpx_3_ola <- htmlTreeParse(file="D:\\r_pro_2\\activities\\TOP5_Ola_3.gpx",useInternalNodes = TRUE)
coords_3_ola <- xpathSApply(doc = gpx_3_ola, path = "//trkpt", fun = xmlAttrs)
ele_3_ola <- xpathSApply(doc = gpx_3_ola, path = "//trkpt/ele", fun = xmlValue)

mydata_3_ola <- data.frame(
  lat = as.numeric(coords_3_ola["lat", ]),
  lon = as.numeric(coords_3_ola["lon", ]),
  ele = as.numeric(ele_3_ola)
)


mydata_3_ola %>% mutate(distance=0) -> mydata_3_ola
mydata_3_ola %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_3_ola
mydata_3_ola %>% mutate(time=4618/722) -> mydata_3_ola
mydata_3_ola %>% mutate(speed=distance*3.6/time) -> mydata_3_ola

mydata_3_ola %>% filter(speed<100) -> mydata_3_ola
mydata_3_ola %>% mutate(summarized_distance=0) -> mydata_3_ola
mydata_3_ola %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_3_ola
mydata_3_ola %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_3_ola


mydata_3_ola$speed<-ifelse(mydata_3_ola$speed>40,40,round(mydata_3_ola$speed))
mydata_3_ola %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_3_ola
mydata_3_ola %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_3_ola

coords_of_kilometers_3_ola<-last_lon_lat_each_km_3_ola %>% left_join(avg_speed_per_km_3_ola,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_3_ola$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_3_ola$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_3_ola

mydata_3_ola %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_3_ola

write.csv(mydata_3_ola,file = "activity_Ola_3.csv")

mydata_3_ola$ele<-mydata_3_ola$speed




# 4 trasa 
953
gpx_4_ola <- htmlTreeParse(file="D:\\r_pro_2\\activities\\TOP5_Ola_4.gpx",useInternalNodes = TRUE)
coords_4_ola <- xpathSApply(doc = gpx_4_ola, path = "//trkpt", fun = xmlAttrs)
ele_4_ola <- xpathSApply(doc = gpx_4_ola, path = "//trkpt/ele", fun = xmlValue)

mydata_4_ola <- data.frame(
  lat = as.numeric(coords_4_ola["lat", ]),
  lon = as.numeric(coords_4_ola["lon", ]),
  ele = as.numeric(ele_4_ola)
)


mydata_4_ola %>% mutate(distance=0) -> mydata_4_ola
mydata_4_ola %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_4_ola
mydata_4_ola %>% mutate(time=6175/953) -> mydata_4_ola
mydata_4_ola %>% mutate(speed=distance*3.6/time) -> mydata_4_ola

mydata_4_ola %>% filter(speed<100) -> mydata_4_ola
mydata_4_ola %>% mutate(summarized_distance=0) -> mydata_4_ola
mydata_4_ola %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_4_ola
mydata_4_ola %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_4_ola


mydata_4_ola$speed<-ifelse(mydata_4_ola$speed>40,40,round(mydata_4_ola$speed))
mydata_4_ola %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_4_ola
mydata_4_ola %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_4_ola

coords_of_kilometers_4_ola<-last_lon_lat_each_km_4_ola %>% left_join(avg_speed_per_km_4_ola,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_4_ola$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_4_ola$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_4_ola

mydata_4_ola %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_4_ola

write.csv(mydata_4_ola,file = "activity_Ola_4.csv")

mydata_4_ola$ele<-mydata_4_ola$speed


# 5 trasa
6942/1119

gpx_5_ola <- htmlTreeParse(file="D:\\r_pro_2\\activities\\TOP5_Ola_5.gpx",useInternalNodes = TRUE)
coords_5_ola <- xpathSApply(doc = gpx_5_ola, path = "//trkpt", fun = xmlAttrs)
ele_5_ola <- xpathSApply(doc = gpx_5_ola, path = "//trkpt/ele", fun = xmlValue)

mydata_5_ola <- data.frame(
  lat = as.numeric(coords_5_ola["lat", ]),
  lon = as.numeric(coords_5_ola["lon", ]),
  ele = as.numeric(ele_5_ola)
)


mydata_5_ola %>% mutate(distance=0) -> mydata_5_ola
mydata_5_ola %>% mutate(distance=ifelse(row_number()!=1,distHaversine(cbind(lag(lon),lag(lat)),cbind(lon,lat)),0)) -> mydata_5_ola
mydata_5_ola %>% mutate(time=6942/1119) -> mydata_5_ola
mydata_5_ola %>% mutate(speed=distance*3.6/time) -> mydata_5_ola

mydata_5_ola %>% filter(speed<100) -> mydata_5_ola
mydata_5_ola %>% mutate(summarized_distance=0) -> mydata_5_ola
mydata_5_ola %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_5_ola
mydata_5_ola %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_5_ola


mydata_5_ola$speed<-ifelse(mydata_5_ola$speed>40,40,round(mydata_5_ola$speed))
mydata_5_ola %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_5_ola
mydata_5_ola %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_5_ola

coords_of_kilometers_5_ola<-last_lon_lat_each_km_5_ola %>% left_join(avg_speed_per_km_5_ola,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_5_ola$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_5_ola$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_5_ola

mydata_5_ola %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_5_ola

write.csv(mydata_5_ola,file = "activity_Ola_5.csv")

mydata_5_ola$ele<-mydata_5_ola$speed
