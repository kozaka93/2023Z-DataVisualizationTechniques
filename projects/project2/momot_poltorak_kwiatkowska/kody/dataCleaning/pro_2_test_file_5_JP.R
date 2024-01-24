library(dplyr)
library(htmltools)


# 1 trasa


maciek_1<-read.csv("D:\\r_pro_2\\R_files\\TOP5_Maciek_1.csv")
mydata_1_maciek<-data.frame(
  lat=maciek_1$lat,
  lon=maciek_1$lon,
  ele=maciek_1$speed,
  distance=maciek_1$distance,
  time=maciek_1$timeBetween,
  speed=maciek_1$speed
)

mydata_1_maciek %>% filter(speed<100) -> mydata_1_maciek
mydata_1_maciek %>% mutate(summarized_distance=0) -> mydata_1_maciek
mydata_1_maciek %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_1_maciek
mydata_1_maciek %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_1_maciek

mydata_1_maciek$speed<-ifelse(mydata_1_maciek$speed>40,40,round(mydata_1_maciek$speed))
mydata_1_maciek %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_1_maciek
mydata_1_maciek %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_1_maciek

coords_of_kilometers_1_maciek<-last_lon_lat_each_km_1_maciek %>% left_join(avg_speed_per_km_1_maciek,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_1_maciek$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_1_maciek$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_1_maciek

mydata_1_maciek %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_1_maciek

write.csv(mydata_1_maciek,file = "activity_Maciek_1.csv")

mydata_1_maciek$ele<-mydata_1_maciek$speed



# 2 Trasa


maciek_2<-read.csv("D:\\r_pro_2\\R_files\\TOP5_Maciek_2.csv")
mydata_2_maciek<-data.frame(
  lat=maciek_2$lat,
  lon=maciek_2$lon,
  ele=maciek_2$speed,
  distance=maciek_2$distance,
  time=maciek_2$timeBetween,
  speed=maciek_2$speed
)

mydata_2_maciek %>% filter(speed<100) -> mydata_2_maciek
mydata_2_maciek %>% mutate(summarized_distance=0) -> mydata_2_maciek
mydata_2_maciek %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_2_maciek
mydata_2_maciek %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_2_maciek

mydata_2_maciek$speed<-ifelse(mydata_2_maciek$speed>40,40,round(mydata_2_maciek$speed))
mydata_2_maciek %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_2_maciek
mydata_2_maciek %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_2_maciek

coords_of_kilometers_2_maciek<-last_lon_lat_each_km_2_maciek %>% left_join(avg_speed_per_km_2_maciek,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_2_maciek$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_2_maciek$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_2_maciek

mydata_2_maciek %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_2_maciek

write.csv(mydata_2_maciek,file = "activity_Maciek_2.csv")

mydata_2_maciek$ele<-mydata_2_maciek$speed


# 3 Trasa


maciek_3<-read.csv("D:\\r_pro_2\\R_files\\TOP5_Maciek_3.csv")
mydata_3_maciek<-data.frame(
  lat=maciek_3$lat,
  lon=maciek_3$lon,
  ele=maciek_3$speed,
  distance=maciek_3$distance,
  time=maciek_3$timeBetween,
  speed=maciek_3$speed
)

mydata_3_maciek %>% filter(speed<100) -> mydata_3_maciek
mydata_3_maciek %>% mutate(summarized_distance=0) -> mydata_3_maciek
mydata_3_maciek %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_3_maciek
mydata_3_maciek %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_3_maciek

mydata_3_maciek$speed<-ifelse(mydata_3_maciek$speed>40,40,round(mydata_3_maciek$speed))
mydata_3_maciek %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_3_maciek
mydata_3_maciek %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_3_maciek

coords_of_kilometers_3_maciek<-last_lon_lat_each_km_3_maciek %>% left_join(avg_speed_per_km_3_maciek,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_3_maciek$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_3_maciek$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_3_maciek

mydata_3_maciek %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_3_maciek

write.csv(mydata_3_maciek,file = "activity_Maciek_3.csv")

mydata_3_maciek$ele<-mydata_3_maciek$speed


# 4 Trasa


maciek_4<-read.csv("D:\\r_pro_2\\R_files\\TOP5_Maciek_4.csv")
mydata_4_maciek<-data.frame(
  lat=maciek_4$lat,
  lon=maciek_4$lon,
  ele=maciek_4$speed,
  distance=maciek_4$distance,
  time=maciek_4$timeBetween,
  speed=maciek_4$speed
)

mydata_4_maciek %>% filter(speed<100) -> mydata_4_maciek
mydata_4_maciek %>% mutate(summarized_distance=0) -> mydata_4_maciek
mydata_4_maciek %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_4_maciek
mydata_4_maciek %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_4_maciek

mydata_4_maciek$speed<-ifelse(mydata_4_maciek$speed>40,40,round(mydata_4_maciek$speed))
mydata_4_maciek %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_4_maciek
mydata_4_maciek %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_4_maciek

coords_of_kilometers_4_maciek<-last_lon_lat_each_km_4_maciek %>% left_join(avg_speed_per_km_4_maciek,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_4_maciek$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_4_maciek$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_4_maciek

mydata_4_maciek %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_4_maciek

write.csv(mydata_4_maciek,file = "activity_Maciek_4.csv")

mydata_4_maciek$ele<-mydata_4_maciek$speed


# 5 Trasa


maciek_5<-read.csv("D:\\r_pro_2\\R_files\\TOP5_Maciek_5.csv")
mydata_5_maciek<-data.frame(
  lat=maciek_5$lat,
  lon=maciek_5$lon,
  ele=maciek_5$speed,
  distance=maciek_5$distance,
  time=maciek_5$timeBetween,
  speed=maciek_5$speed
)

mydata_5_maciek %>% filter(speed<100) -> mydata_5_maciek
mydata_5_maciek %>% mutate(summarized_distance=0) -> mydata_5_maciek
mydata_5_maciek %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata_5_maciek
mydata_5_maciek %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata_5_maciek

mydata_5_maciek$speed<-ifelse(mydata_5_maciek$speed>40,40,round(mydata_5_maciek$speed))
mydata_5_maciek %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km_5_maciek
mydata_5_maciek %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km_5_maciek

coords_of_kilometers_5_maciek<-last_lon_lat_each_km_5_maciek %>% left_join(avg_speed_per_km_5_maciek,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers_5_maciek$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers_5_maciek$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels_5_maciek

mydata_5_maciek %>% mutate(speed_not_rounded=distance*3.6/time) -> mydata_5_maciek

write.csv(mydata_5_maciek,file = "activity_Maciek_5.csv")

mydata_5_maciek$ele<-mydata_5_maciek$speed

