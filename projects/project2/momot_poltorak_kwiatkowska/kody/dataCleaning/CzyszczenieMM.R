library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(leaflet)
library(geosphere)
library(scales)
library(plotly)
library(lubridate)
library(viridis)
library(hrbrthemes)
library(ggridges)
  ############################################################################################################

#sport per minute merged data

############################################################################################################
setwd("C:/Users/macie/Desktop/STUDIA/SEMESTR3/Techniki Wizualizacji Danych/PROJEKTY/Project2")
# Odczyt pliku JSON
ścieżka_do_pliku_kroki_calorie_na_minute <- "data/HUAWEI_HEALTH_20231211144413 (1)/Sport per minute merged data & description/sport per minute merged data.json"

dane_json_kroki_calorie_na_minute <- fromJSON(ścieżka_do_pliku_kroki_calorie_na_minute)

dane_json_kroki_calorie_na_minute <- dane_json_kroki_calorie_na_minute  %>%  select(-timeZone, -version)
# Przetworzenie ramki danych
kroki_calorie_na_minute <- dane_json_kroki_calorie_na_minute %>%
  # Rozwijanie kolumny sportDataUserData
  unnest_wider(sportDataUserData) %>%
  # Rozwijanie kolumny sportBasicInfos
  unnest_longer(sportBasicInfos)

# Sortowanie danych
kroki_calorie_na_minute <- kroki_calorie_na_minute %>%
  arrange(recordDay)
View(kroki_calorie_na_minute)
kroki_calorie_na_minute <- kroki_calorie_na_minute %>% select(-c(dataId, appType, sportType, sportDataSource,timeZone, deviceCode, version))
# Wyświetlenie przetworzonych danych
View(kroki_calorie_na_minute)

cleanList <- function(lst) {
  lapply(lst, function(x) x[1])
}

# Zastosowanie funkcji do odpowiednich kolumn
kroki_calorie_na_minute$startTime <- cleanList(kroki_calorie_na_minute$startTime)
kroki_calorie_na_minute$endTime <- cleanList(kroki_calorie_na_minute$endTime)

kroki_calorie_na_minute$startTime <- as.POSIXct(as.numeric(kroki_calorie_na_minute$startTime) / 1000, origin = "1970-01-01", tz = "UTC")
kroki_calorie_na_minute$endTime <- as.POSIXct(as.numeric(kroki_calorie_na_minute$endTime) / 1000, origin = "1970-01-01", tz = "UTC")
kroki_calorie_na_minute$recordDay <- format(kroki_calorie_na_minute$startTime, format = "%Y-%m-%d")
x <- kroki_calorie_na_minute %>% group_by(recordDay) %>% 
  summarise(n = sum(sportBasicInfos$steps)) %>% arrange(-n)
view(x)
y <- kroki_calorie_na_minute %>% group_by(recordDay) %>% 
  summarise(n = sum(sportBasicInfos$calorie)) %>% arrange(-n)
View(y)
Kroki_Calorie <- x %>% inner_join(y, by="recordDay")
colnames(Kroki_Calorie)[2] <- "Kroki"
colnames(Kroki_Calorie)[3] <- "Kalorie"
Kroki_Calorie$Kalorie <- Kroki_Calorie$Kalorie/1000
write.csv(Kroki_Calorie, "Kroki_Kalorie_Maciek.csv", row.names = FALSE)
#indywidualne statystyki wykres 1
Kroki_Calorie <- read.csv("Kroki_Kalorie_Maciek.csv")
wykres_gestosci_Kroki_Calorie <- Kroki_Calorie  %>% ggplot(aes(x=Kroki,y=Kalorie)) + stat_density2d(geom="tile", aes(fill = after_stat(density)), contour = FALSE) + 
  geom_point(colour = "white") + theme(legend.position = "none")
#to narazie slabe
wykres_kroki_w_danym_miesiacu <- Kroki_Calorie %>% mutate(miesiac = factor(month(recordDay)))%>% filter(Kroki <= 25000) %>% 
  ggplot(aes(x=miesiac,y=Kroki,fill=miesiac)) + geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size=0.4, alpha = 0.9) +
  theme_ipsum() + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")
#to narazie slabe
wykres_kroki_w_danym_miesiacu <- Kroki_Calorie %>% mutate(miesiac = factor(month(recordDay))) %>% ggplot(aes(x=Kroki,y=miesiac)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis()
#wykres indywidualne 2
wykres_gestosci_krokow <- Kroki_Calorie %>% 
                          mutate(miesiac = factor(month(recordDay))) %>% 
                          group_by(miesiac) %>% summarise(n = sum(Kroki)) %>%  
                          plot_ly(x=~miesiac,y=~n,type="bar") %>% 
                          layout(yaxis = list(title = "Score",tickformat = ".d"))
                          

############################################################################################################

#health detail data

############################################################################################################


ścieżka_do_pliku_health_dane <- "data/HUAWEI_HEALTH_20231211144413 (1)/Health detail data & description/health detail data.json"
dane_json_health_dane <- fromJSON(ścieżka_do_pliku_health_dane)
health_dane <- dane_json_health_dane %>% unnest_longer(samplePoints)
health_dane <- health_dane %>% 
  mutate(across(starts_with("samplePoints"), list(value = ~.$value), .names = "{col}_value"))
health_dane$value <- health_dane$samplePoints_value
health_dane <- health_dane %>% filter(samplePoints$key == "DATA_POINT_DYNAMIC_HEARTRATE")
health_dane <- select(health_dane, -starts_with("samplePoints"))
health_dane$startTime <- as.POSIXct(health_dane$startTime / 1000, origin = "1970-01-01", tz = "UTC")
health_dane$endTime <- as.POSIXct(health_dane$endTime / 1000, origin = "1970-01-01", tz = "UTC")
health_dane$recordDay <- format(health_dane$startTime, "%Y:%m:%d")
health_dane$recordDay <- as.POSIXct(health_dane$recordDay)
health_dane$value <- as.numeric(health_dane$value)
View(health_dane)
cos <- health_dane %>% group_by(recordDay) %>% summarise(avgPulse = mean(value))
cos$recordDay <- as.POSIXct(cos$recordDay, format = "%Y:%m:%d")
write.csv(cos, "HeartRate_Maciek.csv", row.names = FALSE)
setwd("C:\\Users\\macie\\Desktop\\STUDIA\\SEMESTR3\\Techniki Wizualizacji Danych\\PROJEKTY\\Project2\\MM")
HeartRate_Maciek <- read.csv("HeartRate_Maciek.csv")
HeartRate_Maciek$recordDay <- as.POSIXct(HeartRate_Maciek$recordDay)
#wykres idividual 3
pp <- HeartRate_Maciek %>% 
  mutate(mean_pulse = mean(avgPulse)) %>% 
  ggplot(aes(x = recordDay, y = avgPulse)) + 
  scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) + 
  geom_line(color = "darkred") +
  geom_hline(aes(yintercept = mean_pulse, color = "Średnia wartość"), linetype = "dashed", size = 1) +
  labs(x = "x",
       y = "y",
       color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(pp)
# plot_ly(data = cos, x = ~recordDay, y = ~avgPulse, type = "scatter", mode = "lines", fill = "toself", fillcolor = "rgba(255, 0, 0, 0.5)") %>%
#   layout(
#     xaxis = list(tickformat = "%Y-%m", tickangle = 45),
#     yaxis = list(range = c(0, max(cos$avgPulse))),
#     showlegend = FALSE
#   )

############################################################################################################

#motion path detail

############################################################################################################


scieżka_do_pliku_motion_path <- "data/HUAWEI_HEALTH_20231211144413 (1)/Motion path detail data & description/motion path detail data.json"
dane_json_motion_path <- fromJSON(scieżka_do_pliku_motion_path)

motion_dane<-dane_json_motion_path %>% select(startTime, endTime, totalDistance, totalTime,totalCalories, sportType,attribute)

motion_dane$startTime <- as.POSIXct(motion_dane$startTime / 1000, origin = "1970-01-01", tz = "UTC")
motion_dane$endTime <- as.POSIXct(motion_dane$endTime / 1000, origin = "1970-01-01", tz = "UTC")
motion_dane <- motion_dane %>%filter(sportType == 3)
motion_dane$totalTime <- motion_dane$totalTime/1000
motion_dane$avgSpeed <- (motion_dane$totalDistance/1000)/(motion_dane$totalTime/3600)
View(motion_dane)
motion_dane$Dystans <- motion_dane$totalDistance/1000
motion_dane <- motion_dane %>% select(-totalDistance)
motion_dane$Osoba <- "Maciek"
motion_dane$Kalorie <- motion_dane$totalCalories/1000
motion_dane <- motion_dane %>% select(-totalCalories)
motion_dane$czasTrwania <- as.POSIXct(motion_dane$totalTime, origin = "1970-01-01", tz = "UTC")
motion_dane$Czas <- format(motion_dane$czasTrwania, "%H:%M:%S")
motion_dane <- motion_dane %>%  select(-sportType,-czasTrwania)
motion_dane_do_csvki <- motion_dane %>% select(-attribute,-totalTime)
View(motion_dane_do_csvki)
write.csv(motion_dane_do_csvki, "ActivitiesMaciek.csv", row.names = FALSE)

extract_coordinates <- function(text) {
  # Szukanie wszystkich wystąpień wzorca lat=...;lon=...
  matches <- gregexpr("lat=([-+]?[0-9]*\\.?[0-9]+);lon=([-+]?[0-9]*\\.?[0-9]+)", text, perl = TRUE)
  
  # Wydobywanie współrzędnych lat i lon
  lat_lon <- regmatches(text, matches)[[1]]
  
  # Tworzenie ramki danych z współrzędnymi
  coordinates_df <- data.frame(
    lat = as.numeric(gsub("lat=|;.*", "", lat_lon)),
    lon = as.numeric(gsub(".*lon=|;.*", "", lat_lon))
  )
  
  return(coordinates_df)
}
calculateDistance <- function(lat1, lon1, lat2, lon2) {
  distVincentySphere(c(lon1, lat1), c(lon2, lat2))
}
##### ciekawe trasy (TOP5) - czyszczenie
# trasa najdluzsza
najdluzsza_trasa <- motion_dane[162,]
najdluzsza_trasa_wspolrzedne <- do.call(rbind, lapply(najdluzsza_trasa$attribute, extract_coordinates))
najdluzsza_trasa <- najdluzsza_trasa %>% select(-attribute)
najdluzsza_trasa_official <- cbind(najdluzsza_trasa, najdluzsza_trasa_wspolrzedne)
najdluzsza_trasa_official <- najdluzsza_trasa_official[-c(4,5941,7850,7890,7901,10001,11273,11831,11894,13767,15173),]
najdluzsza_trasa_official$distance <- c(0, mapply(calculateDistance, najdluzsza_trasa_official$lat[-nrow(najdluzsza_trasa_official)], najdluzsza_trasa_official$lon[-nrow(najdluzsza_trasa_official)], najdluzsza_trasa_official$lat[-1], najdluzsza_trasa_official$lon[-1]))
najdluzsza_trasa_official$timeBetween <- najdluzsza_trasa_official$totalTime/nrow(najdluzsza_trasa_official)
najdluzsza_trasa_official$speed <- (najdluzsza_trasa_official$distance/1000)/(najdluzsza_trasa_official$timeBetween/3600)
write.csv(najdluzsza_trasa_official, "TOP5_Maciek_1.csv", row.names = FALSE)
leaflet() %>%
  addTiles() %>%
  addPolylines(data = najdluzsza_trasa_official, lat = ~lat, lng = ~lon, color = "#000000", opacity = 0.8, weight = 3) %>% addCircleMarkers(data=najdluzsza_trasa_official[c(1,nrow(najdluzsza_trasa_official)),],lat=~lat,lng =~lon)
View(najdluzsza_trasa_official)

# trasa duzo kcal

trasa_2 <- motion_dane[78,]
trasa_2_wspolrzedne <- do.call(rbind, lapply(trasa_2$attribute, extract_coordinates))
trasa_2 <- trasa_2 %>% select(-attribute)
trasa_2_official <- cbind(trasa_2, trasa_2_wspolrzedne)
trasa_2_official <- trasa_2_official[-c(13492,7479,6069,3567),]
trasa_2_official$distance <- c(0, mapply(calculateDistance, trasa_2_official$lat[-nrow(trasa_2_official)], trasa_2_official$lon[-nrow(trasa_2_official)], trasa_2_official$lat[-1], trasa_2_official$lon[-1]))
trasa_2_official$timeBetween <- trasa_2_official$totalTime/nrow(trasa_2_official)
trasa_2_official$speed <- (trasa_2_official$distance/1000)/(trasa_2_official$timeBetween/3600)
write.csv(trasa_2_official, "TOP5_Maciek_2.csv", row.names = FALSE)
leaflet() %>%
  addTiles() %>%
  addPolylines(data = trasa_2_official, lat = ~lat, lng = ~lon, color = "#000000", opacity = 0.8, weight = 3) %>% addCircleMarkers(data=trasa_2_official[c(1,nrow(trasa_2_official)),],lat=~lat,lng =~lon)
View(trasa_2_official)
sum(trasa_2_official$speed)/nrow(trasa_2_official)
# trasa najszybsza srednia predkosc 

trasa_3 <- motion_dane[185,]
trasa_3_wspolrzedne <- do.call(rbind, lapply(trasa_3$attribute, extract_coordinates))
trasa_3 <- trasa_3 %>% select(-attribute)
trasa_3_official <- cbind(trasa_3, trasa_3_wspolrzedne)
trasa_3_official <- trasa_3_official[-c(1450),]
trasa_3_official$distance <- c(0, mapply(calculateDistance, trasa_3_official$lat[-nrow(trasa_3_official)], trasa_3_official$lon[-nrow(trasa_3_official)], trasa_3_official$lat[-1], trasa_3_official$lon[-1]))
trasa_3_official$timeBetween <- trasa_3_official$totalTime/nrow(trasa_3_official)
trasa_3_official$speed <- (trasa_3_official$distance/1000)/(trasa_3_official$timeBetween/3600)
write.csv(trasa_3_official, "TOP5_Maciek_3.csv", row.names = FALSE)
leaflet() %>%
  addTiles() %>%
  addPolylines(data = trasa_3_official, lat = ~lat, lng = ~lon, color = "#000000", opacity = 0.8, weight = 3) %>% addCircleMarkers(data=trasa_3_official[c(1,nrow(trasa_3_official)),],lat=~lat,lng =~lon)
View(trasa_3_official)
sum(trasa_3_official$speed)/nrow(trasa_3_official)
#fajna traska

trasa_4 <- motion_dane[18,]
trasa_4_wspolrzedne <- do.call(rbind, lapply(trasa_4$attribute, extract_coordinates))
trasa_4 <- trasa_4 %>% select(-attribute)
trasa_4_official <- cbind(trasa_4, trasa_4_wspolrzedne)
trasa_4_official <- trasa_4_official[-c(9905),]
trasa_4_official$distance <- c(0, mapply(calculateDistance, trasa_4_official$lat[-nrow(trasa_4_official)], trasa_4_official$lon[-nrow(trasa_4_official)], trasa_4_official$lat[-1], trasa_4_official$lon[-1]))
trasa_4_official$timeBetween <- trasa_4_official$totalTime/nrow(trasa_4_official)
trasa_4_official$speed <- (trasa_4_official$distance/1000)/(trasa_4_official$timeBetween/3600)
write.csv(trasa_4_official, "TOP5_Maciek_4.csv", row.names = FALSE)
leaflet() %>%
  addTiles() %>%
  addPolylines(data = trasa_4_official, lat = ~lat, lng = ~lon, color = "#000000", opacity = 0.8, weight = 3) %>% addCircleMarkers(data=trasa_4_official[c(1,nrow(trasa_4_official)),],lat=~lat,lng =~lon)
View(trasa_4_official)
sum(trasa_4_official$speed)/nrow(trasa_4_official)
#fajna traska 2

trasa_5 <- motion_dane[184,]
trasa_5_wspolrzedne <- do.call(rbind, lapply(trasa_5$attribute, extract_coordinates))
trasa_5 <- trasa_5 %>% select(-attribute)
trasa_5_official <- cbind(trasa_5, trasa_5_wspolrzedne)
trasa_5_official <- trasa_5_official[-c(6813),]
trasa_5_official$distance <- c(0, mapply(calculateDistance, trasa_5_official$lat[-nrow(trasa_5_official)], trasa_5_official$lon[-nrow(trasa_5_official)], trasa_5_official$lat[-1], trasa_5_official$lon[-1]))
trasa_5_official$timeBetween <- trasa_5_official$totalTime/nrow(trasa_5_official)
trasa_5_official$speed <- (trasa_5_official$distance/1000)/(trasa_5_official$timeBetween/3600)
write.csv(trasa_5_official, "TOP5_Maciek_5.csv", row.names = FALSE)
leaflet() %>%
  addTiles() %>%
  addPolylines(data = trasa_5_official, lat = ~lat, lng = ~lon, color = "#000000", opacity = 0.8, weight = 3) %>% addCircleMarkers(data=trasa_5_official[c(1,nrow(trasa_5_official)),],lat=~lat,lng =~lon)
View(trasa_5_official)
sum(trasa_5_official$speed)/nrow(trasa_5_official)
###########################
ActivitiesMaciek <- read.csv("ActivitiesMaciek.csv")
ActivitiesOla <- read.csv("ActivitiesOla.csv")
ActivitiesKuba <- read.csv("ActivitiesKuba.csv")
ActivitiesMaciek <- ActivitiesMaciek %>% select(-endTime)
colnames(ActivitiesOla)[1] <- "startTime"
colnames(ActivitiesOla)[7] <- "avgSpeed"
ActivitiesOla$Osoba <- "Ola"
ActivitiesOla <- ActivitiesOla %>% select(c(startTime,avgSpeed,Dystans,Osoba,Kalorie,Czas))
ActivitiesTogether <- rbind(ActivitiesMaciek,ActivitiesOla)
ActivitiesTogether$startTime <- as.POSIXct(ActivitiesTogether$startTime, format = "%Y-%m-%d %H:%M:%S")
ActivitiesTogether$Rok <- format(ActivitiesTogether$startTime, "%Y")
colnames(ActivitiesTogether)[2] <- "ŚredniaPrędkość"
przelicz_czas_na_minuty <- function(czas_str) {
  skladniki_czasu <- strsplit(czas_str, ":")[[1]]
  godziny <- as.integer(skladniki_czasu[1])
  minuty <- as.integer(skladniki_czasu[2])
  sekundy <- as.integer(skladniki_czasu[3])
  return(godziny * 60 + minuty + sekundy / 60)
}
ActivitiesTogether$Czas <- sapply(ActivitiesTogether$Czas, przelicz_czas_na_minuty)

ActivitiesTogether$Rok <- as.numeric(ActivitiesTogether$Rok)
############
ActivitiesKuba <- ActivitiesKuba %>% filter(Activity.Type == "Ride") %>% select(startTime,avgSpeed,Dystans,Osoba,Kalorie,Czas)
ActivitiesKuba$startTime <- as.POSIXct(ActivitiesKuba$startTime, format = "%Y-%m-%d %H:%M:%S")
ActivitiesKuba$Rok <- format(ActivitiesKuba$startTime, "%Y")
colnames(ActivitiesKuba)[2] <- "ŚredniaPrędkość"
ActivitiesTogether <- rbind(ActivitiesTogether,ActivitiesKuba)
write.csv(ActivitiesTogether, "ActivitiesTogether.csv", row.names = FALSE)
