install.packages("XML")
library(XML)
library(sf)
gpx_parsed<-htmlTreeParse(file="D:\\r_pro_2\\activities\\5182726468.gpx",useInternalNodes = TRUE)
gpx_parsed<-htmlTreeParse(file="D:\\r_pro_2\\activities\\5185484253.gpx",useInternalNodes = TRUE)
coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)


df <- data.frame(
  lat = as.numeric(coords["lat", ]),
  lon = as.numeric(coords["lon", ]),
  elevation = as.numeric(elevation)
)

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addPolylines(data = df, lat = ~lat, lng = ~lon, color = "#000000", opacity = 0.8, weight = 3)

first_and_last<-df[c(1,nrow(df)),]



leaflet() %>%
  addTiles() %>%
  addPolylines(data = df, lat = ~lat, lng = ~lon, color = ~ele, opacity = 0.8, weight = 3) %>% addCircleMarkers(data=df[c(1,nrow(df)),],lat=~lat,lng =~lon)

maciek_1_test<-read.csv("D:\\r_pro_2\\R_files\\TOP5_Maciek_1.csv")

maciek_1 %>% mutate(row_n=row_number()) ->maciek_1

maciek_1 %>% filter(speed<100) -> maciek_1
maciek_1 %>% mutate(nr_of_minute=1+(row_n-1)%/%60) -> maciek_1
maciek_1 %>% group_by(nr_of_minute) %>% summarize(mean_speed=mean(speed))-> maciek_1

plot_ly(maciek_1, x = ~nr_of_minute, y = ~mean_speed, type = 'scatter', mode = 'lines') %>% layout(title = "Średnia prędkość w poszczególnych minutach aktywności nr 1",
                                                                                                   xaxis = list(title = "Minuta"),
                                                                                                   yaxis = list (title = "Średnia prędkość"))
maciek_1_test %>% filter(speed<100) ->maciek_1_test
maciek_1_test %>% mutate(summarized_distance=0) -> maciek_1_test
maciek_1_test %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> maciek_1_test

maciek_1_test %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> maciek_1_test

maciek_1_test %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km


plot_ly(avg_speed_per_km, x = ~kilometer, y = ~round(mean_speed,2), type = 'scatter', mode = 'lines') %>% layout(title = "Średnia prędkość w poszczególnych minutach aktywności nr 1",
                                                                                                        xaxis = list(title = "Minuta"),
                                                                                                        yaxis = list (title = "Średnia prędkość"))


df<-data.frame(lat=maciek_1$lat,lon=maciek_1$lon)
df %>% mutate(speed=maciek_1$speed)


activities<-read.csv("D:\\r_pro_2\\activities.csv")
activities<
# Aktywności:
# 1) activities/7759896918.gpx Najdłuższe
# 2) activities/5883404610.gpx Radom
# 3) activities/7350288688.gpx Warka 4,5 h
# 4) activities/7298422824.gpx Góra 2h 17 min
# 5) activities/5548459678.gpx Zegrze

####test4####
library(XML)
library(dplyr)
library(jsonlite)
library(leaflet)
library(htmlwidgets)
library(htmltools)

test4<-activities[4,]
four_activity_file<-test4$Activity.ID
four_time_elapsed<-test4$Elapsed.Time
four_time<-test4$Moving.Time

gpx_parsed<-htmlTreeParse(file=paste("D:\\r_pro_2\\activities\\",four_activity_file,".gpx",sep=""),useInternalNodes = TRUE)
coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)

track <- data.frame(
  lon = as.numeric(coords["lon", ]),
  lat = as.numeric(coords["lat", ]),
  elevation = as.numeric(elevation)
)

bb <- track %>% summarise_at(vars(lon, lat), funs(min, max))

track <- toJSON(track, dataframe = "values")
geojson <- sprintf('{"type":"Feature","geometry":{"type":"LineString","coordinates":%s},"properties":null}', track)
geojson <- toJSON(fromJSON(geojson), auto_unbox = TRUE)

install.packages("devtools")
devtools::install_github("ropenscilabs/geojsonlint")

geojsonlint::geojson_lint(geojson)

elevationPlugin <- htmlDependency("Leaflet.elevation", "0.0.4",
                                  src = "js/elevation/",
                                  script = "leaflet.elevation-0.0.4.src.js",
                                  stylesheet = "leaflet.elevation-0.0.4.css")
d3Plugin <- htmlDependency("d3", "3.5.17",
                           src = "js/d3/",
                           script = "d3.js")
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
setwd("D:\\r_pro_2\\R_files")
library("geojsonlint")
remotes::install_github("ropensci/geojsonlint")

read.csv("D:\\r_pro_2\\R_files\\activities_JP.csv")->dane_JP

dane_JP %>% select(Activity.Date,Activity.Type,Elapsed.Time,Distance,Moving.Time,srednia_predkosc_km_h,max_predkosc_km_h,Elevation.Gain,Elevation.Loss,Elevation.High,Elevation.Low,Max.Grade,Average.Grade) -> dane_przefiltrowane

dane_przefiltrowane %>% mutate(data.frame(do.call('rbind',strsplit(as.character(dane_przefiltrowane$Activity.Date),",",fixed=TRUE))))-> dane_przefiltrowane
as.numeric(dane_przefiltrowane$X2)->dane_przefiltrowane$rok
dane_przefiltrowane %>% mutate(data.frame(do.call('rbind',strsplit(as.character(X1)," ",fixed=TRUE)))) -> dane_przefiltrowane

dane_przefiltrowane %>% mutate(startTime=mdy_hms(Activity.Date)) -> dane_przefiltrowane

dane_przefiltrowane %>% mutate(avgSpeed=srednia_predkosc_km_h,Dystans=Distance,Osoba="Kuba",Kalorie=NA,Czas=Moving.Time/60) %>% select(-c(X1,X2,X3,rok)) -> data_gotowe