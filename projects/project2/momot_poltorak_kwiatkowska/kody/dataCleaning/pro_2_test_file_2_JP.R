library(plotKML)  #for reading gpx
library(dplyr)    #for setting ele to numeric
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(XML)
library(leafgl)
library(sf)

install.packages("plotKML", repos=c("http://R-Forge.R-project.org")) 

gpx_parsed<-htmlTreeParse(file="D:\\r_pro_2\\activities\\5185484253.gpx",useInternalNodes = TRUE)
coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)

mydata <- data.frame(
  lat = as.numeric(coords["lat", ]),
  lon = as.numeric(coords["lon", ]),
  ele = as.numeric(elevation)
)

maciek_1<-read.csv("D:\\r_pro_2\\R_files\\TOP5_Maciek_1.csv")
mydata<-data.frame(
  lat=maciek_1$lat,
  lon=maciek_1$lon,
  ele=maciek_1$speed,
  distance=maciek_1$distance
)
mydata %>% filter(ele<100) -> mydata
mydata %>% mutate(summarized_distance=0) -> mydata
mydata %>% mutate(summarized_distance=ifelse(row_number()!=1,cumsum(distance),distance)) -> mydata

mydata %>% mutate(kilometer= ceiling(summarized_distance/1000)) -> mydata


mydata$ele<-ifelse(mydata$ele>40,40,ceiling(mydata$ele))

mydata %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km

mydata %>% group_by(kilometer) %>% summarize(mean_speed=mean(ele)) -> avg_speed_per_km

coords_of_kilometers<-last_lon_lat_each_km %>% left_join(avg_speed_per_km,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)

paste("Kilometer: ","<strong>",coords_of_kilometers$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers$mean_speed,2),"</strong>",sep="") %>%
  lapply(htmltools::HTML) -> labels

download.file("https://raw.githubusercontent.com/iosphere/Leaflet.hotline/master/dist/leaflet.hotline.js", 
              'C:/Temp/leaflet.hotline.js', mode="wb")

hotlinePlugin <- htmltools::htmlDependency(
  name = 'Leaflet.hotline',
  version = "0.4.0",
  src = c(file = normalizePath('C:/Temp')),
  script = "leaflet.hotline.js"
)

registerPlugin <- function( map, plugin ) {
  map$dependencies <- c( map$dependencies, list( plugin ) )
  map
}

leaflet() %>% addTiles() %>%
  fitBounds( min(mydata$lon), min(mydata$lat), max(mydata$lon), max(mydata$lat) ) %>%
  registerPlugin(hotlinePlugin) %>%
  onRender("function(el, x, data) {
    data = HTMLWidgets.dataframeToD3(data);
    data = data.map(function(val) { return [val.lat, val.lon, val.ele]; });
    L.hotline(data, {min: 81, max: 116.6}).addTo(this);
  }", data = mydata )


palette<- colorNumeric(palette = colorRampPalette(c("#008800","#ffff00","#ff0000"))(5),
                        domain = 0:40)
mydata %>% mutate(Color=palette1(ifelse(ele>50,50,ceiling(ele)))) -> mydata

leaflet(mydata) %>% addTiles() -> test_map_maciek_1

for(Color in levels(as.factor(mydata$Color))){
  test_map_maciek_1<-test_map_maciek_1 %>%  addPolylines(test_map_maciek_1,data=mydata[mydata$Color==Color,],lng=~lon,lat=~lat,color=~Color)
}

for(i in 1:100){
  test<-mydata[c(i,i+1),]
  test_map_maciek_1<-test_map_maciek_1 %>%  addPolylines(test_map_maciek_1,data=test,lng=~lon,lat=~lat,color=~Color)
}

leaflet() %>% addTiles() %>%
  fitBounds( min(mydata$lon), min(mydata$lat), max(mydata$lon), max(mydata$lat) ) %>%
  registerPlugin(hotlinePlugin) %>%
  onRender("function(el, x, data) {
            data = HTMLWidgets.dataframeToD3(data);
            data = data.map(function(val) { return [val.lat, val.lon, val.ele]; });
            L.hotline(data, {min: 0, max: 40}).addTo(this);
          }", data = mydata ) %>% addLegend("bottomright",pal=palette,values=0:40,opacity=1,title="Speed [km/h]") %>% 
  addCircleMarkers(data=coords_of_kilometers,lat=~lat,lng=~lon,label=~labels,fillColor="blue",fillOpacity=1,stroke=F,radius=3)
test_2<-st_sfc(st_multipoint(as.matrix(mydata)))




read.csv("D:\\r_pro_2\\R_files\\activities_JP.csv")->dane_JP

dane_JP %>% filter(Activity.Type=="Ride") %>% group_by(year(startTime)) %>% summarise(suma=sum(Elevation.Gain),per_ride=sum(Elevation.Gain)/n(),rides=n()) -> elevation_gain_data


dane_JP %>% filter(Activity.Type=="Ride") %>% mutate(percent_of_ridden_time=Moving.Time/Elapsed.Time,length_of_track=ifelse( Distance<40,"Short",ifelse(Distance<70,"Medium","Long"))) -> sml_elapsed_moving_time

plot_ly(data=sml_elapsed_moving_time,x=~Distance,y=~percent_of_ridden_time,color=~length_of_track,colors=c("brown2", "dodgerblue2","chartreuse4" ))

sml_elapsed_moving_time %>% group_by(length_of_track) %>% do(p=plot_ly(.,x=~percent_of_ridden_time,name=~length_of_track,type = "histogram",nbinsx=5)) %>% subplot(nrows=1,shareX = TRUE,shareY = TRUE)

dane_JP %>% filter(Activity.Type=="Ride") %>% group_by(year(startTime)) %>% summarise(mean_ele_low=mean(Elevation.Low),mean_ele_high=mean(Elevation.High),mean_max_grade=mean(Max.Grade)) -> mean_ele_low_high_max_grade_year

dane_JP %>% filter(Activity.Type=="Ride") -> dane_JP_rides

activities %>% filter(Activity.Type=="Ride") -> rides

max_elevations<- data.frame(matrix(ncol=8,nrow=0))
col_names<-c("lat","lon","ele","activity_date","distance","time","mean_speed","min_max")
colnames(max_elevations)<- col_names

for(i in 1:nrow(rides)){
  gpx_parsed<-htmlTreeParse(file=paste("D:\\r_pro_2\\activities\\",rides$Activity.ID[i],".gpx",sep=""),useInternalNodes = TRUE)
  coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
  elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)
  mydata <- data.frame(
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ]),
    ele = as.numeric(elevation)
  )
  row_of_max_elevation<-which.max(mydata$ele)
  row_of_min_elevation<-which.min(mydata$ele)
  new_row_min<-c(lat=mydata$lat[row_of_min_elevation],lon=mydata$lon[row_of_min_elevation],
                 ele=mydata$ele[row_of_min_elevation],activity_date=rides$Activity.Date[i],distance=rides$Distance[i],
                 time=rides$Moving.Time[i],mean_speed=rides$Average.Speed[i],min_max="min")
  new_row_max<-c(lat=mydata$lat[row_of_max_elevation],lon=mydata$lon[row_of_max_elevation],
                 ele=mydata$ele[row_of_max_elevation],activity_date=rides$Activity.Date[i],distance=rides$Distance[i],
                 time=rides$Moving.Time[i],mean_speed=rides$Average.Speed[i],min_max="max")
  max_elevations<-rbind(max_elevations,new_row_min)
  max_elevations<-rbind(max_elevations,new_row_max)
}

colnames(max_elevations)<- col_names
max_elevations$lat<-as.numeric(max_elevations$lat)
max_elevations$lon<-as.numeric(max_elevations$lon)
max_elevations$ele<-as.numeric(max_elevations$ele)
max_elevations$distance<-as.numeric(max_elevations$distance)
max_elevations$time<-as.numeric(max_elevations$time)
max_elevations$mean_speed<-as.numeric(max_elevations$mean_speed)

min_max_icons<- iconList(
  "min"=makeIcon(
    iconUrl = "https://www.google.com/url?sa=i&url=https%3A%2F%2Ficonduck.com%2Ficons%2F256853%2Fminimum&psig=AOvVaw3sJs_4VvHf8eqnmaNcfNfd&ust=1704409518534000&source=images&cd=vfe&ved=0CBIQjRxqFwoTCJCTxLOqwoMDFQAAAAAdAAAAABAE",
    iconWidth = 25,
    iconHeight = 25
    ),
  "max"=makeIcon(
    iconUrl = "https://www.google.com/url?sa=i&url=https%3A%2F%2Ficonduck.com%2Ficons%2F256824%2Fmaximum&psig=AOvVaw0U3Z2EX6d3Zh49cBZkBt4_&ust=1704409537432000&source=images&cd=vfe&ved=0CBIQjRxqFwoTCKCJ-bmqwoMDFQAAAAAdAAAAABAE",
    iconWidth = 25,
    iconHeight = 25
  )
)


leaflet() %>% addTiles() %>% addCircleMarkers(data = max_elevations,lng=~lon,lat=~lat,radius=~(ele/10),color= ifelse(max_elevations$min_max=="min","yellow","red"),fillColor = ifelse(max_elevations$min_max=="min","yellow","red"),fillOpacity = 0.8,popup = paste0(
  "<strong>Date: </strong>", max_elevations$activity_date, "<br>",
  "<strong>Distance (km): </strong>", max_elevations$distance, "<br>",
  "<strong>Time : </strong>", seconds_to_period(max_elevations$time), "<br>",
  "<strong>Mean speed (km/h): </strong>", round(max_elevations$mean_speed*3.6,2), "<br>", 
  "<strong>Minimum/Maximum elevation: </strong>", paste(max_elevations$min_max,": ",max_elevations$ele," m",sep=""), "<br>"))

plot_ly() %>% add_trace(data = dane_JP_rides,x=~startTime,y=~Dystans,type="scatter",mode="lines+markers") %>% add_trace(data=dane_JP_rides,x=~startTime,y=~avgSpeed,type="bar")
























library(leaflet)
library(dplyr)

library(leaflet)
library(dplyr)

# Tworzenie przykładowych danych
set.seed(123)
max_elevations <- data.frame(
  lon = runif(10, 18, 19),
  lat = runif(10, 49, 50),
  ele = runif(10, 100, 500),
  min_max = sample(c("min", "max"), 10, replace = TRUE),
  activity_date = Sys.Date() - sample(1:10, 10),
  distance = runif(10, 5, 20),
  time = sample(3600:7200, 10),
  mean_speed = runif(10, 5, 15)
)

# Funkcja do obsługi zdarzenia kliknięcia
onPopupClick <- function(map, layerId) {
  selected_point <- max_elevations[layerId + 1, ]
  
  # Usunięcie oryginalnych kółek
  map$layerManager$removeMarker("original_markers")
  
  # Dodanie nowej mapy z linią
  new_map <- leaflet() %>%
    addTiles() %>%
    addPolylines(lng = selected_point$lon, lat = selected_point$lat, color = "blue") %>%
    addPopup(lng = selected_point$lon, lat = selected_point$lat, 
             popup = paste0(
               "<strong>Date: </strong>", selected_point$activity_date, "<br>",
               "<strong>Distance (km): </strong>", selected_point$distance, "<br>",
               "<strong>Time : </strong>", seconds_to_period(selected_point$time), "<br>",
               "<strong>Mean speed (km/h): </strong>", round(selected_point$mean_speed * 3.6, 2), "<br>", 
               "<strong>Minimum/Maximum elevation: </strong>", selected_point$min_max, "<br>")
    )
  
  # Wyświetlenie nowej mapy
  print(new_map)
}

# Tworzenie pierwotnej mapy z kółkami
original_map <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    data = max_elevations,
    lng = ~lon, lat = ~lat,
    radius = ~(ele / 10),
    color = ifelse(max_elevations$min_max == "min", "yellow", "red"),
    fillColor = ifelse(max_elevations$min_max == "min", "yellow", "red"),
    fillOpacity = 0.8,
    popup = paste0(
      "<strong>Date: </strong>", max_elevations$activity_date, "<br>",
      "<strong>Distance (km): </strong>", max_elevations$distance, "<br>",
      "<strong>Time : </strong>", seconds_to_period(max_elevations$time), "<br>",
      "<strong>Mean speed (km/h): </strong>", round(max_elevations$mean_speed * 3.6, 2), "<br>", 
      "<strong>Minimum/Maximum elevation: </strong>", max_elevations$min_max, "<br>"
    ),
    group = "original_markers"
  ) %>% 
  addLayersControl(overlayGroups = c("original_markers"), options = layersControlOptions(collapsed = FALSE))

# Dodanie obsługi zdarzenia kliknięcia do każdego kółka na pierwotnej mapie
for (i in 1:nrow(max_elevations)) {
  original_map$dependencies[[i]]$options$popupOnClick <- JS(sprintf("function(e) { onPopupClick(%d); }", i - 1))
}

# Wyświetlenie pierwotnej mapy
print(original_map)