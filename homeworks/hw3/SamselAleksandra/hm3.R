library(ggplot2)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)


# data
cities <- read.csv("uscities.csv", sep = ",")
cities <- cities %>% filter(population > 0)
popAll <- sum(cities$population)

# functions
make_ellipse <- function(center, semi_major_axis, semi_minor_axis, angle) {
        n_points <- 200
        theta <- seq(0, 2*pi, length.out = n_points)
        ellipse_points <- data.frame(
                lng = center['lng'] + semi_major_axis * cos(theta) * cos(angle * pi/180) - semi_minor_axis * sin(theta) * sin(angle * pi/180),
                lat = center['lat'] + semi_major_axis * cos(theta) * sin(angle * pi/180) + semi_minor_axis * sin(theta) * cos(angle * pi/180)
        )
        return(ellipse_points)
        
}

percent_of_area <- function(ellipse, usaData = read.csv("fixed_polygon.csv", sep = ",")){
        
        usaPol <- usaData %>% as.matrix()
        elPol <- ellipse %>% as.matrix()
        elPol <- rbind(elPol, elPol[1, ])
        
        sfc_polygon1 <- st_sfc(st_polygon(list(usaPol)), crs = 4326)
        sfc_polygon2 <- st_sfc(st_polygon(list(elPol)), crs = 4326)
        
        sfc_polygon1_projected <- st_transform(sfc_polygon1, crs = 3395)
        sfc_polygon2_projected <- st_transform(sfc_polygon2, crs = 3395)
        
        intersection <- st_intersection(sfc_polygon1_projected, sfc_polygon2_projected)
        
        return(st_area(intersection)/st_area(sfc_polygon1_projected) * 100)
}


filter_points_inside_ellipse <- function(df, center, semi_major_axis, semi_minor_axis, angle) {
        
        df_translated <- within(df, {
                lat <- lat - center['lat']
                lng <- lng - center['lng']
        })
        angle_rad <- -angle * pi / 180
        df_rotated <- within(df_translated, {
                lng_rot <- cos(angle_rad) * lng - sin(angle_rad) * lat
                lat_rot <- sin(angle_rad) * lng + cos(angle_rad) * lat
                lng <- lng_rot
                lat <- lat_rot
        })
        df_scaled <- within(df_rotated, {
                lng <- lng / semi_major_axis
                lat <- lat / semi_minor_axis
        })
        df_scaled$inside_oval <- with(df_scaled, lng^2 + lat^2 <= 1)
        df_inside_oval <- df_scaled[df_scaled$inside_oval, c('city', 'lat', 'lng', 'population')]
        
        return(df_inside_oval)
}

return_stats <- function(cities, center, semi_major_axis, semi_minor_axis, angle){
        popDf <- filter_points_inside_ellipse(cities, center, semi_major_axis, semi_minor_axis, angle)
        popEl <- sum(popDf$population)
        popAll <- sum(cities$population)
        percent <- popEl / popAll * 100
        return(c(popEl, percent))
}


# ellipse 1
center1 <- c(lat = 35, lng = -78)
semi_major_axis1 <- 15
semi_minor_axis1 <- 5
angle1 <- 57
ellipse1 <- make_ellipse(center1, semi_major_axis1, semi_minor_axis1, angle1)

# ellipse 2
center2 <- c(lat = 40, lng = -121)
semi_major_axis2 <- 10
semi_minor_axis2 <- 4
angle2 <- 110
ellipse2 <- make_ellipse(center2, semi_major_axis2, semi_minor_axis2, angle2)

# ellipse 3
center3 <- c(lat = 39, lng = -100)
semi_major_axis3 <- 17
semi_minor_axis3 <- 11
angle3 <- 0
ellipse3 <- make_ellipse(center3, semi_major_axis3, semi_minor_axis3, angle3)


# population in ellipses
stat1 <- return_stats(cities, center1, semi_major_axis1, semi_minor_axis1, angle1)
stat2 <- return_stats(cities, center2, semi_major_axis2, semi_minor_axis2, angle2)
stat3 <- return_stats(cities, center3, semi_major_axis3, semi_minor_axis3, angle3)

# boxes
div <- tags$div(HTML(sprintf('<div>
  <p>Niebieskimi punktami zaznaczono miejscowości, których populacja uwzględniona jest w obliczenich</p>
  <p>Obszar USA zaznaczony pomarańczowymi elipsami zamieszkuje <span style="color: orange;">%.*f%s</span> 
  całej populacji kraju na <span style="color: orange;">%.*f%s</span> powierzchni całkowitej</p>
</div>
', 1, (stat1[1] + stat2[1])/sum(cities$population) * 100, "%", 1, percent_of_area(ellipse1) + percent_of_area(ellipse2), "%")))

title <- tags$div(HTML("<div><p><b>Mapa pokazująca rozmieszczenie ludności w USA</b></p></span>"))

# map
usa <- leaflet() %>%
        setView(-96, 37.8, 4) %>%
        addTiles() %>% 
        addCircleMarkers(lng = ~lng, lat = ~lat, data = cities, 
                         radius = 0.1, stroke = FALSE) %>% 
        addPolygons(data = ellipse1, lng = ~lng, lat = ~lat, fillColor = "orange", 
                    fillOpacity = 0.3, color = "orange", weight = 1, 
                    popup = sprintf('<span style="color: orange;">%.*f%s</span> populacji kraju zamieszkuje
                                    na <span style="color: orange;">%.*f%s</span> powierzchni całkowitej', 1, stat1[2], "%", 1, percent_of_area(ellipse1), "%")) %>% 
        addPolygons(data = ellipse2, lng = ~lng, lat = ~lat, fillColor = "orange", 
                    fillOpacity = 0.3, color = "orange", weight = 1, 
                    popup = sprintf('<span style="color: orange;">%.*f%s</span> populacji kraju zamieszkuje
                                    na <span style="color: orange;">%.*f%s</span> powierzchni całkowitej', 1, stat2[2], "%", 1, percent_of_area(ellipse2), "%")) %>% 
        addPolygons(data = ellipse3, lng = ~lng, lat = ~lat, fillColor = "green", 
                    fillOpacity = 0.3, color = "green", weight = 1, 
                    popup = sprintf('<span style="color: green;">%.*f%s</span> populacji kraju zamieszkuje
                                    na <span style="color: green;">%.*f%s</span> powierzchni całkowitej', 1, stat3[2], "%", 1, percent_of_area(ellipse3), "%")) %>% 
        addControl(div, position = "bottomleft") %>% 
        addControl(title, position = "topright")

usa

           
