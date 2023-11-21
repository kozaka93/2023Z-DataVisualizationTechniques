library(maps)
library(dplyr)
library(geojsonio)
library(leaflet)
library(sp)

library(htmlwidgets)
library(htmltools)

#data source: https://www.fao.org/faostat/en/#data/OA
#geojson source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries

df <- read.csv("Population_E_Europe_NOFLAG.csv")
str(df)

unique(df$Element)

divide <- function(x){x/1000}

df %>% 
  filter(Element == "Total Population - Both sexes") %>% 
  select(Area,Element,Unit, Y2000:Y2023) %>% 
  mutate(across(Y2000:Y2023, divide))-> df


europe <-geojson_read("CNTR_RG_60M_2020_4326.geojson", what = "sp")

e2s <- europe[europe$EU_STAT == "T", ]

e2s <- merge(e2s,df,by.x = "NAME_ENGL", by.y = "Area")

m <- leaflet(e2s) %>%
  setView(10, 57, 3.5) %>%
  addTiles()
m

bins <- c(0,500,1000,2000,5000,10000,20000,50000,Inf)/1000
pal <- colorBin("YlOrRd", domain = e2s$Y2023 ,bins = bins)
yrs <- paste("Y", 2000:2023, sep = "")

for(y in yrs){
  labels <- sprintf(
    "<strong>%s</strong><br/>%s people",
    e2s$NAME_ENGL, format(e2s[[y]]*1000000, big.mark = " ", scientific = F)
  ) %>% lapply(htmltools::HTML)
  
  m <- m %>% addPolygons(
    group = substr(y,2,5),
    fillColor = ~pal(e2s[[y]]),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 4,
      color = "#555555",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))
}

m %>% 
  addLegend(
    position = "bottomleft", 
    pal = pal,
    values = ~Y2023,
    opacity = 0.7, title = "Population\n(milion people)") %>% 
  addLayersControl(baseGroups = substr(yrs,2,5),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright") %>% 
  addControl(HTML("<strong>Population in EU</strong></br>years 2000:2023"),
             position = "topright") %>% 
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Reset View",
    onClick=JS("function(btn, map){ map.setView([57,10],3.5);}")
    )
  ) %>% onRender("
    function() {
        $('.leaflet-control-layers-list').prepend('Select year');
    }
")
