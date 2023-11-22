#####################       Biblioteki        #####################

library(geojsonio)
library(leaflet)
library(ggplot2)
library(dplyr)
library(SmarterPoland)
library(tidyverse)

###################################################################


stats <- read.csv("tour_cap_nat__custom_8097699_page_linear.csv")

stats %>% select(geo, OBS_VALUE) %>% 
  arrange(-OBS_VALUE) %>% 
  filter(!is.na(OBS_VALUE)) -> Bedplaces_2022_with_sum

Bedplaces_2022_with_sum %>% top_n(1) -> tmp

estimated_sum <- tmp$OBS_VALUE

Bedplaces_2022_with_sum %>% 
  filter(OBS_VALUE != estimated_sum) -> Bedplaces_2022_no_ir

Bedplaces_2022_no_ir %>% 
  summarise(all = sum(OBS_VALUE)) -> tmp2

sum_no_ireland <- tmp2$all

estimated_ireland <- estimated_sum - sum_no_ireland

Bedplaces_2022_no_ir %>% 
  add_row(geo = "Ireland", OBS_VALUE = estimated_ireland) %>% 
  mutate(Bedplaces = OBS_VALUE) -> beds

beds <- beds %>%
  mutate(geo = ifelse(geo == "Czechia", "Czech Republic", geo))

europe_countries <- geojsonio::geojson_read("europe.geojson", what = "sp")

eu_countries <- europe_countries[europe_countries$NAME %in% c("Italy",
                                                              "France",
                                                              "Spain",
                                                              "Germany",
                                                              "Netherlands",
                                                              "Greece",       
                                                              "Croatia",
                                                              "Austria",  
                                                              "Sweden",
                                                              "Czech Republic",
                                                              "Poland",
                                                              "Portugal",      
                                                              "Denmark",
                                                              "Belgium",
                                                              "Romania",
                                                              "Hungary",
                                                              "Bulgaria",
                                                              "Finland",       
                                                              "Slovakia",
                                                              "Slovenia",
                                                              "Lithuania",
                                                              "Cyprus",
                                                              "Estonia",
                                                              "Luxembourg",    
                                                              "Malta",
                                                              "Latvia",
                                                              "Ireland"), ]

eu_countries_beds <- merge(eu_countries, beds, by.x = "NAME", by.y = "geo", all.x = TRUE)


leaflet(eu_countries_beds) %>%
  setView(19, 52, 4) %>%
  addProviderTiles("CartoDB.DarkMatter") -> m

bins <- c(45000, 200000, 500000, 1000000, 2000000, 5000000, 5300000)
pal <- colorBin(c("#F9EF62", "#FB5D0E", "#660000"), domain = eu_countries_beds$ISO2, bins = bins)



labels <- sprintf(
  "<strong>%s</strong><br/>%s Bedplaces",
  eu_countries_beds$NAME, format(eu_countries_beds$Bedplaces, big.mark = " ", scientific = FALSE)) %>%
  lapply(htmltools::HTML)

m %>% addPolygons(
  fillColor = ~pal(Bedplaces),
  weight = 1,
  opacity = 1,
  color = "#303030",
  dashArray = "0",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 3,
    color = "#FFFFFF",
    dashArray = "",
    fillOpacity = 0.3,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "18px",
    direction = "auto")) %>% 
  addLegend(pal = pal, values = ~Bedplaces, opacity = 0.7, title = "Number of bed places:",
            position = "bottomright") %>% 
  addControl(
    html = "<p><strong style='font-size: 25px;'>Number of bed places in tourist accomodation in EU countries (2022)</strong></p>",
    position = "topright"
  )
