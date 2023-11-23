library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(ggrepel)
library(fmsb)

alcohol_raw <- read.csv("./data/alcohol.csv", sep = ";")

par(mar = c(1, 1, 1, 1))

alcohol <- alcohol_raw %>% 
            select(Countries, Type, X2019, X2018, X2017) %>% 
            mutate(Type = trimws(Type)) %>% 
            pivot_longer(cols = c("X2019", "X2018", "X2017"), names_to = "Year", values_to="Value") %>% 
            mutate(Year = gsub("X", "", Year)) %>% 
            pivot_wider(names_from = Type, values_from = Value, values_fill = 0)


data <- alcohol %>% 
        filter(Year == 2018 & Countries %in% c("Finland", "Poland", "France", "Japan")) %>% 
        rename(Other = `Other alcoholic beverages`)

for (i in 1:4) {
  country_name <- as.character(data[i, 1])
  country_data <- data[i, ]
  country_data <- country_data %>% select(Beer, Wine, Spirits, Other)
  
  colors <- RColorBrewer::brewer.pal(n = 5, name = "YlOrBr")
  
  png(paste0("./charts/",country_name, "_radar_chart.png"), width = 400, height = 400, bg = "transparent")
  
  p <- radarchart(rbind(rep(7, 4), rep(0, 4), country_data), 
                  #Polygon
                  axistype = 1, 
                  pcol = colors[(i %% 4) + 2],
                  pfcol = alpha(colors[(i %% 4) + 2], 0.7),
                  plwd = 3,
                  
                  #grid
                  cglcol = "gray",
                  seg = 3, 
                  cglty = 5,
                  cglwd = 0.8,
                  axislabcol = "gray",
                  caxislabels = seq(0, 10, 2),
                  
                  #Label
                  title = country_name,
                  vlcex = 1.4,
                  calcex = 1.6
                  
  )
  dev.off()
  ?radarchart

}

  