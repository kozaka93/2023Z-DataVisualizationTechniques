library(dplyr)
library(ggplot2)
library(plotly)

df <- read.csv("../app/KomunikacJA/appData/heatMap/heatMapData.csv") 



df %>% 
  filter(person == "f") %>% 
  filter(year == "2022")
  group_by(app) %>% 
  summarise(liczba_wiadomosci = n()) -> dat

dat$fraction = dat$liczba_wiadomosci / sum(dat$liczba_wiadomosci)

# Compute the cumulative percentages (top of each rectangle)
dat$ymax = cumsum(dat$fraction)

# Compute the bottom of each rectangle
dat$ymin = c(0, head(dat$ymax, n=-1))

plotly(
  ggplot(dat, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=app)) +
    geom_rect() +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) +
    theme_minimal()
)
