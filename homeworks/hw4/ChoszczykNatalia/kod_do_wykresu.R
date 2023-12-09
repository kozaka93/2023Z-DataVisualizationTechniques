library(plotly)
library(dplyr)

canada_crime <- read.csv("canada_crime.csv", sep = ';')

plot_ly(
  data = canada_crime, 
  x = ~Province.or.territory, 
  y = ~Crime.Severity.Index,
  type = 'bar',
  marker = list(color = ifelse(canada_crime$Province.or.territory == "Canada", "black", "red"))
  
) %>%
  
layout(
  title = list(
    text = "Police-reported Crime Severity Indexes, by province and territory, 2021",
    font = list(size = 14, color = "black")
    ),
  
  xaxis = list(categoryorder = 'trace', title = ""),
  yaxis = list(title = "index", range = c(0, 600)),
  
  updatemenus = list(
    list(
      x = 0.5, y = 0.95,
      buttons = list(
        list(method = "restyle",
             args = list("y", list(~Crime.Severity.Index)),
             label = "Crime severity index"
             ),
        
        list(method = "restyle",
             args = list("y", list(~Violent.Crime.Severity.Index)),
             label = "Violent crime severity index"
             ),
        
        list(method = "restyle",
             args = list("y", list(~Non.violent.Crime.Severity.Index)),
             label = "Non violent crime severity index"
             )
      ))
  ))
