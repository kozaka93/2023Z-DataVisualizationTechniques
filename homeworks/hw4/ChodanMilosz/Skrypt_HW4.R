library(dplyr)
library(plotly)



d <- data.frame(year = 2009:2027,
                expenses = c(4.8, 4.7, 4.5, 4.4, 4.5, 4.4, 4.5, 4.5, 4.6, 4.5, 4.9, 5.0, 5.3, 5.6, 5.8, 6.0, 0.0, 0.0, 7.0)/100)
plot <- plot_ly(
  data = d,
  x = ~year,
  y = ~expenses,
  type = "bar",
  marker = list(
    color = ifelse(d$year >= 2024, 'firebrick', 'chartreuse')
  )
) %>% 
  layout(
    title = list(text = "Wydatki na ochronę zdrowia (jako % PKB) na przestrzeni lat", 
                 font = list(color = "white")),
    xaxis = list(
      title = list(text = "Rok", 
                   font = list(color = "white")),
      tickfont = list(color = "white"),
      dtick = 1, 
      tick0 = 2009, 
      tickmode = "linear"),
    yaxis = list(
      title = list(text = "% PKB", 
                   font = list(color = "white")),
      tickfont = list(color = "white"),
      range = c(0, 0.061),
      tickformat = "0.1%",
      gridcolor = "gray"
    ),
    paper_bgcolor = "navy",
    plot_bgcolor = "navy"
  )


plot
