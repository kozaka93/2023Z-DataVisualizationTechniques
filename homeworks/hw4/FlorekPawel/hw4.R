library(plotly)
library(dplyr)

df <- data.frame(
  Rok = 2009:2021,
  Wydatki = c(4.8, 4.7, 4.5, 4.4, 4.5, 4.4, 4.5, 4.5, 4.6, 4.5, 4.9, 5.0, 5.3)/100
)

p <- plot_ly(
  data = df,
  x = ~Rok,
  y = ~Wydatki,
  type = "bar",
  marker = list(color = 'lightgreen')
) %>% 
  layout(
    title = list(text = "Wydatki na ochronę zdrowia jako % PKB", 
                 font = list(color = "white")),
    xaxis = list(
      title = list(text = "Rok", 
                   font = list(color = "white")),
      tickfont = list(color = "white"),
      dtick = 1, 
      tick0 = 2009, 
      tickmode = "linear"),
    yaxis = list(
      title = list(text = "Wydatki jako % PKB", 
                   font = list(color = "white")),
      tickfont = list(color = "white"),
      range = c(0, 0.061),
      tickformat = "0.1%",
      gridcolor = "gray"
      ),
    paper_bgcolor = "rgb(0, 0, 139)",
    plot_bgcolor = "rgb(0, 0, 139)"
  )


p

