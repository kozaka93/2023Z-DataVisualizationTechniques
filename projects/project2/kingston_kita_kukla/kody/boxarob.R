boxarob <-function(df, wyb)
{
  if(wyb=="kaczor")
  {
    plot_ly(data = df, x = ~Month, y = ~FullHour, type = "box", opacity = 0.8) %>%
      layout(
        title = list(
          text = "Godziny Oglądania",
          font = list(size = 35, family = "Arial, sans-serif", color = "black", weight = "bold"),
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 1.05
        ),
        xaxis = list(
          title = "Miesiąc",
          titlefont = list(size = 21),
          tickfont = list(size = 16),
          tickangle = 45,
          showgrid = FALSE,
          gridcolor = 'lightgrey',
          autorange = TRUE
        ),
        yaxis = list(
          title = "Godzina",
          titlefont = list(size = 21),
          tickfont = list(size = 15),
          range = c(0, 24),
          dtick = 4,
          showgrid = TRUE,
          gridcolor = 'lightgrey',
          autorange = TRUE
        ),
        colorway = c("#FF6347", "#4682B4"),
        hovermode = 'closest',
        showlegend = FALSE,
        margin = list(
          l = 50,
          r = 50,
          b = 50,
          t = 100,
          pad = 4
        )
      )
  }
  else
  {
    plot_ly(data = df, x = ~WeekDay, y = ~FullHour, type = "box", opacity = 0.8) %>%
      layout(
        title = list(
          text = "Godziny Oglądania",
          font = list(size = 35, family = "Arial, sans-serif", color = "black", weight = "bold"),
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 1.05
        ),
        xaxis = list(
          title = "Dzień Tygodnia",
          titlefont = list(size = 21),
          tickfont = list(size = 16),
          tickangle = 45,
          showgrid = FALSE,
          gridcolor = 'lightgrey',
          autorange = TRUE
        ),
        yaxis = list(
          title = "Godzina",
          titlefont = list(size = 21),
          tickfont = list(size = 15),
          range = c(0, 24),
          dtick = 4,
          showgrid = TRUE,
          gridcolor = 'lightgrey',
          autorange = TRUE
        ),
        colorway = c("#FF6347", "#4682B4"),
        hovermode = 'closest',
        showlegend = FALSE,
        margin = list(
          l = 50,
          r = 50,
          b = 50,
          t = 100,
          pad = 4
        )
      )
  }
}