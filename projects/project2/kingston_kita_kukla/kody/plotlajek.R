plotlajek <- function(df, num)
{
  plot_ly(df %>% head(num), x = ~reorder(word, -n), y = ~n, type = 'bar', marker = list(color = 'rgba(50, 171, 96, 0.6)')) %>% 
    layout(
      title = list(
        text = "Słowa ile imbecyle??",
        font = list(size = 35, family = "Arial, sans-serif", color = "black", weight = "bold"),
        xref = "paper",
        yref = "paper",
        x = 0.5,
        y = 1.05
      ),
      xaxis = list(
        title = "Hasło",
        titlefont = list(size = 17),
        tickfont = list(size = 12),
        tickangle = 45,
        showgrid = FALSE,
        gridcolor = 'lightgrey',
        autorange = TRUE
      ),
      yaxis = list(
        title = "Liczba wystąpień",
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