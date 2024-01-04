library(plotly)
library(dplyr)


df <- read.csv("military-expenditure-share-gdp.csv")

df <- df %>% filter(Entity %in% c("Estonia", "Latvia","Lithuania")) 


plot_ly(
  df,
  x = ~ Year,
  y = ~ military_expenditure_share_gdp,
  color = ~ Entity,
  mode = "lines+markers",
  line = list(shape = "linear"),
  type = "scatter",
  text = paste0(
    "Country: ",
    df$Entity,
    "<br>Year: ",
    df$Year,
    "<br>Share of GDP: ",
    df$military_expenditure_share_gdp,
    "%"
  ),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(text = "Military expenditure share of GDP in Baltic Countries", font = list(size = 30)),
    xaxis = list(
      title = "Year",
      rangeslider = list(
        type = "date",
        visible = TRUE,
        tickvals = unique(df$Year),
        ticktext = as.character(unique(df$Year)),
        ticks = "inside",
        tickmode = "discreete"
      ),
      tickmode = "discreete",
      dtick = 1,
      tickangle = 45
    ),
    yaxis = list(
      title = "Military expenditure share of GDP (%)",
      range = c(0, 2.5),
      dtick = 0.25,
      gridwidth = 2
    ),
    showlegend = TRUE,
    margin = list(
      l = 150,
      r = 150,
      b = 50,
      t = 100,
      pad = 4
    )
  )
